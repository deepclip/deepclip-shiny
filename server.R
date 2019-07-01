library(shiny)
library(shinyjs)
library(DT)
library(DBI)

source("theme.R")
source("files.R")

createJob <- function() {
  db <- dbConnect(RSQLite::SQLite(), SQLITE_PATH)
  
  query <- sqlInterpolate(db, "INSERT INTO jobs (token, status) VALUES ('', ?status)", status=JOB_STATUS_ACTIVE)
  dbExecute(db, query)
  
  jobid <- as.numeric(dbGetQuery(db, "SELECT last_insert_rowid()"))
  token <- paste0(jobid, paste0(sample(c(letters, 0:9), 32), collapse=""))
  query <- sqlInterpolate(db, "UPDATE jobs SET token = ?token WHERE id = ?id ;", token=token, id=jobid)
  dbExecute(db, query)
  
  dbDisconnect(db)
  token
}

updateJobStatus <- function(token, status) {
  db <- dbConnect(RSQLite::SQLite(), SQLITE_PATH)
  query <- sqlInterpolate(db, "UPDATE jobs SET status = ?status WHERE token = ?token ;", token=token, status=status)
  dbExecute(db, query)
  dbDisconnect(db)
}

getJobStatus <- function(token) {
  db <- dbConnect(RSQLite::SQLite(), SQLITE_PATH)
  query <- sqlInterpolate(db, "SELECT status FROM jobs WHERE token = ?token ;", token=token)
  res <- dbGetQuery(db, query)
  dbDisconnect(db)
  ifelse(nrow(res) > 0, res[1,1], NA)
}

copyTempFile <- function(path, name) {
  parts <- strsplit(name, "\\.")[[1]]
  ext <- parts[length(parts)]
  tmpfile <- tempfile(fileext=paste0(".", ext))
  file.copy(path, tmpfile)
  tmpfile
}

checkValidFasta <- function(text) {
  if(!startsWith(text, ">")) return(FALSE)
  first_nl <- regexpr("\n", text)[1]
  if(first_nl == -1) return(FALSE)
  if(!tolower(substring(text, first_nl+1, first_nl+1)) %in% c("a","c","g","t","u")) return(FALSE)
  return(TRUE)
}

makePredictionProfilePlot <- function(x, plot_difference) {
  if(is.null(x[["variant_sequence"]])) {
    weights <- unlist(x$weights)
    seq <- strsplit(toupper(x$sequence), "")[[1]]
    tbl <- data.frame(pos=seq_along(weights), weight=weights)
    
    ggplot(tbl, aes(pos, weight)) +
      geom_line() +
      scale_x_continuous(breaks=seq(1, length(weights)), labels=seq) +
      mytheme() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=11)
      ) + labs(y="DeepCLIP score")
  } else {
    weights1 <- unlist(x$weights)
    weights2 <- unlist(x$variant_weights)
    
    seq1 <- strsplit(toupper(x$sequence), "")[[1]]
    seq2 <- strsplit(toupper(x$variant_sequence), "")[[1]]
    
    if(plot_difference) {
      weights2 <- weights2 - weights1
      tbl <- data.frame(
        pos = seq_along(seq2),
        weight = weights2,
        group = factor(rep("difference", length(seq2)))
      )
    } else {
      tbl <- data.frame(
        pos = c(seq_along(seq1), seq_along(seq2)),
        weight = c(weights1, weights2),
        group = factor(c(rep("reference", length(seq1)), rep("variant", length(seq2))), levels=c("reference","variant"))
      )
    }
    
    xlabels <- mapply(function(a, b) paste(a, ifelse(a==b, "", b), sep="\n"), seq1, seq2)
    
    p <- ggplot(tbl, aes(pos, weight))
    if(plot_difference) p <- p + geom_hline(yintercept=0, color="dodgerblue")
    p +
      geom_line(aes(color=group), size=0.8) +
      scale_x_continuous(breaks=seq(1, max(tbl$pos)), labels=xlabels) +
      scale_color_manual(values=c("black", "red")) +
      mytheme() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=11)
      ) + labs(y="DeepCLIP score")
  }
}

shinyServer(function(input, output, session) {
  currentPredictions <- reactiveVal()
  
  autoInvalidate <- reactiveTimer(10000)
  
  jobID <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    req(query[["jobid"]])
    as.character(query[["jobid"]])
  })
  output$jobID <- reactive(jobID())
  outputOptions(output, "jobID", suspendWhenHidden=FALSE)
  
  jobStatus <- reactive({
    status <- req(getJobStatus(jobID()))
    if(status == JOB_STATUS_ACTIVE) autoInvalidate()
    status
  })
  output$jobStatus <- reactive(jobStatus())
  outputOptions(output, "jobStatus", suspendWhenHidden=FALSE)
  
  output$hasPredictions <- reactive({
    req(currentPredictions())
    return(TRUE)
  })
  outputOptions(output, "hasPredictions", suspendWhenHidden=FALSE)
  
  updateSelectizeInput(session, "pretrainedModel", choices=PRETRAINED_MODELS, server=TRUE,
    options=list(placeholder="Select or search for a model.", render=I(
    '{
      option: function(item, escape) {
        console.log(item.protein.length);
        if(item.protein.length > 0) {
          return "<div><strong>" + escape(item.protein) + ", " + escape(item.method) + "</strong><br>" + escape(item.citation) + "</div>";
        } else {
          return "<div class=text-muted>" + escape(item.label) + "</div>";
        }
      }
    }'
    )
  ))
  
  output$summaryText <- renderUI({
    params <- jsonlite::read_json(getParamsPath(jobID()))
    tagList(
      if(length(params$protein) > 0) tagList(strong("Protein: "), params$protein, br()),
      if(length(params$citation) > 0) tagList(strong("Citation: "), params$citation, br()),
      if(length(params$clip_method) > 0) tagList(strong("CLIP method: "), params$clip_method, br()),
      if(length(params$num_peaks) > 0) tagList(strong("Number of peaks: "), params$num_peaks, br()),
      if(length(params$random_seed) > 0) tagList(strong("Random seed: "), params$random_seed, br()),
      strong("Num. epochs: "), params$epochs, br(),
      if(!is.null(params$early_stopping)) tagList(strong("Early stopping: "), params$early_stopping, br()),
      strong("Data split: "), tags$ul(lapply(paste0(c("Training: ", "Validation: ", "Testing: "), params$data_split, "%"), tags$li))
    )
  })
  
  output$jobLog <- renderText({
    jobid <- jobID()
    status <- jobStatus()
    if(status == JOB_STATUS_ACTIVE) autoInvalidate()
    if(status != JOB_STATUS_ERROR) return("")
    
    log.path <- getJobLogPath(jobid)
    log.data <- readChar(log.path, file.info(log.path)$size)
    
    error.path <- getJobErrorPath(jobid)
    error.data <- readChar(error.path, file.info(error.path)$size)
    
    paste0(error.data, "\n\n", log.data)
  })
  
  testROCPlot <- function() {
    jobid <- req(jobID())
    validate(need(jobStatus() == JOB_STATUS_SUCCESS, "Job not completed"))
    
    data <- jsonlite::read_json(getTestOutputPath(jobid))
    roc <- data.frame(
      fpr = sapply(data$data, "[[", 1),
      tpr = sapply(data$data, "[[", 2)
    )
    
    library(ggplot2)
    ggplot(roc, aes(fpr, tpr)) +
      geom_step() +
      labs(x="False positive rate", y="True positive rate") +
      geom_abline(intercept=0, slope=1, linetype="dashed", color="#404040") +
      ggtitle(sprintf("AUROC: %.4f", data$auroc, digits=4)) +
      mytheme()
  }
  
  output$jobProgressBar <- renderUI({
    jobid <- jobID()
    status <- jobStatus()
    if(status != JOB_STATUS_ACTIVE) return()
    autoInvalidate()
    
    params <- jsonlite::read_json(getParamsPath(jobid))
    total_epochs <- params$epochs
    
    log.lines <- suppressWarnings(readLines(getJobLogPath(jobid), -1))
    status.lines <- which(grepl("\\s+Epoch [0-9]+ of [0-9]+", log.lines))
    
    current_epoch <- 0
    if(length(status.lines) > 0) {
      last.line <- log.lines[status.lines[length(status.lines)]]
      parts <- strsplit(trimws(last.line), " ")[[1]]
      current_epoch <- as.numeric(parts[2])
    }
    
    pct <- formatC(round((current_epoch+1) / (total_epochs+2) * 100))
    if(current_epoch == 0) {
      status_text <- "Compiling"
    } else {
      status_text <- sprintf("Epoch %d of %d", current_epoch, total_epochs)
    }
    HTML(sprintf('<div class="progress"><div class="progress-bar progress-bar-striped active" style="width: %s%%;">%s</div></div>', pct, status_text))
  })

  output$testROCPlot <- renderPlot(res=120, testROCPlot())
  
  testPFMLogos <- function() {
    jobid <- req(jobID())
    validate(need(jobStatus() == JOB_STATUS_SUCCESS, "Job not completed"))
    
    data <- jsonlite::read_json(getPFMPath(jobid))
    params <- jsonlite::read_json(getParamsPath(jobid))
    
    library(ggplot2)
    library(ggseqlogo)
    
    scores <- sapply(data[["logos"]], function(logo) {
      pfm <- do.call(rbind, lapply(logo[["pfm"]], unlist))
      sum(colSums(pfm * log2(pfm + 0.000000001)) + log2(4)) / ncol(pfm)
    })
    seq_letters <- c("A", "C", "G", ifelse(params$seq_type == "DNA", "T", "U"))
    logos <- lapply(data[["logos"]], function(logo) {
      weights <- lapply(logo[["pfm"]], unlist)
      weights <- do.call(rbind, weights)
      rownames(weights) <- seq_letters
      weights
    })
    names(logos) <- formatC(scores)
    logos <- logos[order(scores, decreasing=TRUE)]
    
    ggplot() +
      geom_logo(logos, method="prob") +
      facet_grid(seq_group ~ ., switch="y") +
      theme_logo() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        strip.text.y = element_text(angle=180, size=12)
      )
  }
  
  output$testPFMLogos <- renderPlot(res=100, testPFMLogos())
  
  testPredDistPlot <- function() {
    jobid <- req(jobID())
    validate(need(jobStatus() == JOB_STATUS_SUCCESS, "Job not completed"))
    test_pred.path <- getTestPredictionsPath(jobid)
    
    preds <- read.table(test_pred.path, header=TRUE, sep="\t")
    preds$class <- ifelse(preds$class == 0, "background", "bound")
    
    library(ggplot2)
    ggplot(preds, aes(score)) +
      geom_density(aes(fill=class), alpha=0.5) +
      scale_x_continuous(limits=c(0,1)) +
      mytheme() +
      theme(legend.position = "right") +
      labs(x="Score", y="Density", fill="")
  }
  
  output$testPredDistPlot <- renderPlot(res=100, testPredDistPlot())
  
  output$downloadSummaryPlots <- downloadHandler(
    filename = function() { sprintf("%s.zip", jobID()) },
    contentType = "application/zip",
    content = function(file) {
      p.roc <- testROCPlot()
      p.pfm <- testPFMLogos()
      p.dist <- testPredDistPlot()
      
      file.roc <- tempfile(pattern="roc_", fileext=".pdf")
      file.pfm <- tempfile(pattern="pfm_", fileext=".pdf")
      file.dist <- tempfile(pattern="scoredist_", fileext=".pdf")
      
      ggsave(file.roc, p.roc, width=3.7, height=3.7, units="in")
      ggsave(file.pfm, p.pfm, width=6, height=5, units="in")
      ggsave(file.dist, p.dist, width=11.4, height=4, units="in")
      
      zip(file, c(file.roc, file.pfm, file.dist), extras="-j")
      rm(file.roc, file.pfm, file.dist)
    }
  )
  
  currentPredictionProfilePlot <- function() {
    validate(need(input$predictionTable_rows_selected, "Select a row in the table below to show the binding profile."))
    rowid <- req(input$predictionTable_rows_selected)
    preds <- req(currentPredictions())
    x <- preds[[rowid]]
    makePredictionProfilePlot(x, input$profilePlotDifference)
  }
  
  output$predictionProfilePlot <- renderPlot(res=100, currentPredictionProfilePlot())
  
  output$downloadAllPredictionProfilePlots <- downloadHandler(
    filename = function() { sprintf("%s_profiles.zip", jobID()) },
    contentType = "application/zip",
    content = function(file) {
      preds <- req(currentPredictions())
      withProgress(message="Generating profile plots", min=0, max=length(preds), value=0, {
        outfiles <- sapply(seq_along(preds), function(i) {
          x <- preds[[i]]
          p <- makePredictionProfilePlot(x, input$profilePlotDifference)
          if(length(x$variant_id) > 0) {
            outfile <- tempfile(pattern=sprintf("profile_%d_%s_%s_", i, x$id, x$variant_id), fileext=".pdf")
          } else {
            outfile <- tempfile(pattern=sprintf("profile_%d_%s_", i, x$id), fileext=".pdf")
          }
          ggsave(outfile, p, width=10, height=3, units="in")
          setProgress(value=i)
          outfile
        })
      })
      zip(file, outfiles, extras="-j")
      rm(list=outfiles)
    }
  )
  
  output$downloadPredictionData <- downloadHandler(
    filename = function() { sprintf("%s_predictions.json", jobID()) },
    contentType = "application/json",
    content = function(file) {
      preds <- req(currentPredictions())
      jsonlite::write_json(preds, file, auto_unbox=TRUE)
    }
  )
  
  output$downloadPositiveSequences <- downloadHandler(
    filename = "pos.fa",
    contentType = "text/x-fasta",
    content = function(file) {
      file.copy(getPositiveSequencePath(jobID()), file)
    }
  )
  
  output$downloadNegativeSequences <- downloadHandler(
    filename = "neg.fa",
    contentType = "text/x-fasta",
    content = function(file) {
      file.copy(getNegativeSequencePath(jobID()), file)
    }
  )
  
  output$predictionTable <- renderDT(server=FALSE, {
    preds <- req(currentPredictions())
    tbl <- data.frame(
      id = sapply(preds, "[[", "id"),
      seq = sapply(preds, "[[", "sequence"),
      score = sapply(preds, "[[", "score")
    )
    if(!is.null(preds[[1]][["variant_sequence"]])) {
      tbl <- cbind(tbl, data.frame(
        variant_id = sapply(preds, "[[", "variant_id"),
        variant_seq = sapply(preds, "[[", "variant_sequence"),
        variant_score = sapply(preds, "[[", "variant_score")
       ))
    }
    datatable(
      req(tbl),
      rownames = FALSE,
      selection = list(mode="single", selected=1),
      extensions = "Buttons",
      options = list(
        pageLength=10,
        select="single",
        dom="Bfrtip",
        buttons=list(
          list(extend="csv", text="Download CSV", filename="predictions"),
          list(extend="excel", text="Download Excel", filename="predictions", title="predictions")
        )
      )
    )
  })
  
  observeEvent(input$usePretrainedButton, {
    if(input$pretrainedModel == "") {
      alert("Please select a model from the menu.")
      return()
    }
    session$sendCustomMessage("redirectJob", input$pretrainedModel)
  })
  
  observeEvent(input$trainButton, {
    if(!isTruthy(input$seqFile)) {
      alert("Please select a sequence file and wait for it to upload before submitting.")
      return()
    }
    if(input$bkgSource == "fasta" && !isTruthy(input$bkgFile)) {
      alert("Please select a FASTA file containing background sequences and wait for it to upload before submitting.")
      return()
    }
    if(input$bkgSource == "bed" && input$seqFormat != "bed") {
      alert("Generating background sequences from BED file is only possible when binding sequences are in BED format.")
      return()
    }
    
    data_split <- c(input$trainSplit, input$valSplit, input$testSplit)
    if(any(is.na(data_split))) {
      alert("Please specify the fraction of sequences to use for training, validation and testing.")
      return()
    }
    if(any(data_split <= 0)) {
      alert("All data split sizes must be greater than 0.")
      return()
    }
    if(sum(data_split) != 100) {
      alert("Data split sizes must sum to 100%.")
      return()
    }
    random_seed <- NULL
    if(input$randomSeed != "") {
      random_seed <- as.integer(input$randomSeed)
      if(is.na(random_seed)) {
        alert("Provided random seed is not a valid integer.")
        return()
      }
    }
    
    tmpSeqFile <- copyTempFile(input$seqFile$datapath, input$seqFile$name)
    tmpBkgFile <- if(isTruthy(input$bkgFile)) copyTempFile(input$bkgFile$datapath, input$bkgFile$name)
    
    jobid <- createJob()
  
    predict_fn.path <- getPredictFunctionPath(jobid)
    test_output.path <- getTestOutputPath(jobid)
    predict_pfm.path <- getPFMPath(jobid)
    test_pred.path <- getTestPredictionsPath(jobid)
    log_stdout.path <- getJobLogPath(jobid)
    log_stderr.path <- getJobErrorPath(jobid)
    out_seq.path <- getPositiveSequencePath(jobid)
    out_bkg.path <- getNegativeSequencePath(jobid)
    
    args <- c(
      paste0(CODE_PATH, "/DeepCLIP.py"),
      "--runmode", "train",
      "--num_epochs", input$epochs,
      "--sequences", tmpSeqFile,
      "--write_sequences", out_seq.path,
      "--write_background_sequences", out_bkg.path,
      "--predict_function_file", predict_fn.path,
      "--test_output_file", test_output.path,
      "--test_predictions_file", test_pred.path,
      "--predict_PFM_file", predict_pfm.path,
      "--data_split", data_split / 100,
      if(isTruthy(random_seed)) c("--random_seed", random_seed),
      if(isTruthy(input$early_stopping)) c("--early_stopping", input$early_stopping),
      if(input$seqFormat == "bed") {
        c(
          "--force_bed",
          "--genome_file", getGenomeFile(input$seqAssembly),
          "--gtf_file", getGTFFile(input$seqAssembly)
        )
      },
      if(input$bkgSource == "shuffle") "--background_shuffle",
      if(input$bkgSource == "fasta") c("--background_sequences", tmpBkgFile),
      "--min_length", input$minLength,
      "--max_length", input$maxLength,
      if(isTruthy(input$bedWidth)) c("--bed_width", input$bedWidth),
      if(isTruthy(input$bedPadding)) c("--bed_padding", input$bedPadding)
    )
      
    parallel::mcparallel({
      status <- system2(
        PYTHON_PATH, args,
        wait=TRUE,
        stdout=log_stdout.path,
        stderr=log_stderr.path
      )
      
      if(status == 0) updateJobStatus(jobid, JOB_STATUS_SUCCESS)
      else updateJobStatus(jobid, JOB_STATUS_ERROR)
      
      file.remove(tmpSeqFile, tmpBkgFile)
    }, detached=TRUE)
    
    params <- list(
      seq_type = "RNA", seq_format = input$seqFormat, seq_assembly = input$seqAssembly,
      epochs = input$epochs, early_stopping = input$early_stopping,
      data_split = data_split, random_seed = random_seed,
      min_length = input$minLength, max_length = input$maxLength,
      bed_width = input$bedWidth, bed_padding = input$bedPadding
    )
    params.path <- getParamsPath(jobid)
    param_json = jsonlite::write_json(params, params.path, auto_unbox=TRUE)
    
    session$sendCustomMessage("redirectJob", as.character(jobid))
  })
  
  observeEvent(input$predictButton, {
    if(!isTruthy(input$predictSeq) && input$predictSeqText == "") {
      alert("Please provide a sequence file or paste your sequences in the text area.")
      return()
    }
    if(input$predictPaired && !isTruthy(input$predictSeq2) && input$predictSeqText2 == "") {
      alert("Please provide a variant sequence file or paste your variant sequences in the text area.")
      return()
    }
    
    withProgress({
      setProgress(value=0.1, message="Preparing data")
      
      seqfile1 <- tempfile(fileext=".fa")
      seqfile2 <- tempfile(fileext=".fa")
      if(input$predictSeqText != "") {
        if(!checkValidFasta(input$predictSeqText)) {
          addClass("predictSeqPanel", "has-error")
          alert("Sequences provided in left text area is not in valid FASTA format.")
          return()
        }
        write(input$predictSeqText, file=seqfile1)
      } else {
        file.copy(input$predictSeq$datapath, seqfile1)
      }
      if(input$predictPaired) {
        if(input$predictSeqText2 != "") {
          if(!checkValidFasta(input$predictSeqText2)) {
            alert("Sequences provided in right text area is not valid FASTA format.")
            return()
          }
          write(input$predictSeqText2, file=seqfile2)
        }
        else if(isTruthy(input$predictSeq2)) {
          file.copy(input$predictSeq2$datapath, seqfile2)
        }
      }
      
      jobid <- jobID()
      output.path <- tempfile(fileext=".json")
      predict_fn.path <- getPredictFunctionPath(jobid)
      
      args <- c(
        paste0(CODE_PATH, "/DeepCLIP.py"),
        "--runmode", "predict",
        "--predict_function_file", predict_fn.path,
        "--sequences", seqfile1,
        if(input$predictPaired) c("--variant_sequences", seqfile2),
        "--predict_output_file", output.path
      )
    
      setProgress(value=0.2, message="Computing predictions")
      status <- system2(PYTHON_PATH, args, wait=TRUE)
    
      setProgress(value=0.9, message="Finishing up")
      file.remove(seqfile1, seqfile2)
      
      if(status != 0) {
        alert("Prediction failed.")
        return()
      }
      
      data <- jsonlite::read_json(output.path)
      
      currentPredictions(data$predictions)
    })
  })
  
  session$onFlushed(function() {
    hide(id="loading-content", anim=TRUE, animType="fade")
  })
})
