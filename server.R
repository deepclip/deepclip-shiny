library(shiny)
library(shinyjs)
library(DT)
library(DBI)
library(RSQLite)

source("theme.R")
source("files.R")

createJob <- function() {
  db <- dbConnect(SQLite(), SQLITE_PATH)
  
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
  db <- dbConnect(SQLite(), SQLITE_PATH)
  query <- sqlInterpolate(db, "UPDATE jobs SET status = ?status WHERE token = ?token ;", token=token, status=status)
  dbExecute(db, query)
  dbDisconnect(db)
}

getJobStatus <- function(token) {
  db <- dbConnect(SQLite(), SQLITE_PATH)
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
  
  output$jobStatusText <- renderText({
    sprintf("Job ID: %s, status: %d (%s)", jobID(), jobStatus(), JOB_STATUS_NAMES[as.character(jobStatus())])
  })
  
  output$jobLog <- renderText({
    jobid <- jobID()
    status <- jobStatus()
    if(status == JOB_STATUS_ACTIVE) autoInvalidate()
    
    log.path <- getJobLogPath(jobid)
    log.data <- readChar(log.path, file.info(log.path)$size)
    
    error.path <- getJobErrorPath(jobid)
    error.data <- readChar(error.path, file.info(error.path)$size)
    
    paste0(error.data, "\n\n", log.data)
  })
  
  output$testROCPlot <- renderPlot(res=120, {
    jobid <- req(jobID())
    validate(need(jobStatus() == JOB_STATUS_SUCCESS, "Job not completed"))
    test.path <- getTestOutputPath(jobid)
    
    data <- jsonlite::read_json(test.path)
    roc <- data.frame(
      fpr = sapply(data$data, "[[", 1),
      tpr = sapply(data$data, "[[", 2)
    )
    
    library(ggplot2)
    ggplot(roc, aes(fpr, tpr)) +
      geom_step() +
      labs(x="False positive rate", y="True positive rate") +
      geom_abline(intercept=0, slope=1, linetype="dashed", color="#404040") +
      mytheme()
  })
  
  output$testPFMLogos <- renderPlot(res=100, {
    jobid <- req(jobID())
    validate(need(jobStatus() == JOB_STATUS_SUCCESS, "Job not completed"))
    pfm.path <- getPFMPath(jobid)
    
    data <- jsonlite::read_json(pfm.path)
    
    library(ggplot2)
    library(ggseqlogo)
    
    scores <- sapply(data[["logos"]], function(logo) {
      pfm <- do.call(rbind, lapply(logo[["pfm"]], unlist))
      sum(colSums(pfm * log2(pfm + 0.000000001)) + log2(4)) / ncol(pfm)
    })
    logos <- lapply(data[["logos"]], function(logo) {
      weights <- lapply(logo[["pfm"]], unlist)
      weights <- do.call(rbind, weights)
      rownames(weights) <- c("A","C","G","U")
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
  })
  
  output$testPredDistPlot <- renderPlot(res=100, {
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
      labs(x="Score", y="Density", fill="")
  })
  
  output$predictionProfilePlot <- renderPlot(res=100, {
    validate(need(input$predictionTable_rows_selected, "Select a row in the table below to show the binding profile."))
    rowid <- req(input$predictionTable_rows_selected)
    preds <- req(currentPredictions())
    x <- preds[[rowid]]
    
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
      
      tbl <- data.frame(
        pos = c(seq_along(seq1), seq_along(seq2)),
        weight = c(weights1, weights2),
        group = factor(c(rep("reference", length(seq1)), rep("variant", length(seq2))), levels=c("reference","variant"))
      )
      
      xlabels <- mapply(function(a, b) paste(a, ifelse(a==b, "", b), sep="\n"), seq1, seq2)
      
      ggplot(tbl, aes(pos, weight)) +
        geom_line(aes(color=group)) +
        scale_x_continuous(breaks=seq(1, max(tbl$pos)), labels=xlabels) +
        scale_color_manual(values=c("black", "red")) +
        mytheme() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=11)
        ) + labs(y="DeepCLIP score")
    }
  })
  
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
      options = list(pageLength = 10, select="single")
    )
  })
  
  observeEvent(input$usePretrainedButton, {
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
    
    tmpSeqFile <- copyTempFile(input$seqFile$datapath, input$seqFile$name)
    tmpBkgFile <- if(isTruthy(input$bkgFile)) copyTempFile(input$bkgFile$datapath, input$bkgFile$name)
    
    jobid <- createJob()
  
    predict_fn.path <- getPredictFunctionPath(jobid)
    test_output.path <- getTestOutputPath(jobid)
    predict_pfm.path <- getPFMPath(jobid)
    test_pred.path <- getTestPredictionsPath(jobid)
    log_stdout.path <- getJobLogPath(jobid)
    log_stderr.path <- getJobErrorPath(jobid)
    
    args <- c(
      paste0(CODE_PATH, "/DeepCLIP.py"),
      "--runmode", "train",
      "--num_epochs", input$epochs,
      "--sequences", tmpSeqFile,
      "--predict_function_file", predict_fn.path,
      "--test_output_file", test_output.path,
      "--test_predictions_file", test_pred.path,
      "--predict_PFM_file", predict_pfm.path,
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
    session$sendCustomMessage("redirectJob", as.character(jobid))
  })
  
  observeEvent(input$predictButton, {
    if(!isTruthy(input$predictSeq) && input$predictSeqText == "") {
      alert("Please provide a sequence file or paste your sequences in the text area.")
      return()
    }
    
    withProgress({
      setProgress(value=0.1, message="Preparing data")
      
      seqfile1 <- tempfile(fileext=".fa")
      seqfile2 <- tempfile(fileext=".fa")
      if(input$predictSeqText != "") {
        write(input$predictSeqText, file=seqfile1)
      } else {
        file.copy(input$predictSeq$datapath, seqfile1)
      }
      has_variants <- FALSE
      if(input$predictSeqText2 != "") {
        write(input$predictSeqText2, file=seqfile2)
        has_variants <- TRUE
      }
      else if(isTruthy(input$predictSeq2)) {
        file.copy(input$predictSeq2$datapath, seqfile2)
        has_variants <- TRUE
      }
      
      jobid <- jobID()
      output.path <- tempfile(fileext=".json")
      predict_fn.path <- getPredictFunctionPath(jobid)
      
      args <- c(
        paste0(CODE_PATH, "/DeepCLIP.py"),
        "--runmode", "predict",
        "--predict_function_file", predict_fn.path,
        "--sequences", seqfile1,
        if(has_variants) c("--variant_sequences", seqfile2),
        "--predict_mode", "single",
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
  
  hide(id="loading-content", anim=TRUE, animType="fade")
})
