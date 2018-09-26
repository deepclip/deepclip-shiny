library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)

source("files.R")

createJob <- function() {
  db <- dbConnect(SQLite(), SQLITE_PATH)
  dbExecute(db, "INSERT INTO jobs (token, status) VALUES ('', 0)")
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
    if(status == 0) autoInvalidate()
    status
  })
  output$jobStatus <- reactive(jobStatus())
  outputOptions(output, "jobStatus", suspendWhenHidden=FALSE)
  
  output$jobStatusText <- renderText({
    sprintf("Job ID: %s, status: %d", jobID(), jobStatus())
  })
  
  output$jobLog <- renderText({
    jobid <- jobID()
    status <- jobStatus()
    if(status == 0) autoInvalidate()
    
    log.path <- getJobLogPath(jobid)
    log.data <- readChar(log.path, file.info(log.path)$size)
    
    error.path <- getJobErrorPath(jobid)
    error.data <- readChar(error.path, file.info(error.path)$size)
    
    paste0(error.data, "\n\n", log.data)
  })
  
  output$testROCPlot <- renderPlot(res=120, {
    jobid <- jobID()
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
      theme_bw()
  })
  
  output$testPFMLogos <- renderPlot(res=100, {
    jobid <- jobID()
    pfm.path <- getPFMPath(jobid)
    
    data <- jsonlite::read_json(pfm.path)
    
    library(ggplot2)
    library(ggseqlogo)
    
    logos <- lapply(data[["logos"]], function(logo) {
      weights <- lapply(logo[["pfm"]], unlist)
      weights <- do.call(rbind, weights)
      rownames(weights) <- c("A","C","G","U")
      t <- sprintf("Filter: %d, %d. Score: %.2f (%.1f%%).", logo[["size"]], logo[["filter"]], logo[["points"]], logo[["points_pct"]]*100)
      ggseqlogo(weights, method="prob") +
        ggtitle(t) +
        theme(
          plot.margin = margin(3,0,3,0, unit="pt"),
          axis.title.x = element_blank()
        )
    })
    
    cowplot::plot_grid(plotlist=logos, ncol=1)
  })
  
  observeEvent(input$trainButton, {
    if(!isTruthy(input$seqFile)) {
      alert("Please select a sequence file and wait for it to upload before submitting.")
      return()
    }
    tmpSeqFile <- copyTempFile(input$seqFile$datapath, input$seqFile$name)
    tmpBkgFile <- if(isTruthy(input$bkgFile)) copyTempFile(input$bkgFile$datapath, input$bkgFile$name)
    
    jobid <- createJob()
  
    predict_fn.path <- getPredictFunctionPath(jobid)
    test_output.path <- getTestOutputPath(jobid)
    predict_pfm.path <- getPFMPath(jobid)
    log_stdout.path <- getJobLogPath(jobid)
    log_stderr.path <- getJobErrorPath(jobid)
    
    args <- c(
      paste0(CODE_PATH, "/DeepCLIP.py"),
      "--runmode", "train",
      "--num_epochs", input$epochs,
      "--sequences", tmpSeqFile,
      "--predict_function_file", predict_fn.path,
      "--test_output_file", test_output.path,
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
        stderr=log_stderr.path,
        env=c("OMP_NUM_THREADS=4", "THEANO_FLAGS=openmp=True")
      )
      
      if(status == 0) updateJobStatus(jobid, 1)
      else updateJobStatus(jobid, 2)
      
      file.remove(tmpSeqFile, tmpBkgFile)
    }, detached=TRUE)
    session$sendCustomMessage("redirectJob", as.character(jobid))
  })
})
