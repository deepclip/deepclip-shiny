library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)

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
  res[1,1]
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
    as.character(query["jobid"])
  })
  output$jobID <- reactive(jobID())
  outputOptions(output, "jobID", suspendWhenHidden=FALSE)
  
  jobStatus <- reactive({
    status <- getJobStatus(jobID())
    if(status == 0) autoInvalidate()
    status
  })
  output$jobStatus <- reactive(jobStatus())
  outputOptions(output, "jobStatus", suspendWhenHidden=FALSE)
  
  output$jobStatusText <- renderText({
    sprintf("Job ID: %s, status: %d", jobID(), jobStatus())
  })
  
  observeEvent(input$trainButton, {
    if(!isTruthy(input$seqFile)) {
      alert("Please select a sequence file and wait for it to upload before submitting.")
      return()
    }
    tmpSeqFile <- copyTempFile(input$seqFile$datapath, input$seqFile$name)
    tmpBkgFile <- if(isTruthy(input$bkgFile)) copyTempFile(input$bkgFile$datapath, input$bkgFile$name)
    
    args <- c(
      paste0(CODE_PATH, "/DeepCLIP.py"),
      "--runmode", "train",
      "--num_epochs", input$epochs,
      "--sequences", tmpSeqFile,
      if(input$bkgSource == "shuffle") "--background_shuffle",
      if(input$bkgSource == "fasta") c("--background_sequences", tmpBkgFile)
    )
    
    jobid <- createJob()
    
    parallel::mcparallel({
      env <- c("OMP_NUM_THREADS=4", "THEANO_FLAGS=openmp=True")
      status <- system2(PYTHON_PATH, args, wait=TRUE, env=env)
      if(status == 0) {
        updateJobStatus(jobid, 1)
      } else {
        updateJobStatus(jobid, 2)
      }
      file.remove(tmpSeqFile, tmpBkgFile)
    }, detached=TRUE)
    session$sendCustomMessage("redirectJob", as.character(jobid))
  })
})
