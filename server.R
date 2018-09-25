library(shiny)
library(DBI)
library(RSQLite)

createJob <- function() {
  db <- dbConnect(SQLite(), SQLITE_PATH)
  dbExecute(db, "INSERT INTO jobs (status) VALUES (0)")
  jobid <- as.numeric(dbGetQuery(db, "SELECT last_insert_rowid()"))
  dbDisconnect(db)
  jobid
}

updateJobStatus <- function(id, status) {
  db <- dbConnect(SQLite(), SQLITE_PATH)
  query <- sqlInterpolate(db, "UPDATE jobs SET status = ?status WHERE id = ?id;", id=id, status=status)
  dbExecute(db, query)
  dbDisconnect(db)
}

getJobStatus <- function(id) {
  db <- dbConnect(SQLite(), SQLITE_PATH)
  query <- sqlInterpolate(db, "SELECT status FROM jobs WHERE id = ?id;", id=id)
  res <- dbGetQuery(db, query)
  dbDisconnect(db)
  res[1,1]
}

shinyServer(function(input, output, session) {
  jobID <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    query["jobid"]
  })
  output$jobID <- reactive(jobID())
  outputOptions(output, "jobID", suspendWhenHidden=FALSE)
  
  jobStatus <- reactive({
    getJobStatus(jobID())
  })
  output$jobStatus <- reactive(jobStatus())
  outputOptions(output, "jobStatus", suspendWhenHidden=FALSE)
  
  output$jobStatusText <- renderText({
    sprintf("Job status: %d", jobStatus())
  })
  
  observeEvent(input$trainButton, {
    jobid <- createJob()
    parallel::mcparallel({
      Sys.sleep(10)
      updateJobStatus(jobid, 1)
    }, detached=TRUE)
    session$sendCustomMessage("redirectJob", as.character(jobid))
  })
})
