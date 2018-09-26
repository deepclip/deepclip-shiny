library(RSQLite)
library(DBI)

source("global.R")

db <- dbConnect(SQLite(), SQLITE_PATH)
dbExecute(db, "CREATE TABLE jobs (id INTEGER PRIMARY KEY, token TEXT, status INTEGER);")
dbDisconnect(db)
