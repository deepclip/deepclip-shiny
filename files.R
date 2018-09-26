getPredictFunctionPath <- function(jobid) {
  paste0(RESULTS_PATH, "/predict_fn_", jobid, ".pkl")
}

getTestOutputPath <- function(jobid) {
  paste0(RESULTS_PATH, "/test_", jobid, ".json")
}

getPFMPath <- function(jobid) {
  paste0(RESULTS_PATH, "/pfm_", jobid, ".json")
}

getJobLogPath <- function(jobid) {
  paste0(RESULTS_PATH, "/log_stdout_", jobid, ".txt")
}

getJobErrorPath <- function(jobid) {
  paste0(RESULTS_PATH, "/log_stderr_", jobid, ".txt")
}

getGenomeFile <- function(id) {
  paste0(DATA_PATH, "/", id, ".fa")
}

getGTFFile <- function(id) {
  paste0(DATA_PATH, "/", id, ".gtf")
}
