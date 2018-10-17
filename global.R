options(shiny.sanitize.errors = TRUE)

VERSION <- "0.1"

PYTHON_PATH <- "/usr/bin/python2"
PREFIX <- "/srv/deepclip"

SQLITE_PATH <- paste0(PREFIX, "/deepclip.db")
CODE_PATH <- paste0(PREFIX, "/code")
DATA_PATH <- paste0(PREFIX, "/data")
DEEPCLIP_PY_PATH <- paste0(CODE_PATH, "/DeepCLIP.py")
RESULTS_PATH <- paste0(PREFIX, "/results")

JOB_STATUS_ACTIVE  <- 0
JOB_STATUS_SUCCESS <- 1
JOB_STATUS_ERROR   <- 2

JOB_STATUS_NAMES <- c(
  `0` = "active",
  `1` = "success",
  `2` = "error"
)
