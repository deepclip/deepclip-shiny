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

PRETRAINED_MODELS <- c(
  "AGO1-4 PAR-CLIP (Hafner et al. 2010)" = "AGO1-4",
  "AGO2 HITS-CLIP (Kishore et al. 2011)" = "AGO2",
  "ALKBH5 PAR-CLIP (Baltz et al. 2012)" = "ALKBH5",
  "C17ORF85 PAR-CLIP (Baltz et al. 2012)" = "C17ORF85",
  "C22ORF28 PAR-CLIP (Baltz et al. 2012)" = "C22ORF28",
  "CAPRIN1 PAR-CLIP (Baltz et al. 2012)" = "CAPRIN1",
  "ELAVL1 HITS-CLIP (Kishore et al. 2011)" = "ELAVL1",
  "ELAVL1(A) PAR-CLIP (Kishore et al. 2011)" = "ELAVL1A",
  "ELAVL1(B) PAR-CLIP (Lebedeva et al. 2011)" = "ELAVL1B",
  "EWSR1 PAR-CLIP (Hoell et al. 2011)" = "EWSR1",
  "FUS PAR-CLIP (Hoell et al. 2011)" = "FUS",
  "hnRNPC iCLIP (König et al. 2010)" = "hnRNPC",
  "HuR PAR-CLIP (Mukherjee et al. 2011)" = "HuR",
  "IGF2BP1-3 PAR-CLIP (Hafner et al. 2010)" = "IGF2BP1-3",
  "MOV10 PAR-CLIP (Sievers et al. 2012)" = "MOV10",
  "PTBP1 HITS-CLIP (Xue et al. 2009)" = "PTBP1",
  "PUM2 PAR-CLIP (Hafner et al. 2010)" = "PUM2",
  "QKI PAR-CLIP (Hafner et al. 2010)" = "QKI",
  "SRSF1 HITS-CLIP (Sanford et al. 2009)" = "SRSF1",
  "TAF15 PAR-CLIP (Hoell et al. 2011)" = "TAF15",
  "TDP43 iCLIP (Tollervey et al. 2011)" = "TDP43",
  "TIA1 iCLIP (Wang et al. 2010)" = "TIA1",
  "TIAL1 iCLIP (Wang et al. 2010)" = "TIAL1",
  "ZC3H7B PAR-CLIP (Baltz et al. 2012)" = "ZC3H7B"
)