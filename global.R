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

REFERENCE_SPECIES <- c(
  "Human (GRCh38/hg38)" = "hg38",
  "Human (GRCh37/hg19)" = "hg19",
  "Mouse (GRCm38/mm10)" = "mm10",
  "Mouse (NCBI37/mm9"   = "mm9"
)

PRETRAINED_MODELS <- read.table(header=TRUE, sep=',', quote='"', text='
"value","label","protein","method","citation"
"","Select or search for a model.","","",""
"AGO1-4","AGO1-4 PAR-CLIP (Hafner et al. 2010)","AGO1-4","PAR-CLIP","Hafner et al. 2010"
"AGO2","AGO2 HITS-CLIP (Kishore et al. 2011)","AGO2","HITS-CLIP","Kishore et al. 2011"
"ALKBH5","ALKBH5 PAR-CLIP (Baltz et al. 2012)","ALKBH5","PAR-CLIP","Baltz et al. 2012"
"C17ORF85","C17ORF85 PAR-CLIP (Baltz et al. 2012)","C17ORF85","PAR-CLIP","Baltz et al. 2012"
"C22ORF28","C22ORF28 PAR-CLIP (Baltz et al. 2012)","C22ORF28","PAR-CLIP","Baltz et al. 2012"
"CAPRIN1","CAPRIN1 PAR-CLIP (Baltz et al. 2012)","CAPRIN1","PAR-CLIP","Baltz et al. 2012"
"ELAVL1","ELAVL1 HITS-CLIP (Kishore et al. 2011)","ELAVL1","HITS-CLIP","Kishore et al. 2011"
"ELAVL1A","ELAVL1(A) PAR-CLIP (Kishore et al. 2011)","ELAVL1(A)","PAR-CLIP","Kishore et al. 2011"
"ELAVL1B","ELAVL1(B) PAR-CLIP (Lebedeva et al. 2011)","ELAVL1(B)","PAR-CLIP","Lebedeva et al. 2011"
"EWSR1","EWSR1 PAR-CLIP (Hoell et al. 2011)","EWSR1","PAR-CLIP","Hoell et al. 2011"
"FUS","FUS PAR-CLIP (Hoell et al. 2011)","FUS","PAR-CLIP","Hoell et al. 2011"
"hnRNPA1","hnRNPA1 iCLIP (Bruun et al. 2016)","hnRNPA1","iCLIP","Bruun et al. 2016"
"hnRNPC","hnRNPC iCLIP (König et al. 2010)","hnRNPC","iCLIP","König et al. 2010"
"HuR","HuR PAR-CLIP (Mukherjee et al. 2011)","HuR","PAR-CLIP","Mukherjee et al. 2011"
"IGF2BP1-3","IGF2BP1-3 PAR-CLIP (Hafner et al. 2010)","IGF2BP1-3","PAR-CLIP","Hafner et al. 2010"
"MOV10","MOV10 PAR-CLIP (Sievers et al. 2012)","MOV10","PAR-CLIP","Sievers et al. 2012"
"PTBP1","PTBP1 HITS-CLIP (Xue et al. 2009)","PTBP1","HITS-CLIP","Xue et al. 2009"
"PUM2","PUM2 PAR-CLIP (Hafner et al. 2010)","PUM2","PAR-CLIP","Hafner et al. 2010"
"QKI","QKI PAR-CLIP (Hafner et al. 2010)","QKI","PAR-CLIP","Hafner et al. 2010"
"SRSF1","SRSF1 HITS-CLIP (Sanford et al. 2009)","SRSF1","HITS-CLIP","Sanford et al. 2009"
"TAF15","TAF15 PAR-CLIP (Hoell et al. 2011)","TAF15","PAR-CLIP","Hoell et al. 2011"
"TDP43","TDP43 iCLIP (Tollervey et al. 2011)","TDP43","iCLIP","Tollervey et al. 2011"
"TIA1","TIA1 iCLIP (Wang et al. 2010)","TIA1","iCLIP","Wang et al. 2010"
"TIAL1","TIAL1 iCLIP (Wang et al. 2010)","TIAL1","iCLIP","Wang et al. 2010"
"ZC3H7B","ZC3H7B PAR-CLIP (Baltz et al. 2012)","ZC3H7B","PAR-CLIP","Baltz et al. 2012"')
