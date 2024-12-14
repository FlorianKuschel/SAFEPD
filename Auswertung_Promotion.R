# Run analyses for a corss-sectional study on perceived safety of people with Parkinson's Disease (PwPD):
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 2.2 # 2024-26-11, # Smaller adjustments to make the code readble for different collaborators
# Version 2.3 # 2024-01-12, # updated code with suggestions, added one comment/question in load_data
# Version 2.4 # 2024-04-12, #

# ==== Preamble
# Set respective folders according to username to facilitate collaboration
username <- Sys.getenv("USERNAME")

# Define working and data directories
wdir <- ifelse(
  username == "dpedrosac",
  "/media/storage/SAFEPD/",
  "~/SAFEPD/" # Collaborators need to ensure this directory exists
)

# Create the results folder if it doesn't exist
if (!dir.exists(file.path(wdir, "results"))) {
  dir.create(file.path(wdir, "results"))
}

datadir <- file.path(wdir, "data") # Data directory path
message("Working directory set to: ", wdir)  # Log message for debugging

# Load required packages
setwd(wdir)
source("load_packages.R")

# ==== load Data and preprocess
## 1. Load SAFEPD raw (?) dataset
# TODO: I am loading the imputed data for the following analysis; why not running teh imputation here?
df_safepd <- read_xlsx(
  file.path(datadir, "SAFEPD_imputeddata.xlsx"),
  range = "A1:DF209",       # Specific range of data to load
  col_types = rep("numeric", 110),  # Specify column types
  n_max = 210              # Load only the first 210 rows
)

# ====
## Dichotomisierung / Imputation
source("SAFEPD_dich.R")
# write.xlsx(SAFEPD, "SAFEPD_imputeddata.xlsx", rowNames = FALSE)

# ====
## Check for normal distribution
source("SAFEPD_dist.R")

# ====
## Descriptive analyses
source("SAFEPD_descr.R")
source("SAFEPD_descr_diag.R") #TODO: Cool functions!

## Korrelationsanalyse #TODO: Not used?
# source("SAFEPD_corr.R")

## Regressionsanalyse
source("SAFEPD_reg.R")

## Hypothesen Untersuchung
# source("SAFEPD_hypothesis.R")

## Datenabgleich / Diskussion
# source("SAFEPD_disc.R")
