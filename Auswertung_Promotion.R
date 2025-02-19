# ==================================================================================================
# SAFEPD Cross-Sectional Study Analyses
# Developed by Florian Kuschel, Anna and David Pedrosa
# --------------------------------------------------------------------------------------------------
# This script conducts analyses on the perceived safety of people with Parkinson's Disease (PwPD).
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# Version History
# --------------------------------------------------------------------------------------------------
# Version 2.5 (2025-01-19): Added suggestions for debugging code
# Version 2.4 (2024-12-04)
# Version 2.3 (2024-01-12): Updated code with suggestions, added comments/questions in load_data
# Version 2.2 (2024-11-26): Adjustments for readability among collaborators

# ==================================================================================================
# 1. Preamble
# --------------------------------------------------------------------------------------------------
# Load required packages
source("load_packages.R")

# Load functions
source("functionsUsed.R")

# Set respective folders according to username to facilitate collaboration
username <- Sys.getenv("USERNAME")

wdir <- dplyr::if_else(
  username == "dpedrosac",
  "/media/storage/SAFEPD/",
  "~/SAFEPD/" # Collaborators need to ensure this directory exists
)
setwd(wdir)

# Create results folder if it doesn't exist
if (!dir.exists(file.path(wdir, "results"))) {
  dir.create(file.path(wdir, "results"))
}

# Define data directory path
datadir <- file.path(wdir, "data")
message("Working directory set to: ", wdir)  # Log message for debugging

# ==================================================================================================
# 2. Load and Preprocess Data
# --------------------------------------------------------------------------------------------------
# 2.1 Load SAFEPD raw dataset (imputed data for analysis)
df_safepd <- read_xlsx(
  file.path(datadir, "SAFEPD_data.xlsx"),
  range = "A1:DF209",       # Specific range of data to load
  col_types = rep("numeric", 110),  # Specify column types
  n_max = 210              # Load only the first 210 rows
)

# 2.2 Data imputation
source("SAFEPD_imputation.R")

# 2.3 Dichotomisation
source("SAFEPD_dichotomisation.R")

# 2.4 Check for normal distribution
source("SAFEPD_distribution.R")

# ==================================================================================================
# 3. Analyses
# --------------------------------------------------------------------------------------------------

# 3.3 Descriptive analyses
source("SAFEPD_TableOne.R")

# 3.4 Regression analysis
source("SAFEPD_regression.R")
