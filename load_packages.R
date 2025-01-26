# Helper function called in the main script to load necessary packages
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Sorted package list
packages <- c(
  "brant",
  "car",
  "caret",
  "descr",
  "dplyr",
  "easystats",
  "ggplot2",
  "ggpubr",
  "gofcat",
  "gplots",
  "grid",
  "gridExtra",
  "MASS",
  "MLmetrics",
  "mice",
  "openxlsx",
  "ordinal",
  "patchwork",
  "psych",
  "pscl",
  "purrr",
  "readxl",
  "sjlabelled",
  "sjmisc",
  "sjPlot",
  "sjstats",
  "tableone",
  "tidyverse",
  "VIM",
  "writexl",
  "yardstick"
)

# Load or install and load all packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

