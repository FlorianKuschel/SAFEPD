# Run descriptive analyses for the data obtained via questionnaires:
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 1.0 # 2024-09-12, # tidy up code and bring up potential ideas

# TODO: This part is is a nice overview and may serve as a starting point for further ideas. But you  may consider tidying up everything a little. Especially it could make sense to create a single table one (code provided at the end)

## ==========
# Summarise sociodemographic data
describe_vars <- c( # Variables to summarize
  "age",
  "years_since_diagnosis",
  "UPDRS_I_Score",
  "UPDRS_II_Score",
  "FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater",
  "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe",
  "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other", "FIMA_1_Gesamt"
) # TODO: further data may be added here

# Function to summarize using describe
summarize_describe <- function(variable) {
  data <- df_safepd[[variable]]
  desc <- psych::describe(data, IQR = TRUE)
 
  # Convert describe output to a data frame
  data.frame(
    Variable = variable,
    N = desc$n,
    Mean = desc$mean,
    SD = desc$sd,
    Median = desc$median,
    Min = desc$min,
    Max = desc$max,
    IQR = desc$IQR
  )
}

# Apply the summarization function to all variables
describe_results <- do.call(rbind, lapply(describe_vars, summarize_describe))

