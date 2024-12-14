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

## ========== your version
# Soziodemographische Daten
describe(df_safepd$age, IQR = TRUE)
describe(subset(df_safepd$age, df_safepd$gender == 1))
describe(subset(df_safepd$age, df_safepd$gender == 0))

describe(df_safepd$years_since_diagnosis, IQR = TRUE)
describe(subset(df_safepd$years_since_diagnosis, df_safepd$gender == 1))
describe(subset(df_safepd$years_since_diagnosis, df_safepd$gender == 0))

VarDemographic <- c("age", "gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status")

Table_SozDem <- CreateCatTable(
  vars = VarDemographic,
  strata = "gender",
  data = df_safepd,
  includeNA = FALSE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

VarDemographic_Group <- c("age", "gender_Group", "nationality_Group", "martial_status_Group", "years_since_diagnosis", "persons_houshold_Group", "school_graduation_Group", "professional_graduation_Group", "employment_status_Group")

Table_SozDem_Group <- CreateCatTable(
  vars = VarDemographic_Group,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

# Sicherheitsfragebogen
Table_df_safepdQA1 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "UPDRS_I_Score",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

VarArztbesuche <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")
Table_FIMA1 <- CreateCatTable(
  vars = VarArztbesuche,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = TRUE,
  addOverall = TRUE
)

VarTherapie <- c("FIMA_2_Krankengymnastik", "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", "FIMA_2_Heilpraktiker", "FIMA_2_Osteopath", "FIMA_2_Chiropraktiker", "FIMA_2_Psychotherapeut")
Table_FIMA2 <- CreateCatTable(
  vars = VarTherapie,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

VarPflege <- c("FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7", "FIMA_8", "FIMA_8_Group")
Table_FIMA3 <- CreateCatTable(
  vars = VarPflege,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
 
VarKlinik <- c("FIMA_9_Group", "FIMA_10_Group", "FIMA_11_Group", "FIMA_12_Group")
Table_FIMA4 <- CreateCatTable(
  vars = VarKlinik,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
 
VarWohnung <- c("FIMA_15", "FIMA_14_Group")
Table_FIMA5 <- CreateCatTable(
  vars = VarWohnung,
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
 
Table_FIMA6 <- CreateCatTable(
  vars = "FIMA_16_Group",
  strata = "gender",
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)


## ========== ALternative
# Create TableOne (if you opt for this, I would consider moving this to a different filed called TableOne.R or similar)

# 1.1: Helper function: recodes variables as factors
recode_factors <- function(df, vars, levels_labels = NULL) {
  df[vars] <- lapply(df[vars], function(x) factor(as.character(x)))
  if (!is.null(levels_labels)) {
    for (var in names(levels_labels)) {
      df[[var]] <- factor(df[[var]], levels = levels_labels[[var]]$levels, labels = levels_labels[[var]]$labels)
    }
  }
  return(df)
}

# Duplicates df_recoded as df_tableone for clarity
df_tableone <- df_safepd

# 1.2: Define specific factor levels and labels (TODO: The entire next part MUST be adapted to your data)
levels_labels <- list(
  sex = list(levels = c("1", "2", "3"), labels = c("Male", "Female", "Divers")),
  age_r = list(levels = c("1", "2", "3"), labels = c("≤ 60 years", "61 - 69 years", "≥ 70 years")),
  education_r_r = list(levels = c("1", "2", "3"), labels = c("Primary school", "Higher secondary school", "University degree or higher")),
  financial_stability_r_r = list(levels = c("1", "2"), labels = c("Most of the time", "Some of the time or less")),
  family_status_r_r = list(levels = c("1", "2"), labels = c("Without a partner", "With a partner")),
  living_situation_r_r = list(levels = c("1", "2"), labels = c("Independently", "With professional support")),
  living_area_r = list(levels = c("1", "2", "3"), labels = c("Large city", "Medium-small city", "Rural area")),
 
  who5_depr_r = list(levels = c("0", "1"), labels = c("No depression", "Impaired quality of life")),
  comorb_r = list(levels = c("1", "2"), labels = c("Not Present", "Present")),
  time_from_diagnosis_r = list(levels = c("1", "2", "3", "4", "5"), labels = c("<2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years")),
  age_at_diagnosis_r = list(levels = c("1", "2"), labels = c("<50 years", "≥50 years")),
  disturbances_sleep_APD = list(levels = c("0", "1"), labels = c("No", "Yes")),
  ms_fluctuation_APD = list(levels = c("0", "1"), labels = c("No", "Yes")),
  assist_mov_r = list(levels = c("0", "1"), labels = c("No assisted movement", "Assisted movement")),
  falls_r = list(levels = c("1", "2"), labels = c("No", "Yes"))
)

df_tableone <- recode_factors(df_tableone, names(levels_labels), levels_labels) # TODO: once levels_labels is defined, you should run this line

# 1.3: Select variables and create labels for TableOne
Vars <- c("age", "age_r", "sex", "education_r_r", "financial_stability_r_r", "family_status_r_r",
          "living_situation_r_r", "living_area_r", "who5_depr_r", "comorb_r", "age_at_diagnosis_r",
          "time_from_diagnosis_r", "disturbances_sleep_APD", "ms_fluctuation_APD", "assist_mov_r", "falls_r")

colnames_Vars <- c("Age", "Age categorized", "Gender", "Education level", "Financial stability",
                   "Family status", "Household situation", "Living area", "Depression", "Comorbidity",
                   "Age at diagnosis", "Time since diagnosis", "Sleep disturbances",
                   "Motor symptoms fluctuation", "Assisted movement", "Falls")

# 1.4: TableOne function
create_table_one <- function(df, vars, colnames_vars, factor_vars) {
  df_tableOne <- df %>% dplyr::select(all_of(vars))
  colnames(df_tableOne) <- colnames_vars
  results.tableOne <- CreateTableOne(vars = colnames_vars, factorVars = factor_vars, data = df_tableOne)
  print(results.tableOne, nonnormal = c("Time since diagnosis", "Education level"), showAllLevels = TRUE)
  return(results.tableOne)
}

# Create the TableOne and display
factVars <- colnames_Vars[-c(1, 2)]  # Exclude continuous variables
results.tableOne <- create_table_one(df_tableone, Vars, colnames_Vars, factVars)
print(head(df_tableone))

# 1.5: Export TableOne to MS Excel file
export_table_one <- function(results, filename = "Table1.xlsx") {
  tableOne_matrix <- print(results, nonnormal = c("Time since diagnosis", "Education level"), showAllLevels = TRUE)
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
 
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne)
