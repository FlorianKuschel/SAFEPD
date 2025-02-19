# Code developed by Florian Kuschel, Anna and David Pedrosa
# Creates a "TableOne" intended to provide the general information about the analysed population
# Version 2.0 # 2025-01-19, minor changes and some general suggestions
# Version 1.0 # 2024-12-15

# TODO: This Table is a bit lengthy. In general terms, you want to provide general information on gender, age, specific disease
# characteristics, etc. A good example is that there are categories with 202/4 patients or 199/6 patients, indicating that almost
# everyone shares this characteristics. That's something we will exploit later in the regressions; Try to keep a maximum of 10 items;
# I would especially ditch the FIMA, which - BTW - is not coded so far; but otherwise cool script. Personally, I believe SAFEPD_descr.R
# and SAFEPD_desc_diag.R has now become obsolete and I would recommend deleting it.
# TODO: I have tidied up the tables and now show the variables that also appear in the regression in Table 1. 
# For the results, the items of the safety questionnaire are then shown in a seperate table

# ==================================================================================================
# 1. Helper function: recodes variables as factors
# ==================================================================================================
recode_factors <- function(df, vars, levels_labels = NULL) {
  df[vars] <- lapply(df[vars], function(x) factor(as.character(x)))
  if (!is.null(levels_labels)) {
    for (var in names(levels_labels)) {
      df[[var]] <- factor(df[[var]], levels = levels_labels[[var]]$levels, labels = levels_labels[[var]]$labels)
    }
  }
  return(df)
}

# Duplicates df_safepd as df_tableone for clarity
df_tableone_before_imputation <- df_before_imputation
df_tableone_after_imputation <- imputed_data

# ==================================================================================================
# 2. TableOne function
# ==================================================================================================
create_table_one <- function(df, vars, colnames_vars, factor_vars) {
  df_tableOne <- df %>% dplyr::select(all_of(vars))
  colnames(df_tableOne) <- colnames_vars
  results.tableOne <- CreateTableOne(vars = colnames_vars, factorVars = factor_vars, data = df_tableOne)
  print(results.tableOne, nonnormal = c("Years since diagnosis", "UPDRS I", "UPDRS II"), showAllLevels = TRUE)
  return(results.tableOne)
}

# ==================================================================================================
# 3. Define specific factor levels and labels
# ==================================================================================================
levels_labels <- list(
  gender_Group = list(levels = c("0","1"), labels = c("Female", "Male")),
  nationality_Group = list(levels = c("0", "1"), labels = c("other","german")),
  martial_status_Group = list(levels = c("0","1"), labels = c("unmarried", "married")),
  persons_houshold_Group = list (levels = c("0","1"), labels = c("not living alone", "living alone")),
  school_graduation_Group = list (levels = c("0","1"), labels = c("other", "Abitur")),
  professional_graduation_Group = list (levels = c("0","1"), labels = c("other", "graduation")),
  employment_status_Group = list (levels = c("0", "1"), labels = c("other", "retired")),
  lack_of_information_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  uncertain_future_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  chaging_symptom_severity_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  gait_insecurity_fall_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  pain_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  gastrointestinal_symptoms_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  urinary_symptoms_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  mental_abilities_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  mental_symptoms_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  other_disease_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  nursing_care_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  side_effects_complications_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  access_healthcare_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  communication_with_me_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  communication_between_professionals_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  loneliness_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  everyday_problems_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  daily_routine_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  overload_among_people_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  pejorativ_looks_comments_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  family_role_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  conflicts_with_relatives_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  victim_to_crime_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  financial_worries_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  not_at_peace_with_myself_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  participation_in_road_traffic_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted")),
  overall_situation_Group = list(levels = c("0", "1"), labels = c("unrestricted", "restricted"))
)
  

df_tableone_before_imputation <- recode_factors(df_tableone_before_imputation, names(levels_labels), levels_labels)
df_tableone_after_imputation <- recode_factors(df_tableone_after_imputation, names(levels_labels), levels_labels)

# ==================================================================================================
# 4.1. Select variables and create labels for TableOne
# ==================================================================================================
Vars <- c("age", "years_since_diagnosis", "gender_Group", "nationality_Group", "martial_status_Group",
          "persons_houshold_Group", "school_graduation_Group", "professional_graduation_Group", "employment_status_Group",
          "UPDRS_I_Score", "UPDRS_II_Score", "FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_2_Krankengymnastik")

colnames_Vars <- c("Age", "Years since diagnosis", "Gender", "Nationality", "Martial status",
                   "Houshold size", "School education", "Graduation", "Employment status",
                   "UPDRS I", "UPDRS II", "Visits of General Practitioner", "Visits of Neurologist", "Visits of Physiotherapist")

# ==================================================================================================
# 4.2. Create the TableOne and display
# ==================================================================================================
factVars <- colnames_Vars[-c(1, 2, 10, 11)]  # Exclude continuous variables
results.tableOne_before_imputation <- create_table_one(df_tableone_before_imputation, Vars, colnames_Vars, factVars)
print(head(df_tableone_before_imputation))
results.tableOne_after_imputation <- create_table_one(df_tableone_after_imputation, Vars, colnames_Vars, factVars)
print(head(df_tableone_after_imputation))

# ==================================================================================================
# 4.3. Export TableOne to MS Excel file
# ==================================================================================================
export_table_one <- function(results, filename = "Table1.general_demographics_before_imputation.v1.0.xlsx") {
  tableOne_matrix <- print(results, nonnormal = c("Years since diagnosis", "UPDRS I", "UPDRS II"), showAllLevels = FALSE) 
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
  
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne_before_imputation)

export_table_one <- function(results, filename = "Table1.general_demographics_after_imputation.v1.0.xlsx") {
  tableOne_matrix <- print(results, nonnormal = c("Years since diagnosis", "UPDRS I", "UPDRS II"), showAllLevels = FALSE) 
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
  
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne_after_imputation)

# ==================================================================================================
# 5.1. Select variables and create labels for TableOne
# ==================================================================================================
Vars <- c("lack_of_information_Group", "uncertain_future_Group", "chaging_symptom_severity_Group", "gait_insecurity_fall_Group",
          "pain_Group", "gastrointestinal_symptoms_Group", "urinary_symptoms_Group", "mental_abilities_Group", "mental_symptoms_Group",
          "other_disease_Group", "nursing_care_Group", "side_effects_complications_Group", "access_healthcare_Group",
          "communication_with_me_Group", "communication_between_professionals_Group", "loneliness_Group", "everyday_problems_Group",
          "daily_routine_Group", "overload_among_people_Group", "pejorativ_looks_comments_Group", "family_role_Group",
          "conflicts_with_relatives_Group", "victim_to_crime_Group", "financial_worries_Group", "not_at_peace_with_myself_Group",
          "participation_in_road_traffic_Group", "overall_situation_Group")

colnames_Vars <- c("Mangel an Informationen", "Unsichere Zukunft", "Veränderung der Symptomschwere", 
                   "Gangunsicherheit/Stürze", "Schmerzen", "Gastrointestinale Symptome", "Harnwegssymptome", 
                   "Mentale Fähigkeiten", "Mentale Symptome", "Andere Erkrankungen", "Pflegebedarf", 
                   "Nebenwirkungen/Komplikationen", "Zugang zur Gesundheitsversorgung", 
                   "Kommunikation mit dem Patienten", "Kommunikation zwischen Fachkräften", "Einsamkeit", 
                   "Alltagsprobleme", "Tagesroutine", "Überforderung unter Menschen", 
                   "Stigmatisierung", "Rolle in der Familie", "Konflikte mit Verwandten", 
                   "Opfer von Verbrechen", "Finanzielle Sorgen", "Selbstzweifel", 
                   "Teilnahme am Straßenverkehr", "Gesamtsituation")

# ==================================================================================================
# 5.2. Create the TableOne and display
# ==================================================================================================
factVars <- colnames_Vars
results.tableOne <- create_table_one(df_tableone_after_imputation, Vars, colnames_Vars, factVars)
print(head(df_tableone_after_imputation))

# ==================================================================================================
# 5.3. Export TableOne to MS Excel file
# ==================================================================================================
export_table_one <- function(results, filename = "Table2.safetyQA.v1.0.xlsx") {
  tableOne_matrix <- print(results, showAllLevels = FALSE) 
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
  
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne)
