# Run Descriptive Analysis with TableOne

# Code developed by Florian Kuschel, Anna and David Pedrosa
# Version 1.0 # 2024-15-12


# Create TableOne

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
  gender_Group = list(levels = c("0","1"), labels = c("Female", "Male")),
  nationality_Group = list(levels = c("0", "1"), labels = c("other","german")),
  martial_status_Group = list(levels = c("0","1"), labels = c("unmarried", "married")),
  persons_houshold_Group = list (levels = c("0","1"), labels = c("not living alone", "living alone")),
  school_graduation_Group = list (levels = c("0","1"), labels = c("other", "Abitur")),
  professional_graduation_Group = list (levels = c("0","1"), labels = c("other", "graduation")),
  employment_status_Group = list (levels = c("0", "1"), labels = c("other", "retired")),
  lack_of_information_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  uncertain_future_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  chaging_symptom_severity_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  gait_insecurity_fall_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  pain_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  gastrointestinal_symptoms_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  urinary_symptoms_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  mental_abilities_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  mental_symptoms_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  other_disease_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  nursing_care_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  side_effects_complications_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  access_healthcare_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  communication_with_me_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  communication_between_professionals_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  loneliness_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  everyday_problems_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  daily_routine_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  overload_among_people_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  pejorativ_looks_comments_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  family_role_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  conflicts_with_relatives_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  victim_to_crime_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  financial_worries_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  not_at_peace_with_myself_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  participation_in_road_traffic_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted")),
  overall_situation_Group = list(levels = c("0", "1"), labels = c("restricted", "unrestricted"))
)

df_tableone <- recode_factors(df_tableone, names(levels_labels), levels_labels) # TODO: once levels_labels is defined, you should run this line

# 1.3: Select variables and create labels for TableOne
Vars <- c("age", "years_since_diagnosis", "gender_Group", "nationality_Group", "martial_status_Group", 
          "persons_houshold_Group", "school_graduation_Group", "professional_graduation_Group", "employment_status_Group",
          "lack_of_information_Group", "uncertain_future_Group", "chaging_symptom_severity_Group", "gait_insecurity_fall_Group", 
          "pain_Group", "gastrointestinal_symptoms_Group", "urinary_symptoms_Group", "mental_abilities_Group", "mental_symptoms_Group",
          "other_disease_Group", "nursing_care_Group", "side_effects_complications_Group", "access_healthcare_Group", 
          "communication_with_me_Group", "communication_between_professionals_Group", "loneliness_Group", "everyday_problems_Group", 
          "daily_routine_Group", "overload_among_people_Group", "pejorativ_looks_comments_Group", "family_role_Group", 
          "conflicts_with_relatives_Group", "victim_to_crime_Group", "financial_worries_Group", "not_at_peace_with_myself_Group", 
          "participation_in_road_traffic_Group", "overall_situation_Group", "UPDRS_I_Score", "UPDRS_II_Score", 
          "FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", 
          "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other", "FIMA_2_Krankengymnastik", 
          "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", "FIMA_2_Heilpraktiker", "FIMA_2_Osteopath", "FIMA_2_Chiropraktiker", 
          "FIMA_2_Psychotherapeut", "FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7", "FIMA_8", "FIMA_9", "FIMA_10", 
          "FIMA_11", "FIMA_12", "FIMA_14", "FIMA_15", "FIMA_16", "FIMA_13_Anzahl")

colnames_Vars <- c("Age", "Years since diagnosis", "Gender", "Nationality", "Martial status", 
                   "Houshold size", "School education", "Graduation", "Employment status",
                   "Lack of Information", "Uncertain Future", "Changing Symptom Severity", "Gait Insecurity Fall", "Pain", 
                   "Gastrointestinal Symptoms", "Urinary Symptoms", "Mental Abilities", "Mental Symptoms", "Other Disease", 
                   "Nursing Care", "Side Effects Complications", "Access Healthcare", "Communication With Me", 
                   "Communication Between Professionals", "Loneliness", "Everyday Problems", "Daily Routine", "Overload Among People", 
                   "Pejorative Looks Comments", "Family Role", "Conflicts With Relatives", "Victim To Crime", "Financial Worries", 
                   "Not At Peace With Myself", "Participation In Road Traffic", "Overall Situation", "UPDRS I", "UPDRS II",
                   "Visits of General Practitioner", "Visits of Neurologist", "Visits of Psychiatrist", "Visits of Internist", 
                   "Visits of Gynecologist", "Visits of Urologist", "Visits of Orthopedist", "Visits of Emergency Department", 
                   "Visits of Other Specialists", "Visits of Physiotherapist", "Visits of Occupational Therapist", 
                   "Visits of Speech Therapist", "Visits of Alternative Medicine Practitioner", "Visits of Osteopath", 
                   "Visits of Chiropractor", "Visits of Psychotherapist", "Use of Outpatient Care Service",
                   "Use of Paid Domestic Help", "Help from Family Members or Friends", "Use of Day Care Facility", 
                   "Nursing Care Level", "Use of Inpatient Rehabilitation Facility", "Use of Day Clinic", "Use of Hospital Treatment", 
                   "Use of Inpatient Psychiatric Treatment", "Living Situation", "Move in the Last 12 Months", "Insurance Status",
                   "Number of Assistive Devices")

# 1.4: TableOne function
create_table_one <- function(df, vars, colnames_vars, factor_vars) {
  df_tableOne <- df %>% dplyr::select(all_of(vars))
  colnames(df_tableOne) <- colnames_vars
  results.tableOne <- CreateTableOne(vars = colnames_vars, factorVars = factor_vars, data = df_tableOne)
  print(results.tableOne, nonnormal = c("Years since diagnosis", "UPDRS I", "UPDRS II"), showAllLevels = TRUE)
  return(results.tableOne)
}

# Create the TableOne and display
factVars <- colnames_Vars[-c(1, 2, 37, 38)]  # Exclude continuous variables
results.tableOne <- create_table_one(df_tableone, Vars, colnames_Vars, factVars)
print(head(df_tableone))

# 1.5: Export TableOne to MS Excel file
export_table_one <- function(results, filename = "Table1.xlsx") {
  tableOne_matrix <- print(results, nonnormal = c("Years since diagnosis", "UPDRS I", "UPDRS II"), showAllLevels = TRUE)
  tableOne_with_rowname <- cbind(Variable = rownames(tableOne_matrix), tableOne_matrix)
  
  wb1 <- createWorkbook()
  addWorksheet(wb1, "Table1")
  writeData(wb1, "Table1", tableOne_with_rowname)
  setColWidths(wb1, "Table1", cols = 1:ncol(tableOne_with_rowname), widths = "auto")
  saveWorkbook(wb1, file = file.path(getwd(), "results", filename), overwrite = TRUE)
}

export_table_one(results.tableOne)
