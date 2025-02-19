# Code developed by Florian Kuschel, Anna and David Pedrosa
# Dichotomisation of data and recoding the variables in order to prepare for subsequent analyses in the SAFEPD study 
# Version 2.2 # 2024-01-12, # updated the code with suggestions; added some comments/questions

# ================================================================================================== 
# 1.1 Dichotomisation of sociodemographic data in the dataset before imputation
# ================================================================================================== 
df_before_imputation <- df_before_imputation %>%
  mutate(
    gender_Group = as.numeric(gender, 
                              levels = c(0, 1), 
                              labels = c("female", "male")),
    nationality_Group = as.numeric(ifelse(nationality == 0, 1, 0), 
                                   levels = c(0, 1), 
                                   labels = c("other", "german")),
    martial_status_Group =  as.numeric(ifelse(martial_status == 1, 1, 0), 
                                       levels = c(0, 1), 
                                       labels = c("unmarried", "married")),
    persons_houshold = if_else(persons_houshold == 0, 1, persons_houshold),
    persons_houshold_Group = as.numeric(ifelse(persons_houshold == 1, 1, 0), 
                                        levels = c(0, 1), 
                                        labels = c("not living alone", "living alone")),
    school_graduation_Group = as.numeric(ifelse(school_graduation == 3, 1, 0), 
                                         levels = c(0, 1), 
                                         labels = c("other", "Abitur")),
    professional_graduation_Group = as.numeric(ifelse(professional_graduation == 4, 1, 0), 
                                               levels = c(0, 1), 
                                               labels = c("without graduation", "graduation")),
    employment_status_Group = as.numeric(ifelse(employment_status == 4, 1, 0), 
                                         levels = c(0, 1), 
                                         labels = c("not retired", "retired"))
  )

# ================================================================================================== 
# 1.2 Dichotomisation of sociodemographic data in the imputed dataset
# ================================================================================================== 
imputed_data <- imputed_data %>%
  mutate(
    gender_Group = as.numeric(gender, 
                              levels = c(0, 1), 
                              labels = c("female", "male")),
    nationality_Group = as.numeric(ifelse(nationality == 0, 1, 0), 
                                   levels = c(0, 1), 
                                   labels = c("other", "german")),
    martial_status_Group =  as.numeric(ifelse(martial_status == 1, 1, 0), 
                                       levels = c(0, 1), 
                                       labels = c("unmarried", "married")),
    persons_houshold = if_else(persons_houshold == 0, 1, persons_houshold),
    persons_houshold_Group = as.numeric(ifelse(persons_houshold == 1, 1, 0), 
                                        levels = c(0, 1), 
                                        labels = c("not living alone", "living alone")),
    school_graduation_Group = as.numeric(ifelse(school_graduation == 3, 1, 0), 
                                         levels = c(0, 1), 
                                         labels = c("other", "Abitur")),
    professional_graduation_Group = as.numeric(ifelse(professional_graduation == 4, 1, 0), 
                                               levels = c(0, 1), 
                                               labels = c("without graduation", "graduation")),
    employment_status_Group = as.numeric(ifelse(employment_status == 4, 1, 0), 
                                         levels = c(0, 1), 
                                         labels = c("not retired", "retired"))
  )

# ================================================================================================== 
# 2.1 Define variables from safety questionnaire for transformation
# ================================================================================================== 
VarQA <- c(
  "lack_of_information", "uncertain_future", "chaging_symptom_severity", 
  "gait_insecurity_fall", "pain", "gastrointestinal_symptoms", 
  "urinary_symptoms", "mental_abilities", "mental_symptoms", 
  "other_disease", "nursing_care", "side_effects_complications", 
  "access_healthcare", "communication_with_me", 
  "communication_between_professionals", "loneliness", "everyday_problems", 
  "daily_routine", "overload_among_people", "pejorativ_looks_comments", 
  "family_role", "conflicts_with_relatives", "victim_to_crime", 
  "financial_worries", "not_at_peace_with_myself", 
  "participation_in_road_traffic", "overall_situation"
)

# ================================================================================================== 
# 2.2. Transform and create new grouped variables in the dataset before imputation
# ================================================================================================== 
df_before_imputation <- df_before_imputation %>%
  mutate(across(
    all_of(VarQA),
    ~ as.numeric(if_else(. %in% 1:3, 1, 0)), # 1 = eingeschränkt, 0 = uneingeschränkt
    .names = "{.col}_Group"                # Append "_Group" to each variable name
  ))

# ================================================================================================== 
# 2.3. Transform and create new grouped variables in the imputed dataset
# ================================================================================================== 
imputed_data <- imputed_data %>%
  mutate(across(
    all_of(VarQA),
    ~ as.numeric(if_else(. %in% 1:3, 1, 0)), # 1 = eingeschränkt, 0 = uneingeschränkt
    .names = "{.col}_Group"                # Append "_Group" to each variable name
  ))

# ================================================================================================== 
# 3.1. Transform and create new grouped variables in the dataset before imputation
# ================================================================================================== 
df_before_imputation <- df_before_imputation %>%
  mutate(
    FIMA_1_Hausarzt = ifelse(FIMA_1_Hausarzt == 0, "No", "Yes"),
    FIMA_1_Neurologe = ifelse(FIMA_1_Neurologe == 0, "No", "Yes"),
    FIMA_2_Krankengymnastik = ifelse(FIMA_2_Krankengymnastik == 0, "No", "Yes")
  )

# ================================================================================================== 
# 3.2. Transform and create new grouped variables in the imputed dataset
# ================================================================================================== 
imputed_data <- imputed_data %>%
  mutate(
    FIMA_1_Hausarzt = ifelse(FIMA_1_Hausarzt == 0, "No", "Yes"),
    FIMA_1_Neurologe = ifelse(FIMA_1_Neurologe == 0, "No", "Yes"),
    FIMA_2_Krankengymnastik = ifelse(FIMA_2_Krankengymnastik == 0, "No", "Yes")
  )
