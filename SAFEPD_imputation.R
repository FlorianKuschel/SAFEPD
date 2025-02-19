# Imputation of missing variables/input if feasible
# Code developed by Florian Kuschel, Anna and David Pedrosa
# Version 1.0 # 2024-09-12, # first draft of code adapted from distinct repository (https://www.github.com/dpedrosac/qol_prospective/)
# Version 2.0 # 2025-19-01, # small change so only "inlist"-variables are considered for imputation

# ================================================================================================== 
# 1. Preparing data before imputation
# ================================================================================================== 
df_before_imputation <- df_safepd # %>%

# Percentage of the data that is missing!
total_missing <- sum(is.na(df_before_imputation))
total_values <- prod(dim(df_before_imputation))
missing_percentage <- (total_missing / total_values) * 100

# Output the percentage of missing values
cat("Percentage of missing values in selected variables for analysis:", missing_percentage, "%\n")

pdf(file.path(getwd(), "results", "suppl_fig1a.aggr_plot_output.pdf"), width = 11, height = 8.5)  # Save the aggr plot output to a PDF file in landscape orientation

aggr_plot <- aggr(
  df_before_imputation,
  col = c('navyblue', 'red'),
  numbers = TRUE,
  sortVars = TRUE,
  labels = colnames(df_before_imputation),
  cex.axis = 0.5,
  gap = 3,
  ylab = c("Histogram of missing data", "Pattern")
)


text(
  x = 0.25,  # Centered horizontally
  y = .84, # Position below the plot (adjust if needed)
  sprintf("Percentage of missing values in \nselected variables for analysis is: %.2f%%", missing_percentage),
  cex = 1.2,
  pos = 1 # Aligns text below the specified coordinates
)

dev.off() # Close the PDF device to save the file

# ================================================================================================== 
# 2. Start imputation
# ================================================================================================== 
# Define the variables to be included as covariates in each imputation model
inlist <- c("age", "years_since_diagnosis", "gender", "nationality",
            "martial_status", "school_graduation", "persons_houshold", 
            "professional_graduation", "employment_status", 
            "lack_of_information",  "uncertain_future",  "chaging_symptom_severity",  "gait_insecurity_fall",  
            "pain",  "gastrointestinal_symptoms",  "urinary_symptoms", "mental_abilities", "mental_symptoms", "other_disease",  "nursing_care",  "side_effects_complications",  "access_healthcare",
            "communication_with_me",  "communication_between_professionals",  "loneliness",  "everyday_problems",
            "daily_routine",  "overload_among_people",  "pejorativ_looks_comments",  "family_role",  
            "conflicts_with_relatives",  "victim_to_crime", "financial_worries", "not_at_peace_with_myself",  
            "participation_in_road_traffic",  "overall_situation", 
            "UPDRS_I_Score", "UPDRS_II_Score", "FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_2_Krankengymnastik"
)

# Generate predictor matrix with minimum proportion of usable cases set to 0.5
df_for_imputation <- df_before_imputation[, inlist]
pred <- quickpred(
  df_for_imputation,
  minpuc = 0.5
)

# Perform multiple imputation using the MICE package
generate_imputation <- mice(
  data = df_for_imputation,
  predictorMatrix = pred,
  method = "midastouch", # Imputation method
  m = 10,                # Number of imputed datasets
  maxit = 5,             # Number of iterations
  diagnostics = TRUE     # Enable diagnostics
)

# Generate and save density plot to PDF in landscape orientation
pdf(file.path(getwd(), "results", "suppl_fig1b.densityplots_afterimputation.pdf"), width = 11, height = 8.5)

densityplot(
  generate_imputation,
  xlim = c(0, 7),
  ylim = c(0, 1)
)

dev.off()  # Close the PDF device to save the file

# Save data after imputation as imputed values
imputed_values <- complete(generate_imputation, 1)

# Save data after imputation in original dataset
imputed_data <- df_before_imputation  
imputed_data[inlist] <- imputed_values  

# Visualize missing data after imputation
if (flag_check) {
  aggr_plot <- aggr(
    imputed_data,
    col = c('navyblue', 'red'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = colnames(imputed_data),
    cex.axis = 0.5,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )
}

        
