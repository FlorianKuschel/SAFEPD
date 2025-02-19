# Code developed by Florian Kuschel, Anna and David Pedrosa
# Run regression analyses, in order to see the factors related to feeling of safety/lack thereof;
# Version 2.0 # 2025-19-01, # Some additional thoughts on the general principle and some suggestion for an analysis pipeline

# ==================================================================================================
# GLM Analysis: Comparing Full Model with Stepwise Reduction (glmStepAIC) and Elastic Net Regression
# Adapted from:
# 1. https://rpubs.com/mpfoley73/625323
# 2. https://github.com/dpedrosac/EOL_parkinson/blob/main/EOL_PD.R
# ==================================================================================================
# TODO: Verify the number of factors in the dataset. If there are additional factors, consider letting the
# model handle variable selection to reduce complexity. Otherwise, restrict variables only if you have
# clear a priori knowledge to justify it.
# TODO: I use all the socio-demographic data collected, the UPDRS and 3 items regarding the use of healthcare services, 
# the FIMA would otherwise have even more variables, but in some cases very small groups 
# but we had actually discussed leaving these out after all

# Step 1: Define Variables of Interest
variables_of_interest <- c(
  "age", 
  "years_since_diagnosis", 
  "gender_Group", 
  "nationality_Group", 
  "martial_status_Group", 
  "school_graduation_Group", 
  "persons_houshold_Group", 
  "professional_graduation_Group", 
  "employment_status_Group", 
  "UPDRS_I_Score", 
  "UPDRS_II_Score", 
  "FIMA_1_Hausarzt", 
  "FIMA_1_Neurologe", 
  "FIMA_2_Krankengymnastik"
)

# Step 2: Prepare Data
# Convert `overall_situation_Group` to a factor with levels "yes" and "no"
imputed_data <- imputed_data %>%
  mutate(overall_situation_Group_factor = factor(
    ifelse(overall_situation_Group == 0, "no", "yes")
  ))

data_full_glmSAFETY <- imputed_data %>%
  dplyr::select(all_of(variables_of_interest), overall_situation_Group_factor)

# Step 3: Split Data into Training and Testing Sets
# Note: This is necessary to test the model on unseen data and evaluate its performance using metrics like ROC.
index <- createDataPartition(
  data_full_glmSAFETY$overall_situation_Group_factor, 
  p = 0.8, 
  list = FALSE
)

train_data <- data_full_glmSAFETY[index, ]  # Training dataset (here 80%)
test_data <- data_full_glmSAFETY[-index, ]  # Testing dataset (here 20%)

# ==================================================================================================
# Train Control Configuration for Model Training
# ==================================================================================================

# Primary trainControl configuration with repeated cross-validation
# This configuration uses repeated cross-validation (5-fold, repeated 10 times) and minimizes 
# log-loss, a metric commonly used for evaluating probabilistic classification models.
train_control <- trainControl(
  method = "repeatedcv",           # Repeated cross-validation
  number = 5,                      # 5-folds
  repeats = 10,                    # Repeat 10 times
  summaryFunction = mnLogLoss,     # Log Loss as evaluation metric,For an explanation cf: https://towardsdatascience.com/intuition-behind-log-loss-score-4e0c9979680a
  savePredictions = "final",
  classProbs = TRUE,
  verboseIter = TRUE               # offers more control
)

# TODO: Left your train control for comparison purposes
# Definiere die trainControl für Cross Validation
# set.seed(123)
# train_control <- trainControl(
#  method = "cv",           # Kreuzvalidierung
#  number = 10,             # 10-fache CV
#  classProbs = TRUE,       # Berechne Klassenwahrscheinlichkeiten
#  summaryFunction = twoClassSummary # ROC-AUC als Bewertungsmaß
# )

# ==================================================================================================
# a) Estimate the distinct models; (results_model) is defined in functionsUsed.R and basically
# estimated a GLM based on the input and taking advantage of the {caret}-package
# ==================================================================================================
mdl_fullSAFETY = results_modelSAFETYmod(
  method = 'glm',
  data = data_full_glmSAFETY,
  train_control = train_control,
  tunegrid = NULL,
  test_data = test_data,
  model_name = 'Full GLM'
)
annotation_fullSAFETY <- data.frame(
  x = .8,
  y = .6,
  label = sprintf(
    "AUC = %.2f [%.2f; %.2f]",
    mdl_fullSAFETY[[3]]$overall[[1]],
    mdl_fullSAFETY[[3]]$overall[[3]],
    mdl_fullSAFETY[[3]]$overall[[4]]
  )
)

mdl_stepSAFETY = results_modelSAFETYmod(
  method = 'glmStepAIC',
  data = data_full_glmSAFETY,
  train_control = train_control,
  tunegrid = NULL,
  test_data = test_data,
  model_name = 'Stepwise reduced GLM'
)
annotation_stepSAFETY <- data.frame(
  x = .8,
  y = .6,
  label = sprintf(
    "AUC = %.2f [%.2f; %.2f]",
    mdl_stepSAFETY[[3]]$overall[[1]],
    mdl_stepSAFETY[[3]]$overall[[3]],
    mdl_stepSAFETY[[3]]$overall[[4]]
  )
)

lambda.grid <- seq(0.0001, 1, length = 100) #seq(0, 100)
alpha.grid <- seq(0, 1, length = 11) #1
grid_total <- expand.grid(
  alpha = alpha.grid,
  lambda = lambda.grid
)
mdl_penSAFETY = results_modelSAFETYmod(
  method = 'glmnet',
  data = data_full_glmSAFETY,
  train_control = train_control,
  tunegrid = grid_total,
  test_data = test_data,
  model_name = 'ElasticNet regularization'
)
annotation_penSAFETY <- data.frame(
  x = .8,
  y = .6,
  label = sprintf(
    "AUC = %.2f [%.2f; %.2f]",
    mdl_penSAFETY[[3]]$overall[[1]],
    mdl_penSAFETY[[3]]$overall[[3]],
    mdl_penSAFETY[[3]]$overall[[4]]
  )
)

# ==================================================================================================
# Print results in separate subfigures (not really informative, therefore deprecated in EOL_PD back then)

fig99a <- print_AUC(mdl_fullSAFETY[[1]], test_data = test_data, annotation = annotation_fullSAFETY,
                    subtitle="Full GLM")
fig99b <- print_AUC(mdl_stepSAFETY[[1]], test_data = test_data, annotation = annotation_stepSAFETY,
                    subtitle="Stepwise reduced GLM (AIC)")
fig99c <- print_AUC(mdl_penSAFETY[[1]], test_data = test_data, annotation = annotation_penSAFETY,
                    subtitle="ElasticNet regularization")

# ==================================================================================================
# Bootstrap confidence intervals for the models (SAFETY); this takes a while and so results are saved once locally

file2save_bootstrap <- file.path(wdir, "results", "CIdataBootstrapSAFETY.v1.0.Rdata")
if (!file.exists(file2save_bootstrap)){ # loads data if existent
  nboot = 1000
  CI_full <- results_bootstrap(method='glm', data=data_full_glmSAFETY, test_data=test_data,
                               model_name='FULL', nboot = nboot, predictor = 'overall_situation_Group_factor')
  CI_step <- results_bootstrap(method='glmStepAIC', data=data_full_glmSAFETY, test_data=test_data,
                               model_name='STEP', nboot = nboot, predictor = 'overall_situation_Group_factor')
  CI_pen <- results_bootstrap(method='glmnet', data=data_full_glmSAFETY, test_data=test_data,
                              model_name='PEN', nboot = nboot, predictor = 'overall_situation_Group_factor')
  save(list = c("CI_full", "CI_step", "CI_pen"),
       file = file2save_bootstrap)
} else {
  load(file2save_bootstrap)
}


# ==================================================================================================
# Plot metrics for distinct models
plot_results_withCI(CI_full, CI_step, CI_pen, "Safety", "Figure1.model_comparisonSAFETY.v1.0.pdf")

# ==================================================================================================
# Plot confidence intervals from the penalised model for all factors
pdf(file        <- file.path(wdir, "results", "Suppl.Figure1.coefsBootstrapSAFETY.v1.0.pdf"))
coefs                 <- data.frame(as.matrix(coef(mdl_penSAFETY[[1]]$finalModel,
                                                   mdl_penSAFETY[[1]]$bestTune$lambda))) # extracts all coefficients from the  penalised model with all data
CIpen2plot                                 <- CI_pen[[2]] %>% drop_na() %>% dplyr::select(2:dim(CI_pen[[2]])[2])
# r                                         <- colSums(CIpen2plot == 0)
names_predictors                <- colnames(CI_pen[[2]])         # extracts the predictors to plot later as a sort of "legend"
colnames(CI_pen[[2]])         <- 1:length(CI_pen[[2]])          # replaces predictors with numbers to make plot easier to read
ggplot(stack(CI_pen[[2]]), aes(x = ind, y = values)) +
  geom_boxplot() +
  geom_jitter(width=0.15, alpha=0.1) +
  theme_minimal() +
  labs(
    y = "",
    x = "",
    fill = NULL,
    title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) - SAFETY ",
    caption = "") +
  theme(plot.title = element_text(size=22)) +
  scale_color_brewer(palette = 1) +
  ylim(c(-2,2))
dev.off()

# ==================================================================================================
# Plot confidence intervals from the penalised model for all significant factors - SAFETY
pdf(file = file.path(wdir, "results", "Figure2.coefsBootstrapPenalisedModelSAFETY.v1.0.pdf"))
zero_counts <- colSums(CI_pen[[2]] == 0)
significant_predictors <- names_predictors[zero_counts/1000 < .05]
idx_CIpen <- rownames(coefs)[which(coefs != 0)]
idx_CIpen <- gsub("Yes$", "", idx_CIpen)
colnames(CI_pen[[2]]) <- names_predictors
data2plot <- CI_pen[[2]] %>%
  dplyr::select(all_of(idx_CIpen)) %>%
  dplyr::select(-"(Intercept)")
colnames(data2plot) <- c(
  "Geschlecht",
  "UPDRS I\nScore",
  "UPDRS II\nScore"
)
ggplot(stack(data2plot), aes(x = ind, y = values)) +
  geom_boxplot() +
  ylim(c(-2, 2)) +
  scale_x_discrete(labels = colnames(data2plot)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Dreht x-Achse-Beschriftung
    plot.margin = margin(10, 10, 50, 10),  # Mehr Platz unten
    plot.title = element_text(size = 10, hjust = 0.5),  # Zentriert den Titel
    plot.title.position = "plot" 
  ) +
  labs(
    y = "",
    x = "",
    fill = NULL,
    title = "Koeffizienten durch Bootstrapping für das penalisierte Regressionsmodell (ElasticNet)\n
             Abhängige Variable: Sicherheit"
  ) +
  scale_fill_brewer(palette = 1)
dev.off()

# ==================================================================================================
# Create table for penalised reduced model - SAFETY
mdl_pen_final         <- mdl_penSAFETY[[1]]
coefs <- data.frame(as.matrix(coef(mdl_pen_final$finalModel, mdl_pen_final$bestTune$lambda)))
sig_predictors         <- which(coefs != 0)
mdl_pen_sig         <- data.frame(predictor = c("(Intercept)", "gender_Group", 
                                                 "UPDRS_I_Score", "UPDRS_II_Score"),
                                                                 coef=coefs[sig_predictors,])

write.csv(mdl_pen_sig, file.path(wdir, "results", "table3.ResultsElasticNet_modelSAFETY.v1.0.csv"),
          row.names = T) # csv-file may be easily imported into text processing software

# ==================================================================================================
# Create table for stepwise reduced model  - SAFETY
mdl_step_final         <- mdl_stepSAFETY[[1]]
# sig_predictors         <- attr(which(summary(mdl_step_final)$coef[,4] <= .05), "names")
mdl_step_sig         <- data.frame(summary(mdl_step_final)$coef)
sig_predictors <- which(mdl_step_sig[,4]<.05 | mdl_step_sig[,4]>.95)
mdl_step_sig         <- mdl_step_sig[sig_predictors, ]
rownames(mdl_step_sig) <- c("gender_Group", "UPDRS_I_Score")

write.csv(mdl_step_sig, file.path(wdir, "results", "table4.ResultsStepWiseReduced_modelSAFETY.v1.0.csv"),
          row.names = T) # csv-file may be easily imported into text processing software

# ==================================================================================================
# Create table for full model - SAFETY
mdl_full_final <- mdl_fullSAFETY[[1]]
coefs_full <- data.frame(summary(mdl_full_final)$coef)
sig_predictors_full <- which(coefs_full[, 4] < .05 | coefs_full[, 4] > .95) 
mdl_full_sig <- coefs_full[sig_predictors_full, ]
rownames(mdl_full_sig) <- rownames(coefs_full)[sig_predictors_full]

write.csv(mdl_full_sig, file.path(wdir, "results", "table5.ResultsFull_modelSAFETY.v1.0.csv"),
          row.names = TRUE)
