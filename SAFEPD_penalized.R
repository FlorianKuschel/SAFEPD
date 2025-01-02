alle_variablen2 <- c("age", 
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
                    "FIMA_2_Krankengymnastik") 


# penalised regression
# Vorbereitung der Daten
# Die unabhängigen Variablen (Prädiktoren) werden in X gespeichert
# Die Zielvariable wird in y gespeichert
x <- df_safepd %>%
  dplyr::select(all_of(alle_variablen2)) %>%
  mutate_if(is.factor, as.numeric) %>%       
  as.matrix()

df_safepd$overall_situation_Group_factor <- as.factor(df_safepd$overall_situation_Group)
df_safepd$overall_situation_Group_factor <- make.names(df_safepd$overall_situation_Group_factor)
y <- df_safepd$overall_situation_Group_factor

# Definiere die trainControl für Cross Validation
set.seed(123)
train_control <- trainControl(
  method = "cv",           # Kreuzvalidierung
  number = 10,             # 10-fache CV
  classProbs = TRUE,       # Berechne Klassenwahrscheinlichkeiten
  summaryFunction = twoClassSummary # ROC-AUC als Bewertungsmaß
)

# Penalised Regression: Ridge (alpha = 0), Lasso (alpha = 1), Elastic Net (alpha zwischen 0 und 1)

### Ridge Regression
set.seed(123)
ridge_model <- train(
  x = x, 
  y = as.factor(y),
  method = "glmnet",
  family = "binomial",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-4, 2, length = 100)) # Alpha = 0 für Ridge
)

### Lasso Regression
set.seed(123)
lasso_model <- train(
  x = x, 
  y = as.factor(y),
  method = "glmnet",
  family = "binomial",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, 2, length = 100)) # Alpha = 1 für Lasso
)

### Elastic Net Regression
set.seed(123)
elastic_model <- train(
  x = x, 
  y = as.factor(y),
  method = "glmnet",
  family = "binomial",
  trControl = train_control,
  tuneLength = 10 # Automatische Abstimmung von alpha und lambda
)

# Ergebnisse vergleichen
cat("Ridge Regression: Best Lambda:", ridge_model$bestTune$lambda, "\n")
cat("Lasso Regression: Best Lambda:", lasso_model$bestTune$lambda, "\n")
cat("Elastic Net: Best Alpha:", elastic_model$bestTune$alpha, 
    "Best Lambda:", elastic_model$bestTune$lambda, "\n")

# Modellgüte mit ROC AUC bewerten
ridge_probs <- predict(ridge_model, x, type = "prob")[,2]
lasso_probs <- predict(lasso_model, x, type = "prob")[,2]
elastic_probs <- predict(elastic_model, x, type = "prob")[,2]

ridge_roc <- roc(response = y, predictor = ridge_probs)
lasso_roc <- roc(response = y, predictor = lasso_probs)
elastic_roc <- roc(response = y, predictor = elastic_probs)

cat("Ridge AUC:", auc(ridge_roc), "\n")
cat("Lasso AUC:", auc(lasso_roc), "\n")
cat("Elastic Net AUC:", auc(elastic_roc), "\n")

# Plot der ROC-Kurven
plot(ridge_roc, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(lasso_roc, col = "red", lwd = 2)
lines(elastic_roc, col = "green", lwd = 2)
legend("bottomright", legend = c("Ridge", "Lasso", "Elastic Net"), 
       col = c("blue", "red", "green"), lwd = 2)

# Extrahiere die finalen Koeffizienten für das beste Lambda aus elastic_model
final_lambda <- elastic_model$bestTune$lambda
coefficients <- coef(elastic_model$finalModel, s = final_lambda)

# Konvertiere Koeffizienten in eine verständliche Form
coef_df <- as.data.frame(as.matrix(coefficients))
coef_df <- data.frame(Variable = rownames(coef_df), Coefficient = coef_df[, 1])
coef_df <- coef_df[coef_df$Coefficient != 0, ]  # Entferne Koeffizienten, die Null sind

# Berechne die Odds Ratios (OR) aus Elastic Net
coef_df$OR <- exp(coef_df$Coefficient)

# Zeige Ergebnisse
print(coef_df)

## Bootstrapping für Konfidenzintervalle
# Lineares Modell für den Datensatz
original_model <- lm("overall_situation_Group ~
                          age + 
                          years_since_diagnosis + 
                          gender_Group + 
                          nationality_Group +
                          martial_status_Group +
                          school_graduation_Group +
                          persons_houshold_Group +
                          professional_graduation_Group +
                          employment_status_Group +
                          UPDRS_I_Score +
                          UPDRS_II_Score +
                          FIMA_1_Hausarzt +
                          FIMA_1_Neurologe +
                          FIMA_2_Krankengymnastik",
                     data = df_safepd)

# Anzahl Bootstrap-Stichproben
nboot <- 1000

# Funktion zum Bootstrappen
bootstrap_ci <- function(data, nboot, formula) {
  # Container für Bootstrap-Koeffizienten
  coef_boot <- matrix(NA, nrow = nboot, ncol = length(all.vars(formula)) - 1)
  colnames(coef_boot) <- all.vars(formula)[-1]
  
  pb <- txtProgressBar(min = 0, max = nboot, style = 3)
  for (i in 1:nboot) {
    setTxtProgressBar(pb, i)
    # Bootstrap-Stichprobe
    boot_idx <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    # Logistisches Regressionsmodell
    model <- glm(formula, data = boot_data, family = binomial)
    
    # Speichere die Koeffizienten
    coef_boot[i, ] <- coef(model)[-1]  # Ohne Intercept
  }
  close(pb)
  
  # Berechnung der Konfidenzintervalle für Koeffizienten
  coef_ci <- apply(coef_boot, 2, function(x) {
    quantile(x, probs = c(0.025, 0.975))  # 95%-Konfidenzintervall
  })
  
  # Berechnung der Odds Ratios und deren Konfidenzintervalle
  or_boot <- exp(coef_boot)
  or_ci <- apply(or_boot, 2, function(x) {
    quantile(x, probs = c(0.025, 0.975))  # 95%-Konfidenzintervall
  })
  
  return(list(
    coef_ci = coef_ci,   # Konfidenzintervalle der Koeffizienten
    or_ci = or_ci        # Konfidenzintervalle der Odds Ratios
  ))
}


# Konfidenzintervalle berechnen
results <- bootstrap_ci(
  data = df_safepd,
  nboot = 1000,
  formula = formula(original_model)
)

# Ergebnisse anzeigen
cat("Konfidenzintervalle für die Koeffizienten:\n")
print(results$coef_ci)

cat("\nKonfidenzintervalle für die Odds Ratios:\n")
print(results$or_ci)

## gemeinsame tabellarische Darstellung der Ergebnisse
# Extrahiere die Konfidenzintervalle
ci_table <- as.data.frame(t(results$or_ci))

# Konvertiere die CI-Werte in separate Spalten
ci_table <- data.frame(
  Variable = rownames(ci_table),
  `2.5% CI` = ci_table[, "2.5%"],
  `97.5% CI` = ci_table[, "97.5%"]
)
rownames(ci_table) <- NULL

# Kombiniere die Tabellen
final_table <- merge(
  coef_df, ci_table, 
  by = "Variable", 
  all.x = TRUE
)

# Umbenennen der Spaltenüberschriften der finalen Tabelle
colnames(final_table) <- c("Variable", "Coefficient", "OR", "lower CI", "upper CI")

# Setze die Variable 'Variable' als Faktor mit der Reihenfolge aus alle_variablen2
final_table$Variable <- factor(final_table$Variable, levels = alle_variablen2)

# Sortiere die Tabelle nach der Reihenfolge der Variablen
final_table_sorted <- final_table[order(final_table$Variable), ]

# Tabelle anzeigen
print(final_table)
print(final_table_sorted)








