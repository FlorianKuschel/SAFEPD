# Code developed by Florian Kuschel and David Pedrosa
# Analyse and visualize normality and distribution of continous variables
# Version 2.4 # 2025-01-19, # Added improvements for efficiency, validation, and enhanced visualization
# Version 2.3 # 2024-15-12, # added KS-Test to test for normality, indicating normaldistribution for age (and UPDRS I), as seen in Q-Q-Plot
# Version 2.2 # 2024-01-12, # updated the code with suggestions; added one questions to your comment on the UPDRS scores

# ==================================================================================================
# 1. Generalized function for normality tests
# ==================================================================================================
test_normality <- function(variable, data) {
  if (is.numeric(data[[variable]])) {
    shapiro_test <- shapiro.test(data[[variable]])
    ks_test <- ks.test(
      data[[variable]], "pnorm",
      mean(data[[variable]], na.rm = TRUE),
      sd(data[[variable]], na.rm = TRUE)
    )
    data.frame(
      Variable = variable,
      Shapiro_p_value = shapiro_test$p.value,
      Shapiro_is_normal = shapiro_test$p.value > 0.05,
      KS_p_value = ks_test$p.value,
      KS_is_normal = ks_test$p.value > 0.05
    )
  } else {
    data.frame(Variable = variable, Shapiro_p_value = NA, Shapiro_is_normal = NA, KS_p_value = NA, KS_is_normal = NA)
  }
}
# ==================================================================================================
# 2.1. Define variables for normality test
# ==================================================================================================
VarNVTest <- c(
  "age", "years_since_diagnosis", "UPDRS_I_Score", "UPDRS_II_Score"
)

# ==================================================================================================
# 2.2.. Validate variable existence and numeric type
# ==================================================================================================
valid_vars <- VarNVTest[VarNVTest %in% names(df_safepd) & sapply(df_safepd[VarNVTest], is.numeric)]
if (length(valid_vars) < length(VarNVTest)) {
  warning("Some variables in VarNVTest are either missing or not numeric and will be excluded.")
}


# ==================================================================================================
# 3. Apply tests to validated variables
results_table <- map_df(valid_vars, ~test_normality(.x, df_safepd))
# ==================================================================================================

# ==================================================================================================
# 4. Save results and export as csv
# ==================================================================================================
results_dir <- "results"
if (!dir.exists(results_dir)) dir.create(results_dir)

write.csv(results_table, file.path(results_dir, "suppl.table1.normality_results.csv"), row.names = FALSE)

# ==================================================================================================
# 5.1. Create Q-Q plots for validated variables
# ==================================================================================================
qq_plots <- map(valid_vars, ~{
  ggplot(df_safepd, aes(sample = .data[[.x]])) +
    stat_qq() + 
    stat_qq_line() + 
    labs(title = paste("Q-Q Plot:", .x)) + 
    theme_minimal()
})

# ==================================================================================================
# 5.2. Arrange and export plots as pdf
# ==================================================================================================
pdf_file <- file.path(results_dir, "suppl.figure1.qq_plots_with_tests.pdf")
pdf(pdf_file, width = 8, height = 10)

grid.arrange(
  grobs = qq_plots,
  ncol = 1,
  top = textGrob("Q-Q Plots for Normality Assessment", gp = gpar(fontsize = 16, fontface = "bold"))
)

# ==================================================================================================
# 5.3. Add results table to PDF
# ==================================================================================================
grid.newpage()
grid.text("Normality Test Results", y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
results_grob <- tableGrob(results_table, rows = NULL)
grid.arrange(results_grob, ncol = 1)

dev.off()

message("Analysis complete. Results and plots saved to the 'results' directory.")