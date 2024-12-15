# analyze and visualize normality and distribution of continous variables 
# Code developed by Florian Kuschel and David Pedrosa

# Version 2.2 # 2024-01-12, # updated the code with suggestions; added one questions to your comment on the UPDRS scores
# Version 2.3 # 2024-15-12, # added KS-Test to test for normality, indicating normaldistribution for age (and UPDRS I), as seen in Q-Q-Plot 

# Variables for normality test
VarNVTest <- c(
  "age", "years_since_diagnosis", "UPDRS_I_Score", "UPDRS_II_Score"
)

# Shapiro-Wilk normality tests
shapiro_results <- lapply(VarNVTest, function(variable) {
  if (is.numeric(df_safepd[[variable]])) {
    test <- shapiro.test(df_safepd[[variable]])
    data.frame(
      Variable = variable, 
      p_value = test$p.value, 
      is_normal = test$p.value > 0.05
    )
  } else {
    data.frame(Variable = variable, p_value = NA, is_normal = NA)
  }
})

# Kolmogorov-Smirnov normality tests
ks_results <- lapply(VarNVTest, function(variable) {
  if (is.numeric(df_safepd[[variable]])) {
    # Kolmogorov-Smirnov-Test (vergleicht mit einer Normalverteilung)
    test <- ks.test(df_safepd[[variable]], "pnorm", mean(df_safepd[[variable]]), sd(df_safepd[[variable]]))
    
    # Ergebnis als DataFrame speichern
    data.frame(
      Variable = variable, 
      p_value = test$p.value, 
      is_normal = test$p.value > 0.05
    )
  } else {
    data.frame(Variable = variable, p_value = NA, is_normal = NA)
  }
})

# Combine results for Shapiro-Wilk and Kolmogorov-Smirnov tests
shapiro_results_table <- do.call(rbind, shapiro_results)
ks_results_table <- do.call(rbind, ks_results)

# Define the path for saving the results
pdf_file <- file.path("results", "suppl.figure1.qq_plots_with_tests.pdf")

# Create Q-Q plots for selected variables
plot_age <- ggplot(df_safepd, aes(sample = age)) + 
  stat_qq() +  
  stat_qq_line() +  
  labs(title = "Q-Q Plot: Age") +  
  theme_minimal()

plot_updrs1 <- ggplot(df_safepd, aes(sample = UPDRS_I_Score)) + 
  stat_qq() +  
  stat_qq_line() +  
  labs(title = "Q-Q Plot: UPDRS I Score") +  
  theme_minimal()

plot_updrs2 <- ggplot(df_safepd, aes(sample = UPDRS_II_Score)) + 
  stat_qq() +  
  stat_qq_line() +  
  labs(title = "Q-Q Plot: UPDRS II Score") +  
  theme_minimal()

# Create tables for the results
shapiro_table_grob <- tableGrob(shapiro_results_table, rows = NULL)
ks_table_grob <- tableGrob(ks_results_table, rows = NULL)

# Open the PDF device
pdf(pdf_file, width = 8, height = 10)  

# Arrange plots and tables in a layout
# First, display the combined Q-Q plots
grid.arrange(
  plot_age, plot_updrs1, plot_updrs2, 
  ncol = 1, # Arrange the plots in one column
  heights = c(1, 1, 1) # Make sure each plot gets equal height
)

# Add text for Shapiro-Wilk table
grid.newpage()
grid.text("Shapiro-Wilk Normality Test Results", y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
grid.arrange(shapiro_table_grob, ncol = 1)

# Add text for Kolmogorov-Smirnov table
grid.newpage()
grid.text("Kolmogorov-Smirnov Normality Test Results", y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
grid.arrange(ks_table_grob, ncol = 1)

# Close the PDF device
dev.off()

# Save the results as CSV
write.csv(shapiro_results_table, file.path(wdir, "results", "suppl.table1.shapiro_results.csv"), row.names = FALSE)
write.csv(ks_results_table, file.path(wdir, "results", "suppl.table1.ks_results.csv"), row.names = FALSE)

message("Q-Q plots and results saved to ", pdf_file)
