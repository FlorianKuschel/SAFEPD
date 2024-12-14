# analyze and visualize normality and distribution of all variables
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 2.2 # 2024-01-12, # updated the code with suggestions; added one questions to your comment on the UPDRS scores

# Variables for normality test
VarNVTest <- c(
  "age", "gender", "nationality", "martial_status", "years_since_diagnosis",
  "persons_houshold", "school_graduation", "professional_graduation",
  "employment_status", "UPDRS_I_Score", "UPDRS_II_Score", "lack_of_information",
  "uncertain_future", "chaging_symptom_severity", "gait_insecurity_fall",
  "pain", "gastrointestinal_symptoms", "urinary_symptoms", "mental_abilities",
  "mental_symptoms", "other_disease", "nursing_care", "side_effects_complications",
  "access_healthcare", "communication_with_me", "communication_between_professionals",
  "loneliness", "everyday_problems", "daily_routine", "overload_among_people",
  "pejorativ_looks_comments", "family_role", "conflicts_with_relatives",
  "victim_to_crime", "financial_worries", "not_at_peace_with_myself",
  "participation_in_road_traffic", "overall_situation"
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

shapiro_results_table <- do.call(rbind, shapiro_results)
write.csv(shapiro_results_table, file.path(wdir, "results", "suppl.table1.shapiro_results.csv"), row.names = FALSE)

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

# Create an empty spacer plot for the 2x2 layout
empty_plot <- ggplot() +
  theme_void() +
  labs(title = "")

combined_plot <- (plot_age + plot_updrs1) /
                 (plot_updrs2 + empty_plot) +
  plot_annotation(
    title = "Q-Q Plots for Selected Variables"
  )

message("Normality for different scores in the population. This figure illustrates the Q-Q-plots for age (A), UPDRS I Score (B), and UPDRS II Score (C) in the dataset")

#TODO: why these scores?
  # I don't really understand the question.
  # We surveyed part 1 and 2 and I wanted to check the distribution, although this is actually already clear from the diagrams (right-skewed distribution).
  # If necessary, I could omit the test here.
  # I have added a comment to the sum score of parts 1 and 2 in SAFEPD_dich.R, whether this makes sense?
  # DP: Sorry, the question was rather why *these* scorese (here). You seem to make an arbitary choice here. Besides, there is some scepticism about the disadvantages of#
  #         the shapiro.wilk test, as it favors estimates indicating to reject H0 when there are outliers. In fact the qqplot for age (qqnorm(df_safepd$age)) looks quite normally
  # disttributed. I guess I would ditch this part, especially since a considerable number is categorial or ordinal and it doesn't make too much sense to test them.

# Save the combined plot as a PDF
pdf_file <- file.path("results", "suppl.figure1.qq_plots.pdf")
ggsave(pdf_file, plot = combined_plot, width = 8, height = 8)

message("Q-Q plots saved to ", pdf_file)
