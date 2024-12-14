# generate and visualize aggregated frequency data and histograms for various health-related factors and safety questionnaire
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 2.2 # 2024-01-12, # added loops to downsize the codelength; added some comments/questions

# diagrams for age und years since diagnosis
# function visualizings data
create_histogram <- function(data, column, breaks, labels, xlab, ylab, title, ylim = NULL) {
  data[[paste0(column, "_cat")]] <- cut(data[[column]],
                                        breaks = breaks,
                                        labels = labels,
                                        right = FALSE,
                                        include.lowest = TRUE)
  hist_data <- table(data[[paste0(column, "_cat")]])
  ylim <- ylim %||% c(0, max(hist_data) + 10)  # Fallback für ylim
  bar_positions <- barplot(hist_data,
                           xlab = xlab,
                           ylab = ylab,
                           main = title,
                           col = "steelblue",
                           ylim = ylim,
                           space = 0)
  text(bar_positions, hist_data, labels = hist_data, pos = 3, cex = 0.8)
}
# apply function for age
create_histogram(df_safepd, "age", c(0, 45, seq(50, 85, by = 5), Inf),
                 c("<45", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">84"),
                 "Alter", "Häufigkeit", "Altersverteilung")

# apply function for years since diagnosis
create_histogram(df_safepd, "years_since_diagnosis", c(0, 1, 6, 11, 16, 21, 26, Inf),
                 c("<1", "1-5", "6-10", "11-15", "16-20", "21-25", ">25"),
                 "Krankheitsdauer (Jahre)", "Häufigkeit", "Verteilung der Krankheitsdauer", ylim = c(0, 80))


# diagrams for UPDRS Scores
hist_UPDRS_I <- hist(df_safepd$UPDRS_I_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="UPDRS Teil 1", col="steelblue", ylim=c(0,80), breaks=4)
text(hist_UPDRS_I$mids, hist_UPDRS_I$counts, labels = hist_UPDRS_I$counts, adj=c(0.5, -0.5))
hist_UPDRS_II <- hist(df_safepd$UPDRS_II_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="UPDRS Teil 2", col="steelblue", ylim=c(0,100), xlim=c(0,50), breaks=4)
text(hist_UPDRS_II$mids, hist_UPDRS_II$counts, labels = hist_UPDRS_II$counts, adj=c(0.5, -0.5))

# diagrams for visits of GP and neurologist
# function visualizing data
create_barplot <- function(data, column, title, max_category = 5) {
  data[[column]] <- ifelse(data[[column]] > max_category, max_category, data[[column]])
  categories <- c(as.character(0:(max_category - 1)), paste0(max_category, "+"))
  counts <- table(factor(data[[column]], levels = 0:max_category))
  bar_positions <- barplot(
    counts,
    names.arg = categories,
    xlab = "Anzahl der Besuche",
    ylab = "Häufigkeit",
    main = title,
    col = "steelblue",
    ylim = c(0, max(counts) + 10),
    space = 0
  )
  text(bar_positions, counts, labels = counts, pos = 3, cex = 0.8)
}

# apply function for GP
create_barplot(df_safepd, "FIMA_1_Hausarzt", "Anzahl der Konsultationen des Hausarztes")

# apply function for neurologist
create_barplot(df_safepd, "FIMA_1_Neurologe", "Anzahl der Konsultationen des Neurologen")

# diagrams for other data from FIMA
# function for visualizing data
generate_frequency_data <- function(df, variables, labels, group_labels) {
  data_list <- lapply(variables, function(var) {
    data.frame(
      Variable = var,
      Ja = sum(df[[var]] > 0, na.rm = TRUE),
      Nein = sum(df[[var]] == 0, na.rm = TRUE)
    )
  })
  data_combined <- do.call(rbind, data_list)
  data_combined$Variable <- factor(data_combined$Variable, levels = variables, labels = labels)
  pivot_longer(data_combined, cols = c("Ja", "Nein"), names_to = "Besuch", values_to = "Anzahl") %>%
    mutate(Gruppe = rep(group_labels, each = length(variables)))
}

# apply function for specialists
fachrichtungen <- c("FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe",
                    "FIMA_1_Orthopaede", "FIMA_1_Psychiater", "FIMA_1_other", "FIMA_1_Notfall_KH")
FA_Ja <- sapply(fachrichtungen, function(fach) sum(df_safepd[[fach]] > 0, na.rm = TRUE))
FA_Nein <- sapply(fachrichtungen, function(fach) sum(df_safepd[[fach]] == 0, na.rm = TRUE))
result <- data.frame(Fachrichtung = fachrichtungen, Ja = FA_Ja, Nein = FA_Nein)
arzt_data <- generate_frequency_data(df_safepd,
                                     variables = c("FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede","FIMA_1_Psychiater", "FIMA_1_other", "FIMA_1_Notfall_KH"),
                                     labels = c("Internist", "Gynäkologe", "Urologe","Orthopaede", "Psychiater", "anderer Facharzt", "Notfallambulanz"),
                                     group_labels = c("Ja", "Nein"))
ggplot(arzt_data, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Anzahl), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) +
  theme_minimal() +
  labs(title = "Häufigkeit der Konsultationen bei verschiedenen Fachärzten",
       x = "Fachrichtung / Einrichtung", y = "")
                 
# apply function for supportive therapies
therapie_data <- generate_frequency_data(df_safepd,
                                         variables = c("FIMA_2_Krankengymnastik", "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", "FIMA_2_Heilpraktiker", "FIMA_2_Chiropraktiker", "FIMA_2_Osteopath", "FIMA_2_Psychotherapeut"),
                                         labels = c("Krankengymnastik", "Ergotherapie", "Sprachtherapie", "Heilpraktiker", "Chiropraktiker", "Osteopath", "Psychotherapeut"),
                                         group_labels = c("Ja", "Nein"))
ggplot(therapie_data, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Anzahl), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(x = "Therapie", y = "Anzahl der Probanden", fill = "Besuch") +
  theme_minimal() + 
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) + 
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Therapien in Anspruch genommen?") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                 
# apply function for care-offers
pflege_data <- generate_frequency_data(df_safepd,
                                         variables = c("FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7"),
                                         labels = c("ambulanter Pflegedienst", "bezahlte Haushaltshilfe", "Verwandte und Bekannte", "teilstationäre Pflege", "vollstationäre Pflege"),
                                         group_labels = c("Ja", "Nein"))
ggplot(pflege_data, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Anzahl), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(x = "Therapie", y = "Anzahl der Probanden", fill = "Besuch") +
  theme_minimal() + 
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) + 
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Pflegeangebote in Anspruch genommen?") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                                 
# apply function for inpatient care
kh_data <- generate_frequency_data(df_safepd,
                                         variables = c("FIMA_9", "FIMA_10", "FIMA_11", "FIMA_12"),
                                         labels = c("Rehabilitation", "Tagesklinik", "somatische Klinik", "psychiatrische Klinik"),
                                         group_labels = c("Ja", "Nein"))
ggplot(kh_data, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Anzahl), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(x = "Therapie", y = "Anzahl der Probanden", fill = "Besuch") +
  theme_minimal() + 
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) + 
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Krankenhausaufenthalte in Anspruch genommen?") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# histogram for Pflegegrade
hist_Pflegegrade <- hist(df_safepd$FIMA_8, 
                         breaks = seq(-0.5, 5.5, by = 1),   
                         col = "steelblue",                 
                         main = "Häufigkeit der Pflegegrade",
                         xlab = "Ausprägung",               
                         ylab = "Häufigkeit",
                         plot = FALSE)                     
plot(hist_Pflegegrade, ylim = c(0, max(hist_Pflegegrade$counts) * 1.1), col = "steelblue", main = "Häufigkeit der Pflegegrade", xlab = "Ausprägung", ylab = "Häufigkeit",)
text(hist_Pflegegrade$mids, hist_Pflegegrade$counts, labels = hist_Pflegegrade$counts, pos = 3)
                 
# diagrams for data form safety questionnaire, grouped by similarity
# function visualizing data                 
aggregate_security_data <- function(df, variables, labels) {
  frequencies <- lapply(variables, function(var) {
    table(df[[var]]) %>%
      as.data.frame() %>%
      mutate(Kategorie = var)
  })
  combined <- bind_rows(frequencies) %>%
    pivot_wider(names_from = Var1, values_from = Freq, values_fill = 0) %>%
    mutate(
      restricted = rowSums(across(c("1", "2", "3"))),
      unrestricted = rowSums(across(c("4", "5")))
    )
  combined <- combined %>%
    pivot_longer(cols = c("restricted", "unrestricted"), names_to = "Gruppe", values_to = "Anzahl")
 
  combined$Kategorie <- factor(combined$Kategorie, levels = variables, labels = labels)
  combined
}

# define groups of influencing factors
krankheitsfaktoren_list <- list(
  krankheitsfaktoren1 = c("gait_insecurity_fall", "pain", "gastrointestinal_symptoms", "urinary_symptoms"),
  krankheitsfaktoren2 = c("side_effects_complications", "chaging_symptom_severity", "uncertain_future", "other_disease"),
  krankheitsfaktoren3 = c("mental_abilities", "mental_symptoms"),
  krankheitsfaktoren4 = c("loneliness", "not_at_peace_with_myself"),
  krankheitsfaktoren5 = c("everyday_problems", "daily_routine", "participation_in_road_traffic"),
  krankheitsfaktoren6 = c("family_role", "conflicts_with_relatives", "overload_among_people", "pejorativ_looks_comments"),
  krankheitsfaktoren7 = c("victim_to_crime", "financial_worries"),
  krankheitsfaktoren8 = c("access_healthcare", "nursing_care"),
  krankheitsfaktoren9 = c("lack_of_information", "communication_with_me", "communication_between_professionals"),
  krankheitsfaktoren_overall = c("overall_situation")
)

# renaming labels for representation in the plots
labels_list <- list(
  krankheitsfaktoren1 = c("Gangunsicherheit", "Schmerzen", "Magen-Darm-Symptome", "Harnsymptome"),
  krankheitsfaktoren2 = c("Nebenwirkungen & Komplikationen", "wechselnde Symptomeschwere", "ungewisse Zukunft", "andere Erkrankungen"),
  krankheitsfaktoren3 = c("Nachlassen geistiger Fähigkeiten", "psychiatrische Symptome"),
  krankheitsfaktoren4 = c("Einsamkeit", "Seelenfrieden"),
  krankheitsfaktoren5 = c("alltägliche Aktivitäten", "Unterbrechung täglicher Routinen", "Teilnahme am Straßenverkehr"),
  krankheitsfaktoren6 = c("Ausübung der Familienrolle", "Konflikte mit Verwandten", "Überforderung unter Menschen", "Stigmatisierung"),
  krankheitsfaktoren7 = c("Opfer von Kriminalität", "finanzielle Sorgen"),
  krankheitsfaktoren8 = c("Zugang zum Gesundheitswesen", "Probleme der pflegerischen Versorgung"),
  krankheitsfaktoren9 = c("Mangel an Informationen", "Kommunikationsprobleme mit dem Patienten", "Kommunikationsprobleme zwischen Fachpersonal"),
  krankheitsfaktoren_overall = c("Gesamtsituation")
)

# defining headlines for each plot
head_list <- list(
  krankheitsfaktoren1 = "Krankheit: Physische Symptome",
  krankheitsfaktoren2 = "Krankheit: Therapie & Progress",
  krankheitsfaktoren3 = "Krankheit: Psychische Symptome",
  krankheitsfaktoren4 = "Emotional",
  krankheitsfaktoren5 = "Sozial: Alltägliches Leben",
  krankheitsfaktoren6 = "Sozial: Soziale Interaktionen",
  krankheitsfaktoren7 = "Sozial: Soziale Risiken",
  krankheitsfaktoren8 = "Gesundheitssystem: Zugang",
  krankheitsfaktoren9 = "Gesundheitssystem: Kommunikation",
  krankheitsfaktoren_overall = "Gesamtsituation")

# loop generating plots for each group of influencing factos
plot_list <- list()  # empty list saving plots
for (i in 1:length(krankheitsfaktoren_list)) {
  krankheitsfaktoren <- krankheitsfaktoren_list[[i]]
  labels <- labels_list[[i]]
  data_phys <- aggregate_security_data(df_safepd, krankheitsfaktoren, labels)
  print(data_phys)
  plot <- ggplot(data_phys, aes(x = Kategorie, y = Anzahl, fill = Gruppe)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(x = "Krankheitsfaktor", y = "Anzahl", fill = "Gruppe") +
    ggtitle(head_list[[i]]) +
    scale_fill_manual(values = c("restricted" = "lightblue", "unrestricted" = "steelblue")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Anzahl),
        position = position_dodge(width = 0.8),
        vjust = -0.5, 
        size = 3,
        fontface = "bold")
  plot_list[[i]] <- plot   
}
for (plot in plot_list) {
  print(plot)
}
