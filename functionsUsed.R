# ==================================================================================================
# SAFEPD - Functions Script
# Developed by Florian Kuschel, Anna and David Pedrosa
# Version 1.0 (2025-01-19)
# Version 2.0 (2025-02-19): adapted line 96-98 with ROC-function, somehow it didnt work for me with the original code
# --------------------------------------------------------------------------------------------------
# This script defines reusable functions for estimating models, performing bootstrap CI calculations,
# printing AUC results, and comparing models using plots. It is designed to support "SAFEPD_regression.R."
# ==================================================================================================

# ==================================================================================================
# 1. Estimate Generalized Linear Models (GLM) using caret
# --------------------------------------------------------------------------------------------------
results_modelSAFETY <- function(method, data, train_control, tunegrid, test_data, model_name) {
  mdl_caret <- train(
    as.formula(paste("overall_situation_Group_factor ~ .")),
    data = data,
    method = method,
    preProcess = c("nzv", "center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneLength = 25,
    metric = "logLoss",
    tuneGrid = tunegrid
  )
  
  estimated_model <- data.frame(
    model_name = model_name,
    AUC = NA,
    LogLoss = NA,
    Accuracy = NA
  )
  
  predicted_classes <- predict(mdl_caret, newdata = test_data)
  mdl_caret_matrix <- confusionMatrix(
    predicted_classes,
    test_data$overall_situation_Group_factor,
    positive = "yes"
  )
  
  mdl_caret_prob <- predict(mdl_caret, newdata = test_data, type = "prob") %>%
    cbind(
      .,
      caret_results = predict(mdl_caret, test_data),
      overall_situation_Group_factor = test_data$overall_situation_Group_factor
    )
  
  trueLabelsBinary_full <- ifelse(mdl_caret_prob$overall_situation_Group_factor == "yes", 1, 0)
  predictedLabelsBinary <- ifelse(mdl_caret_prob$caret_results == "yes", 1, 0)
  
  estimated_model$LogLoss[1] <- LogLoss(mdl_caret_prob$yes, trueLabelsBinary_full)
  estimated_model$AUC[1] <- AUC(mdl_caret_prob$yes, trueLabelsBinary_full)
  estimated_model$Accuracy[1] <- mdl_caret_matrix$overall[[1]]
  
  return(list(mdl_caret, estimated_model, mdl_caret_matrix))
}

# ==================================================================================================
# 2. Modified Model Estimation with Configurable Target Variable
# --------------------------------------------------------------------------------------------------
results_modelSAFETYmod <- function(method, data, train_control, tunegrid = NULL, test_data, model_name,
                                   target = "overall_situation_Group_factor",
                                   preProcess = c("nzv", "center", "scale"),
                                   metric = "logLoss", family = "binomial") {
  formula <- as.formula(paste(target, "~ ."))
  
  mdl_caret <- train(
    formula,
    data = data,
    method = method,
    preProcess = preProcess,
    family = family,
    trControl = train_control,
    tuneLength = 25,
    metric = metric,
    tuneGrid = tunegrid
  )
  
  estimated_model <- data.frame(
    model_name = model_name,
    AUC = NA,
    LogLoss = NA,
    Accuracy = NA
  )
  
  predicted_classes <- predict(mdl_caret, newdata = test_data)
  mdl_caret_matrix <- confusionMatrix(predicted_classes, test_data[[target]], positive = "yes")
  
  mdl_caret_prob <- predict(mdl_caret, newdata = test_data, type = "prob") %>%
    cbind(., caret_results = predicted_classes, target = test_data[[target]])
  
  trueLabelsBinary_full <- as.numeric(mdl_caret_prob$target == "yes")
  predictedLabelsBinary <- as.numeric(mdl_caret_prob$caret_results == "yes")
  
  estimated_model$LogLoss[1] <- LogLoss(mdl_caret_prob$yes, trueLabelsBinary_full)
  roc_obj <- pROC::roc(trueLabelsBinary_full, mdl_caret_prob$yes)
  auc_value <- as.numeric(pROC::auc(roc_obj))
  estimated_model$AUC[1] <- auc_value
  estimated_model$Accuracy[1] <- mdl_caret_matrix$overall[[1]]
  
  return(list(model = mdl_caret, metrics = estimated_model, confusion_matrix = mdl_caret_matrix))
}

# ==================================================================================================
# 3. Print AUC for Estimated Models
# --------------------------------------------------------------------------------------------------
print_AUC <- function(mdl, test_data, annotation, subtitle) {
  options(yardstick.event_first = FALSE)  # Set the second level as success
  
  fig <- data.frame(
    pred = predict(mdl, newdata = test_data, type = "prob")$yes,
    obs = test_data$overall_situation_Group_factor
  ) %>%
    yardstick::roc_curve(obs, pred) %>%
    autoplot() +
    theme_bw() +
    labs(
      title = "Prediction of the model in the test dataset",
      subtitle = subtitle
    ) +
    geom_text(data = annotation, aes(x = x, y = y, label = label), color = "black", fontface = "bold") +
    coord_equal() +
    xlab("1 - specificity") + ylab("Sensitivity")
  
  return(fig)
}

# ==================================================================================================
# 4. Bootstrap Confidence Intervals for Estimated Models
# --------------------------------------------------------------------------------------------------
results_bootstrap <- function(method, data, test_data, model_name, nboot, predictor) {
  if (method == 'glmnet') {
    lambda.grid <- seq(0.0001, 1, length = 50)
    alpha.grid <- seq(0, 1, length = 11)
    grid_total <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)
    method_train <- "repeatedcv"
  } else {
    grid_total <- NULL
    method_train <- "none"
  }
  
  numpredictors <- 15
  est_boot <- NA
  tcBoot <- trainControl(
    method = method_train,
    summaryFunction = mnLogLoss,
    savePredictions = "final",
    verboseIter = FALSE,
    classProbs = TRUE
  )
  
  model_est_boot <- as.data.frame(matrix(data = NA, nrow = nboot, ncol = 6))
  colnames(model_est_boot) <- c('k', 'AUC', 'LogLoss', 'Accuracy', 'alpha', 'lambda')
  
  pb <- txtProgressBar(min = 0, max = nboot, style = 3)
  options(warn = -1)
  
  for (k in 1:nboot) {
    setTxtProgressBar(pb, k)
    ncoefs <- 0
    
    while (ncoefs != numpredictors) {
      model_est_boot[k, 1] <- k
      boot_idx <- sample(dim(data)[1], replace = TRUE)
      boot_data <- data[boot_idx, ]
      test_idx <- setdiff(1:dim(data)[1], boot_idx)
      test_data <- data[test_idx, ]
      
      mdl_boot <- if (predictor == 'overall_situation_Group_factor') {
        results_modelSAFETYmod(
          method = method, data = boot_data, train_control = tcBoot,
          tunegrid = grid_total, test_data = test_data, model_name = model_name
        )
      }
      
      ncoefs <- if (method == 'glmnet') {
        length(data.frame(as.matrix(coef(mdl_boot[[1]]$finalModel, mdl_boot[[1]]$bestTune$lambda)))$s1)
      } else {
        numpredictors
      }
    }
    
    if (method == 'glmnet') {
      model_est_boot[k, 2:4] <- mdl_boot[[2]][1, 2:4]
      model_est_boot[k, 5] <- mdl_boot[[1]]$bestTune$alpha
      model_est_boot[k, 6] <- mdl_boot[[1]]$bestTune$lambda
      
      if (k == 1) {
        temp_estimates <- c('(Intercept)', mdl_boot[[1]]$coefnames)
        est_boot <- as.data.frame(matrix(ncol = length(temp_estimates), nrow = nboot))
        colnames(est_boot) <- temp_estimates
      }
      est_boot[k, ] <- data.frame(as.matrix(coef(mdl_boot[[1]]$finalModel, mdl_boot[[1]]$bestTune$lambda)))$s1
    } else {
      model_est_boot[k, 2:4] <- mdl_boot[[2]][1, 2:4]
    }
  }
  
  options(warn = 0)
  close(pb)
  return(list(model_est_boot, est_boot))
}

# ==================================================================================================
# 5. Plot Model Comparison Including Confidence Intervals
# --------------------------------------------------------------------------------------------------
plot_results_withCI <- function(model1, model2, model3, dependent_variable, filename_save) {
  data2plotwCI <- bind_rows(model1[[1]], model2[[1]], model3[[1]], .id = "model_name") %>%
    mutate(model_name = dplyr::recode(as.character(model_name), "1" = "Full GLM", "2" = "Stepwise GLM", "3" = "ElasticNet regularization")) %>%
    dplyr::select(model_name, AUC, LogLoss, Accuracy) %>%
    group_by(model_name) %>%
    summarize_all(funs(mean, sd, se = sd(.) / sqrt(n())))
  
  WhiskerDataTemp <- data2plotwCI %>%
    dplyr::select(., c(model_name, matches("sd"), -matches("LogLoss"))) %>%
    pivot_longer(!model_name, names_to = "metric")
  
  AUC_Accuracy <- data2plotwCI %>%
    dplyr::select(., c(model_name, matches("mean"), -matches("LogLoss"))) %>%
    pivot_longer(!model_name, names_to = "metric") %>%
    add_column(val_whis = WhiskerDataTemp$value) %>%
    mutate(metric = dplyr::recode(metric, "AUC_mean" = "AUC", "Accuracy_mean" = "Accuracy"))
  
  p_comparison_models <- AUC_Accuracy %>%
    ggplot(aes(
      fill = factor(model_name, levels = c("Full GLM", "Stepwise GLM", "ElasticNet regularization")),
      y = value,
      x = metric
    )) +
    geom_bar(position = position_dodge(.9), stat = "identity") +
    geom_errorbar(aes(ymin = value - val_whis, ymax = value + val_whis), width = .1, position = position_dodge(.9)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      plot.caption = element_text(hjust = .7, face = "italic"),
      legend.title = element_text(hjust = .5, color = "black", size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.position = "none",
      plot.margin = margin(t = 10, unit = "pt")
    ) +
    ylim(0, max(select_if(AUC_Accuracy, is.numeric)) + .4) +
    scale_fill_brewer(palette = 1) +
    labs(y = "", x = "", fill = NULL, title = "", caption = "") +
    geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 5, position = position_dodge(width = 0.9))
  
  WhiskerDataTemp <- data2plotwCI %>%
    dplyr::select(., c(model_name, matches("LogLoss"))) %>%
    dplyr::select(., c(model_name, matches("sd"))) %>%
    pivot_longer(!model_name, names_to = "metric")
  
  LogLoss <- data2plotwCI %>%
    dplyr::select(., c(model_name, matches("LogLoss_mean"))) %>%
    pivot_longer(!model_name, names_to = "metric") %>%
    add_column(val_whis = WhiskerDataTemp$value) %>%
    mutate(metric = dplyr::recode(metric, "LogLoss_mean" = "logLoss"))
  
  p_comparison_models2 <- LogLoss %>%
    ggplot(aes(
      fill = factor(model_name, levels = c("Full GLM", "Stepwise GLM", "ElasticNet regularization")),
      y = value,
      x = metric
    )) +
    geom_bar(position = position_dodge(.9), stat = "identity") +
    geom_errorbar(aes(ymin = value - val_whis, ymax = value + val_whis), width = .1, position = position_dodge(.9)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      plot.caption = element_text(hjust = .7, face = "italic"),
      legend.title = element_text(hjust = .5, color = "black", size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.position = c(0.54, 0.9),
      plot.margin = margin(t = 10, unit = "pt")
    ) +
    ylim(0, max(select_if(LogLoss, is.numeric) * 2.5)) +
    scale_fill_brewer(palette = 1) +
    labs(y = "", x = "", fill = NULL, title = "", caption = "") +
    geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 5, position = position_dodge(width = 0.9))
  
  pdf(file = file.path(wdir, "results", filename_save))
  title_grid <- sprintf(
    "Comparing full model, stepwise reduced and \nregularised model for the predictor: \n%s",
    dependent_variable
  )
  grid.arrange(
    p_comparison_models, p_comparison_models2, nrow = 1,
    top = text_grob(title_grid, size = 16, face = "bold"),
    bottom = text_grob(
      label = "Higher values indicate better performance: Accuracy and AUC\nLower values indicate better performance: logLoss",
      size = 9, face = "italic", hjust = 0
    )
  )
  dev.off()
}
