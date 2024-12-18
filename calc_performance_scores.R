# Load the data

rankscorefun <- function(pmeasures,
                               is_normalized = TRUE){
  Performance_measures <- c("AUC_Class_1", "AUC_Class_3",
                            "LR_positive_class_1", "LR_positive_class_3",
                            "LR_negative_class_1", "LR_negative_class_3",
                            "Pairwise_classification_accuracy","Classification_error")
  Performance_levels <- c("Excellent_performance", 
                          "Good_performance",
                          "Minimally_acceptable_performance",
                          "Not_acceptabel")
  
  # A dataframe for the weights of performance measures
  pm_df <- data.frame(
    Performance_measure = Performance_measures,
    Weight = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 3.0, 3.0)
  )
  
  # A dataframe for the weights of performance levels
  pl_df <- data.frame(
    Performance_level = Performance_levels,
    Weight = c(3.0, 2.0, 1.0, 0.0)
  )
  if (is_normalized) {
    pm_df$Weight <- pm_df$Weight / sum(pm_df$Weight)
    pl_df$Weight <- pl_df$Weight / max(pl_df$Weight)
  }
  # pl_df$Weight[4] <- NA # Not acceptable performance is removed
  
  # Initialize the performance dataframe for the model
  model_df <- data.frame(
    Performance_measure = Performance_measures,
    Performance_level_weight = c(NA, NA, NA, NA, NA, NA, NA, NA)
  )
  
  # Check the performance level for each performance measure 
  # by looking at the 95% confidence interval that match the CiPA's criteria
  
  # AUC_Class_1
  score_to_check <- pmeasures$AUC_Class_1
  if (score_to_check < 0.7) {
    model_df[model_df$Performance_measure == "AUC_Class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check >= 0.7 & score_to_check < 0.8) {
    model_df[model_df$Performance_measure == "AUC_Class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check >= 0.8 & score_to_check < 0.9) {
    model_df[model_df$Performance_measure == "AUC_Class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "AUC_Class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # AUC_Class_3
  score_to_check <- pmeasures$AUC_Class_3
  if (score_to_check < 0.7) {
    model_df[model_df$Performance_measure == "AUC_Class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check >= 0.7 & score_to_check < 0.8) {
    model_df[model_df$Performance_measure == "AUC_Class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check >= 0.8 & score_to_check < 0.9) {
    model_df[model_df$Performance_measure == "AUC_Class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "AUC_Class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # LR_positive_class_1
  score_to_check <- pmeasures$LR_positive_class_1
  if (score_to_check < 2.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check >= 2.0 & score_to_check < 5.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check >= 5.0 & score_to_check < 10.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "LR_positive_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # LR_positive_class_3
  score_to_check <- pmeasures$LR_positive_class_3
  if (score_to_check < 2.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check >= 2.0 & score_to_check < 5.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check >= 5.0 & score_to_check < 10.0) {
    model_df[model_df$Performance_measure == "LR_positive_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "LR_positive_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # LR_negative_class_1
  score_to_check <- pmeasures$LR_negative_class_1
  if (score_to_check > 0.5) {
    model_df[model_df$Performance_measure == "LR_negative_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check <= 0.5 & score_to_check > 0.2) {
    model_df[model_df$Performance_measure == "LR_negative_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check <= 0.2 & score_to_check > 0.1) {
    model_df[model_df$Performance_measure == "LR_negative_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "LR_negative_class_1",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # LR_negative_class_3
  score_to_check <- pmeasures$LR_negative_class_3
  if (score_to_check > 0.5) {
    model_df[model_df$Performance_measure == "LR_negative_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check <= 0.5 & score_to_check > 0.2) {
    model_df[model_df$Performance_measure == "LR_negative_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check <= 0.2 & score_to_check > 0.1) {
    model_df[model_df$Performance_measure == "LR_negative_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "LR_negative_class_3",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # Pairwise_classification_accuracy
  score_to_check <- pmeasures$Pairwise_classification_accuracy
  if (score_to_check < 0.7) {
    model_df[model_df$Performance_measure == "Pairwise_classification_accuracy",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check >= 0.7 & score_to_check < 0.8) {
    model_df[model_df$Performance_measure == "Pairwise_classification_accuracy",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check >= 0.8 & score_to_check < 0.9) {
    model_df[model_df$Performance_measure == "Pairwise_classification_accuracy",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "Pairwise_classification_accuracy",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # Classification_error
  score_to_check <- pmeasures$Classification_error
  if (score_to_check > 1.0) {
    model_df[model_df$Performance_measure == "Classification_error",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Not_acceptabel",]$Weight
  } else if (score_to_check <= 1.0 & score_to_check > 0.5) {
    model_df[model_df$Performance_measure == "Classification_error",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Minimally_acceptable_performance",]$Weight
  } else if (score_to_check <= 0.5 & score_to_check > 0.3) {
    model_df[model_df$Performance_measure == "Classification_error",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Good_performance",]$Weight
  } else {
    model_df[model_df$Performance_measure == "Classification_error",]$Performance_level_weight <- pl_df[pl_df$Performance_level == "Excellent_performance",]$Weight
  }
  
  # Calculate rank score
  rank_score <- model_df$Performance_level_weight %*% pm_df$Weight
  
  return(as.numeric(rank_score))
}
max_input <- 7
results <- data.frame()
for (i in 1:max_input) {
  data <- read.csv(paste0("Accepted_models_manual_weight_1/summary_",i,".csv"))
  # data <- na.omit(data)
  if (nrow(data)!=0) {
    data <- data[, !names(data) %in% c("Alpha_1", "Alpha_2")]
    for (j in 1:i) {
      colname <- paste0("Beta_",j)
      data[colname] <- NULL
      colname <- paste0("Mean_",j)
      data[colname] <- NULL
      colname <- paste0("SD_",j)
      data[colname] <- NULL
    }
    data$Input <- i
    data$Rank_score_rev <- NA
    for (j in 1:nrow(data)) {
      # print(c(i,j))
      if (is.na(data[j,1])) {
        data[-j,]
      } else {
        data[j,]$Rank_score_rev <- rankscorefun(data[j,])
      }
    }
    results <- rbind(results,data)
  }
}

# Save the new dataframe to a CSV file
write.csv(results, "summary_manual_weight_1.csv", row.names = FALSE)