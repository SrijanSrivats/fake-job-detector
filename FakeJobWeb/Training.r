# fake_job_prediction_full.R
# Complete, fixed version of Fake Job Prediction pipeline using XGBoost + caret
# Requirements: install packages listed below (run once)
#install.packages(c("tm","tidyverse","caret","randomForest","e1071","xgboost","rpart","rpart.plot","pROC","ROSE","corrplot","ggplot2","ggthemes","stringr","SnowballC","wordcloud","textmineR","Matrix","kernlab"))

# Load libraries
library(tm)
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(rpart)
library(rpart.plot)
library(pROC)
library(ROSE)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(stringr)
library(SnowballC)
library(wordcloud)
library(textmineR)
library(Matrix)
library(kernlab)

set.seed(123)

# Create output directories
dir.create("visualizations", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)

data_path <- "fake_job_postings.csv"
if (!file.exists(data_path)) stop(paste("Dataset not found at", data_path))
data <- read.csv(data_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

cat("Dataset Dimensions:", dim(data), "\n")
cat("Column Names:", paste(names(data), collapse = ", "), "\n")

# Make sure column names are syntactically valid
names(data) <- make.names(names(data))

# Convert fraudulent to factor early (ensure it exists)
if (!"fraudulent" %in% names(data)) stop("Dataset must contain column named 'fraudulent'")
data$fraudulent <- as.integer(data$fraudulent) # ensure numeric 0/1
data$fraudulent[is.na(data$fraudulent)] <- 0
data$fraudulent <- factor(data$fraudulent, levels = c(0, 1), labels = c("No", "Yes"))

# Handle missing / blank text columns
text_cols <- c("title","location","department","company_profile","description",
               "requirements","benefits","employment_type","required_experience",
               "required_education","industry","function.")
for (col in intersect(text_cols, names(data))) {
  data[[col]][is.na(data[[col]])] <- ""
}

# Numeric binary fields: telecommuting, has_company_logo, has_questions
for (col in c("telecommuting","has_company_logo","has_questions")) {
  if (col %in% names(data)) {
    data[[col]] <- as.integer(replace(data[[col]], is.na(data[[col]]), 0))
  } else {
    data[[col]] <- 0L
  }
}

# Remove duplicates
data <- data[!duplicated(data), ]

# Add simple text-length features and keyword counts
data <- data %>%
  mutate(
    desc_length = nchar(description),
    req_length = nchar(requirements),
    benefits_length = nchar(benefits),
    company_profile_length = nchar(company_profile),
    title_length = nchar(title),
    # basic keyword signals
    salary_keywords = str_count(tolower(paste(description, requirements, benefits, sep = " ")),
                               "unpaid|volunteer|no salary|commission only|paid after"),
    urgency_keywords = str_count(tolower(paste(description, requirements, sep = " ")),
                                 "immediate|urgent|quick|start now|apply now|ASAP")
  )

# winsorize function for numeric columns
handle_outliers <- function(x) {
  if (!is.numeric(x)) return(x)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

numeric_cols_to_trim <- c("desc_length","req_length","benefits_length","company_profile_length","title_length")
for (col in numeric_cols_to_trim) {
  if (col %in% names(data)) data[[col]] <- handle_outliers(data[[col]])
}

# Text preprocessing helper
preprocess_text <- function(text) {
  if (is.na(text) || text == "") return("")
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("english"))
  text <- stripWhitespace(text)
  text <- trimws(text)
  return(text)
}

cat("Preprocessing text columns (this may take a moment)...\n")
data$description_clean <- vapply(data$description, preprocess_text, FUN.VALUE = "")
data$requirements_clean <- vapply(data$requirements, preprocess_text, FUN.VALUE = "")

# Function to create simple text-derived numeric features
create_text_features <- function(texts, prefix) {
  fake_keywords <- c("money","cash","quick","easy","guarantee","free",
                     "profit","income","rich","wealth","million","billion",
                     "immediate","urgent","commission")
  
  # Each vapply call should return a single integer per row → use integer(1)
  word_count <- vapply(texts, function(x) {
    if (is.na(x) || x == "") return(0L)
    w <- unlist(strsplit(x, "\\s+"))
    w <- w[w != ""]
    length(w)
  }, integer(1))
  
  char_count <- nchar(texts)
  avg_word_length <- ifelse(word_count > 0, char_count / word_count, 0)
  
  keyword_counts <- vapply(texts, function(txt) {
    if (is.na(txt) || txt == "") return(0L)
    sum(vapply(fake_keywords, function(k) str_count(txt, fixed(k)), integer(1)))
  }, integer(1))
  
  df <- data.frame(
    word_count = word_count,
    char_count = char_count,
    avg_word_length = avg_word_length,
    keyword_counts = keyword_counts
  )
  colnames(df) <- paste0(prefix, c("_word_count", "_char_count", "_avg_word_length", "_fake_keywords"))
  return(df)
}

desc_feats <- create_text_features(data$description_clean, "desc")
req_feats <- create_text_features(data$requirements_clean, "req")

# Combine features
essential_features <- data %>%
  transmute(
    telecommuting = telecommuting,
    has_company_logo = has_company_logo,
    has_questions = has_questions,
    desc_length = desc_length,
    req_length = req_length,
    benefits_length = benefits_length,
    company_profile_length = company_profile_length,
    title_length = title_length,
    salary_keywords = salary_keywords,
    urgency_keywords = urgency_keywords
  )

final_features <- cbind(essential_features, desc_feats, req_feats)
final_data <- as.data.frame(final_features)
final_data$fraudulent <- data$fraudulent  # ensure target exists in final_data

# Remove near-zero variance features if any
nzv <- nearZeroVar(final_data[, setdiff(names(final_data), "fraudulent")], saveMetrics = FALSE)
if (length(nzv) > 0) {
  warning("Removing near-zero variance columns: ", paste(names(final_data)[nzv], collapse = ", "))
  final_data <- final_data[, setdiff(names(final_data), names(final_data)[nzv]), drop = FALSE]
}


check_data_quality <- function(df, name) {
  cat("\n-- Data Quality Check:", name, "--\n")
  cat("Dimensions:", dim(df), "\n")
  cat("NA count:", sum(is.na(df)), "\n")
  cat("Target distribution:\n")
  print(table(df$fraudulent))
  print(round(prop.table(table(df$fraudulent)), 4))
}
check_data_quality(final_data, "Final features (before sampling)")

cat("\nApplying ROSE to balance classes...\n")
# ovun.sample wants formula; ensure fraudulent exists as factor in final_data
final_data$fraudulent <- factor(final_data$fraudulent, levels = c("No","Yes"))

balanced_obj <- ovun.sample(fraudulent ~ ., data = final_data, method = "both", p = 0.5, seed = 123)
final_data_balanced <- balanced_obj$data
check_data_quality(final_data_balanced, "After ROSE balancing")

p1 <- ggplot(final_data_balanced, aes(x = fraudulent, fill = fraudulent)) +
  geom_bar() + labs(title = "Class Distribution After ROSE", x = "Fraudulent", y = "Count") +
  theme_minimal()
ggsave("visualizations/class_distribution.png", p1, width = 7, height = 5)

set.seed(123)
train_index <- createDataPartition(final_data_balanced$fraudulent, p = 0.8, list = FALSE)
train_data <- final_data_balanced[train_index, , drop = FALSE]
test_data <- final_data_balanced[-train_index, , drop = FALSE]

cat("Train dimensions:", dim(train_data), "Test dimensions:", dim(test_data), "\n")

# Preprocess numeric predictors (center & scale) - exclude target
predictor_names <- setdiff(colnames(train_data), "fraudulent")
preproc <- preProcess(train_data[, predictor_names], method = c("center", "scale"))
train_scaled <- train_data
train_scaled[, predictor_names] <- predict(preproc, train_data[, predictor_names])
test_scaled <- test_data
test_scaled[, predictor_names] <- predict(preproc, test_data[, predictor_names])

# Ensure factors are correct for caret
train_scaled$fraudulent <- factor(train_scaled$fraudulent, levels = c("No", "Yes"))
test_scaled$fraudulent <- factor(test_scaled$fraudulent, levels = c("No", "Yes"))

saveRDS(preproc, "models/preproc_scaler.rds")
cat("Scaler saved to models/preproc_scaler.rds\n")

train_control <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# A small grid
xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

cat("Training XGBoost model (may take a minute)...\n")
set.seed(123)
xgb_model <- train(
  x = train_scaled[, predictor_names],
  y = train_scaled$fraudulent,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "ROC",
  verbosity = 0
)

# Save the model
saveRDS(xgb_model, file = "models/xgboost_model.rds")
cat("Model saved to models/xgboost_model.rds\n")

preds <- predict(xgb_model, test_scaled[, predictor_names])
probs <- predict(xgb_model, test_scaled[, predictor_names], type = "prob")

conf <- confusionMatrix(preds, test_scaled$fraudulent, positive = "Yes")
accuracy <- conf$overall["Accuracy"]
precision <- conf$byClass["Precision"]
recall <- conf$byClass["Recall"]
f1 <- conf$byClass["F1"]

# AUC
roc_obj <- roc(response = ifelse(test_scaled$fraudulent == "Yes", 1, 0), predictor = probs[, "Yes"])
auc_value <- as.numeric(auc(roc_obj))

cat("\n=== MODEL PERFORMANCE ===\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n")
cat("F1:", round(f1, 4), "\n")
cat("AUC:", round(auc_value, 4), "\n")
cat("Confusion Matrix:\n")
print(conf$table)

# Save performance table
results_summary <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC"),
  Value = c(round(as.numeric(accuracy), 4),
            round(as.numeric(precision), 4),
            round(as.numeric(recall), 4),
            round(as.numeric(f1), 4),
            round(auc_value, 4))
)
write.csv(results_summary, "visualizations/model_performance.csv", row.names = FALSE)
print(results_summary)

# ROC plot saved
png("visualizations/roc_curve.png", width = 800, height = 600)
plot(roc_obj, main = paste0("ROC Curve - XGBoost (AUC=", round(auc_value, 4), ")"))
dev.off()

# Feature importance from final model
if (!is.null(xgb_model$finalModel)) {
  importance_matrix <- xgb.importance(feature_names = predictor_names, model = xgb_model$finalModel)
  if (nrow(importance_matrix) > 0) {
    topN <- min(10, nrow(importance_matrix))
    top_features <- importance_matrix[1:topN, ]
    p_imp <- ggplot(top_features, aes(x=reorder(Feature, Gain), y=Gain)) +
      geom_col() + coord_flip() + labs(title = "XGBoost Feature Importance (Gain)", x = "Feature", y = "Gain") +
      theme_minimal()
    ggsave("visualizations/xgb_feature_importance.png", p_imp, width = 9, height = 6)
  }
}

cat("\n=== PIPELINE COMPLETED SUCCESSFULLY ===\n")
cat("Visualizations saved in 'visualizations/' and model saved in 'models/'.\n")

# ============================================================
#                🔄 BATCH PREDICTION (FULL DATASET)
# ============================================================

cat("\n=== Running Batch Prediction on full dataset ===\n")

# -------------------------------------------
# 1. Load the preprocessing scaler (saved earlier)
# -------------------------------------------
preproc_path <- "models/preproc_scaler.rds"
if (!file.exists(preproc_path)) {
  stop("❌ Missing scaler file: models/preproc_scaler.rds. 
       Please save using: saveRDS(preproc, 'models/preproc_scaler.rds')")
}
preproc <- readRDS(preproc_path)

cat("✅ Preprocessing scaler loaded.\n")


# -------------------------------------------
# 2. Load the original dataset again
# -------------------------------------------
batch_data <- read.csv("fake_job_postings.csv", 
                       stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Only keep frontend fields
frontend_fields <- c(
  "title", "description", "requirements", "benefits", "company_profile",
  "telecommuting", "has_company_logo", "has_questions"
)

# Add missing frontend fields if absent
for (f in frontend_fields) {
  if (!f %in% names(batch_data)) batch_data[[f]] <- ""
  batch_data[[f]][is.na(batch_data[[f]])] <- 
    ifelse(f %in% c("telecommuting","has_company_logo","has_questions"), 0, "")
}

batch_data$telecommuting    <- as.integer(batch_data$telecommuting)
batch_data$has_company_logo <- as.integer(batch_data$has_company_logo)
batch_data$has_questions    <- as.integer(batch_data$has_questions)


# -------------------------------------------
# 3. Apply SAME FEATURE ENGINEERING used in training
# -------------------------------------------

# prepare_features_batch() uses:
# preprocess_text()
# create_text_features()
# and your engineered fields
prepare_features_batch <- function(df) {

  df$description_clean  <- vapply(df$description, preprocess_text, FUN.VALUE = "")
  df$requirements_clean <- vapply(df$requirements, preprocess_text, FUN.VALUE = "")

  desc_feats <- create_text_features(df$description_clean, "desc")
  req_feats  <- create_text_features(df$requirements_clean, "req")

  essential <- df %>% mutate(
    desc_length             = nchar(description),
    req_length              = nchar(requirements),
    benefits_length         = nchar(benefits),
    company_profile_length  = nchar(company_profile),
    title_length            = nchar(title),
    salary_keywords = str_count(
      tolower(paste(description, requirements, benefits)),
      "unpaid|volunteer|no salary|commission only|paid after"
    ),
    urgency_keywords = str_count(
      tolower(paste(description, requirements)),
      "immediate|urgent|quick|start now|apply now|asap"
    )
  )

  cbind(
    essential,
    desc_feats,
    req_feats
  )
}

features_batch <- prepare_features_batch(batch_data)


# -------------------------------------------
# 4. APPLY SCALING USED IN TRAINING
# -------------------------------------------
features_scaled <- predict(preproc, features_batch)


# -------------------------------------------
# 5. MODEL PREDICTION
# -------------------------------------------
probs_batch <- predict(xgb_model, newdata = features_scaled, type = "prob")

# If caret returns numeric vector
if (is.numeric(probs_batch)) {
  probs_batch <- data.frame(No = 1 - probs_batch, Yes = probs_batch)
}

pred_batch <- ifelse(probs_batch$Yes >= 0.5, "Yes", "No")


# -------------------------------------------
# 6. CREATE CLEAN OUTPUT (ONLY FRONTEND FIELDS + PREDICTION)
# -------------------------------------------
output_df <- batch_data[ , frontend_fields ]
output_df$Prediction          <- pred_batch
output_df$Probability_Fake    <- round(probs_batch$Yes, 4)
output_df$Probability_Genuine <- round(probs_batch$No, 4)


# -------------------------------------------
# 7. SAVE CSV FILES
# -------------------------------------------
write.csv(output_df, "predictions_all_records.csv", row.names = FALSE)
write.csv(output_df[output_df$Prediction == "Yes",], 
          "predictions_fake_only.csv", row.names = FALSE)
write.csv(output_df[output_df$Prediction == "No",],  
          "predictions_genuine_only.csv", row.names = FALSE)

cat("\n🎉 Batch Prediction Completed!\n")
cat("📁 Saved: predictions_all_records.csv\n")
cat("📁 Saved: predictions_fake_only.csv\n")
cat("📁 Saved: predictions_genuine_only.csv\n")
