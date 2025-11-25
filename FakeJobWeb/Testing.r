# ===============================================
# Fake Job Prediction - Model Loading and Inference (with Probabilities)
# ===============================================

# Required packages
library(tm)
library(stringr)
library(caret)
library(xgboost)

# Helper operator for defaults
`%||%` <- function(a, b) if (!is.null(a)) a else b

model_path <- "models/xgboost_model.rds"
if (!file.exists(model_path)) stop("❌ Model file not found. Make sure models/xgboost_model.rds exists.")
xgb_model <- readRDS(model_path)
cat("✅ Model loaded successfully!\n")

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

# Create text-derived numeric features
create_text_features <- function(texts, prefix) {
  fake_keywords <- c("money","cash","quick","easy","guarantee","free",
                     "profit","income","rich","wealth","million","billion",
                     "immediate","urgent","commission")
  
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

prepare_features <- function(job_post) {
  # Ensure text fields exist
  text_fields <- c("title","description","requirements","benefits","company_profile")
  for (f in text_fields) {
    if (!f %in% names(job_post)) job_post[[f]] <- ""
    if (is.na(job_post[[f]])) job_post[[f]] <- ""
  }
  
  # Numeric flags
  telecommuting <- as.integer(job_post$telecommuting %||% 0)
  has_company_logo <- as.integer(job_post$has_company_logo %||% 0)
  has_questions <- as.integer(job_post$has_questions %||% 0)
  
  # Preprocess text
  desc_clean <- preprocess_text(job_post$description)
  req_clean <- preprocess_text(job_post$requirements)
  
  # Feature engineering
  desc_feats <- create_text_features(desc_clean, "desc")
  req_feats <- create_text_features(req_clean, "req")
  
  # Basic numeric features
  essential <- data.frame(
    telecommuting = telecommuting,
    has_company_logo = has_company_logo,
    has_questions = has_questions,
    desc_length = nchar(job_post$description),
    req_length = nchar(job_post$requirements),
    benefits_length = nchar(job_post$benefits),
    company_profile_length = nchar(job_post$company_profile),
    title_length = nchar(job_post$title),
    salary_keywords = str_count(tolower(paste(job_post$description, job_post$requirements, job_post$benefits)), 
                                "unpaid|volunteer|no salary|commission only|paid after"),
    urgency_keywords = str_count(tolower(paste(job_post$description, job_post$requirements)), 
                                 "immediate|urgent|quick|start now|apply now|asap")
  )
  
  final <- cbind(essential, desc_feats, req_feats)
  return(final)
}

predict_fake_job <- function(job_post) {
  features <- prepare_features(job_post)
  
  # Predict probabilities using the model
  probs <- predict(xgb_model, newdata = features, type = "prob")
  
  # If output is numeric (some caret versions)
  if (is.numeric(probs)) {
    # When the model is not wrapped with caret's twoClassSummary
    probs <- data.frame(No = 1 - probs, Yes = probs)
  }
  
  # Determine class with highest probability
  prediction <- ifelse(probs$Yes >= 0.5, "Yes", "No")
  
  result <- data.frame(
    Prediction = prediction,
    Probability_Fake = round(probs$Yes, 4),
    Probability_Genuine = round(probs$No, 4),
    Message = ifelse(prediction == "Yes", 
                     "⚠️ This job post is likely FAKE.", 
                     "✅ This job post seems GENUINE.")
  )
  return(result)
}

example_job <- list(
  title = "Work from home data entry - earn Rs.5000 daily",
  description = "Urgent hiring! Immediate start. Earn money fast with simple typing work. No experience needed.",
  requirements = "Computer, internet connection. Apply now!",
  benefits = "Quick payments and flexible schedule.",
  company_profile = "We are an online freelancing company.",
  telecommuting = 1,
  has_company_logo = 0,
  has_questions = 0
)

cat("\n🔍 Predicting for example job post...\n")
result <- predict_fake_job(example_job)
print(result)
