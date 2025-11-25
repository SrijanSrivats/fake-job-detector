###############################################################
# Fake Job Detection - INFERENCE + BATCH PREDICTION SCRIPT
# This MUST be sourced inside Shiny: source("fake_job_prediction_full.R")
###############################################################

library(tm)
library(stringr)
library(caret)
library(xgboost)
library(tidyverse)

# ------------------------------------------------------------
# LOAD MODEL + SCALER
# ------------------------------------------------------------
model_path <- "models/xgboost_model.rds"
scaler_path <- "models/preproc_scaler.rds"

if (!file.exists(model_path)) stop("❌ Model file missing.")
if (!file.exists(scaler_path)) stop("❌ Scaler file missing; training must save it.")

xgb_model <- readRDS(model_path)
preproc   <- readRDS(scaler_path)

cat("✅ Model and scaler loaded successfully!\n")

# ------------------------------------------------------------
# TEXT PREPROCESSING
# ------------------------------------------------------------
preprocess_text <- function(text) {
    if (is.na(text) || text == "") return("")
    text <- tolower(text)
    text <- removePunctuation(text)
    text <- removeNumbers(text)
    text <- removeWords(text, stopwords("english"))
    text <- stripWhitespace(text)
    trimws(text)
}

# ------------------------------------------------------------
# TEXT FEATURE ENGINEERING
# ------------------------------------------------------------
create_text_features <- function(texts, prefix) {
  fake_keywords <- c("money","cash","quick","easy","guarantee","free",
                     "profit","income","rich","wealth","million","billion",
                     "immediate","urgent","commission")
  
  word_count <- vapply(texts, function(x){
    if (x=="") return(0L)
    length(unlist(strsplit(x,"\\s+")))
  }, integer(1))
  
  char_count <- nchar(texts)
  avg_word_length <- ifelse(word_count > 0, char_count/word_count, 0)
  
  keyword_counts <- vapply(texts, function(txt){
    if (txt=="") return(0L)
    sum(vapply(fake_keywords,
               function(k) str_count(txt, fixed(k)),
               integer(1)))
  }, integer(1))
  
  # Build normally
  df <- data.frame(
    word_count      = word_count,
    char_count      = char_count,
    avg_word_length = avg_word_length,
    fake_keywords   = keyword_counts,
    check.names = FALSE
  )
  
  # Rename columns with prefix
  colnames(df) <- paste0(prefix, "_", colnames(df))
  
  return(df)
}


# ------------------------------------------------------------
# FULL FEATURE ENGINEERING PIPELINE
# ------------------------------------------------------------
prepare_features_batch <- function(df) {

    df$desc_clean  <- vapply(df$description, preprocess_text, FUN.VALUE = "")
    df$req_clean   <- vapply(df$requirements, preprocess_text, FUN.VALUE = "")

    desc_feats <- create_text_features(df$desc_clean, "desc")
    req_feats  <- create_text_features(df$req_clean,  "req")

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

    cbind(essential, desc_feats, req_feats)
}

# ------------------------------------------------------------
# SINGLE PREDICTION
# ------------------------------------------------------------
predict_fake_job <- function(job_post) {

    df <- as.data.frame(job_post, stringsAsFactors = FALSE)

    # Feature engineering
    feats <- prepare_features_batch(df)

    # 🔥 SCALE using training scaler
    feats_scaled <- predict(preproc, feats)

    # Predict
    probs <- predict(xgb_model, newdata = feats_scaled, type = "prob")

    if (is.numeric(probs)) {
        probs <- data.frame(No = 1 - probs, Yes = probs)
    }

    pred <- ifelse(probs$Yes >= 0.5, "Yes", "No")

    data.frame(
        Prediction         = pred,
        Probability_Fake   = round(probs$Yes, 4),
        Probability_Genuine= round(probs$No, 4),
        Message = ifelse(pred == "Yes",
                         "⚠️ Fake job suspected",
                         "✅ Job appears genuine"),
        stringsAsFactors = FALSE
    )
}

# ------------------------------------------------------------
# BATCH PREDICTION FUNCTION (Used for dataset)
# ------------------------------------------------------------
batch_predict <- function(filepath = "fake_job_postings.csv") {

    df <- read.csv(filepath, stringsAsFactors = FALSE, na.strings = c("", "NA"))

    frontend_fields <- c(
        "title","description","requirements","benefits","company_profile",
        "telecommuting","has_company_logo","has_questions"
    )

    # guarantee presence
    for (f in frontend_fields) {
        if (!f %in% names(df)) df[[f]] <- ""
        df[[f]][is.na(df[[f]])] <- ifelse(
            f %in% c("telecommuting","has_company_logo","has_questions"),
            0, ""
        )
    }

    df$telecommuting    <- as.integer(df$telecommuting)
    df$has_company_logo <- as.integer(df$has_company_logo)
    df$has_questions    <- as.integer(df$has_questions)

    engineered <- prepare_features_batch(df)
    scaled     <- predict(preproc, engineered)

    probs <- predict(xgb_model, newdata = scaled, type = "prob")

    if (is.numeric(probs)) {
        probs <- data.frame(No = 1 - probs, Yes = probs)
    }

    pred <- ifelse(probs$Yes >= 0.5, "Yes", "No")

    output <- df[ , frontend_fields]
    output$Prediction          <- pred
    output$Probability_Fake    <- round(probs$Yes, 4)
    output$Probability_Genuine <- round(probs$No, 4)

    write.csv(output, "predictions_all_records.csv", row.names = FALSE)
    write.csv(output[output$Prediction=="Yes",], "predictions_fake_only.csv", row.names = FALSE)
    write.csv(output[output$Prediction=="No",],  "predictions_genuine_only.csv", row.names = FALSE)

    output
}

# ------------------------------------------------------------
# END OF INFERENCE FILE
# ------------------------------------------------------------
