# Fake Job Detector
Detect fraudulent job postings with a reproducible R pipeline and a Shiny web app.

![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-Posit-blue)
![XGBoost](https://img.shields.io/badge/Model-XGBoost-green)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![Status](https://img.shields.io/badge/status-on%20track-brightgreen)

> **Summary:** End-to-end system for fake job detection using the public *Fake Job Postings* dataset.  
> Trained XGBoost model, clean preprocessing, and a Shiny app for single-post predictions and (soon) batch scoring.

---

## ✨ Features
- **Shiny App**: Paste job details (title/description/requirements/benefits + metadata) → prediction + probability.
- **Trained Model**: `models/xgboost_model.rds` with strong metrics on a held-out set.
- **Visualizations**: ROC curve, feature importance, class distribution, metrics CSV under `visualizations/`.
- **Reproducibility**: `renv` lockfile planned; simple Dockerfile planned.
- **Roadmap (active)**:
  - Batch CSV scoring (upload → predict → download)
  - Per-prediction explanations + feature importance view
  - Threshold slider with live confusion matrix
  - Light load testing and deployment (shinyapps.io/Posit Connect / container)

> We are on track; we aim to finish and submit the presentation by **Nov 17, 2025**.

---

## 📦 Repository Structure

fake-job-detector/
├─ app.R # Shiny UI/server
├─ fake_job_prediction_training.R # Training pipeline
├─ fake_job_prediction_full.R # Inference utilities (predict_fake_job)
├─ data/
│ └─ fake_job_postings.csv # Dataset (or place it here)
├─ models/
│ └─ xgboost_model.rds # Trained model artifact
├─ visualizations/
│ ├─ roc_curve.png
│ ├─ xgb_feature_importance.png
│ ├─ class_distribution.png
│ └─ model_performance.csv
├─ www/
│ └─ styles.css # (optional) custom styles
├─ README.md
└─ (planned) renv.lock, Dockerfile
