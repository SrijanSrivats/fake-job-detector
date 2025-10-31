# app.R
# Install missing packages if not already
#install.packages(c("shiny", "shinythemes", "DT", "ggplot2", "markdown"))

# Load them manually
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(markdown)
library(shiny)
library(shinythemes)  # load the newly installed package
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(markdown)

# Load the prediction functions from your training code
source("fake_job_prediction_full.R")

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS for styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      /* Background styling */
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        background-attachment: fixed;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* Main content container */
      .container-fluid {
        background: rgba(255, 255, 255, 0.95);
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
        margin-top: 20px;
        margin-bottom: 20px;
        padding: 30px;
      }
      
      /* Header styling */
      .main-header {
        text-align: center;
        color: #2c3e50;
        margin-bottom: 30px;
        padding-bottom: 20px;
        border-bottom: 3px solid #3498db;
      }
      
      .main-header h1 {
        font-weight: 700;
        font-size: 2.5em;
        margin-bottom: 10px;
      }
      
      .main-header h4 {
        color: #7f8c8d;
        font-weight: 300;
      }
      
      /* Card styling */
      .custom-card {
        background: white;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        padding: 25px;
        margin-bottom: 20px;
        border-left: 5px solid #3498db;
        transition: transform 0.3s ease;
      }
      
      .custom-card:hover {
        transform: translateY(-5px);
      }
      
      /* Result boxes */
      .result-genuine {
        background: linear-gradient(135deg, #2ecc71, #27ae60);
        color: white;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        font-size: 1.2em;
        font-weight: bold;
      }
      
      .result-fake {
        background: linear-gradient(135deg, #e74c3c, #c0392b);
        color: white;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        font-size: 1.2em;
        font-weight: bold;
      }
      
      /* Button styling */
      .btn-predict {
        background: linear-gradient(135deg, #3498db, #2980b9);
        color: white;
        border: none;
        padding: 12px 30px;
        font-size: 1.1em;
        border-radius: 25px;
        width: 100%;
        transition: all 0.3s ease;
      }
      
      .btn-predict:hover {
        transform: scale(1.05);
        background: linear-gradient(135deg, #2980b9, #2471a3);
      }
      
      /* Feature explanation */
      .feature-box {
        background: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin: 10px 0;
        border-radius: 5px;
      }
      
      /* Image gallery */
      .gallery-img {
        width: 100%;
        border-radius: 10px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        margin-bottom: 15px;
        transition: transform 0.3s ease;
      }
      
      .gallery-img:hover {
        transform: scale(1.02);
      }
      
      /* Suggestion messages */
      .suggestion-box {
        background: #fff3cd;
        border: 1px solid #ffeaa7;
        border-radius: 10px;
        padding: 15px;
        margin: 10px 0;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .container-fluid {
          margin: 10px;
          padding: 15px;
        }
        
        .main-header h1 {
          font-size: 2em;
        }
      }
      
      /* Navbar Styling - clean and consistent */
      .navbar {
        background-color: #ffffff !important; /* white navbar background */
        border-bottom: 3px solid #3498db !important;
      }
      
      .navbar-nav > li > a,
      .navbar-brand {
        color: #2c3e50 !important; /* dark text for contrast */
        font-weight: 600;
      }
      
      .navbar-nav > li > a:hover,
      .navbar-brand:hover {
        color: #3498db !important; /* blue on hover */
      }
      
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover,
      .navbar-nav > .active > a:focus {
        background-color: #3498db !important;
        color: #fff !important; /* white active tab */
      }

    "))
  ),
  
  # Main Application
  navbarPage(
    title = div(
      tags$i(class = "fas fa-search-dollar", style = "margin-right: 10px;"),
      "Fake Job Detector"
    ),
    id = "nav",
    collapsible = TRUE,
    
    # Tab 1: Prediction Interface
    tabPanel(
      "Job Prediction",
      icon = icon("search"),
      
      div(class = "container-fluid",
          div(class = "main-header",
              h1("Fake Job Post Classification"),
              h4("AI-Powered Detection of Fraudulent Job Listings")
          ),
          
          fluidRow(
            # Left Column - Input Form
            column(6,
                   div(class = "custom-card",
                       h3(tags$i(class = "fas fa-edit"), "Job Details Input"),
                       p("Enter the job post details below to analyze if it's genuine or fake."),
                       
                       textInput("title", "Job Title:", placeholder = "e.g., Data Entry Specialist - Work From Home"),
                       textAreaInput("description", "Job Description:", 
                                     placeholder = "Paste the complete job description here...",
                                     rows = 5),
                       textAreaInput("requirements", "Requirements:", 
                                     placeholder = "List job requirements and qualifications...",
                                     rows = 4),
                       textAreaInput("benefits", "Benefits:", 
                                     placeholder = "Salary, benefits, perks...",
                                     rows = 3),
                       textAreaInput("company_profile", "Company Profile:", 
                                     placeholder = "About the company...",
                                     rows = 3),
                       
                       fluidRow(
                         column(4,
                                numericInput("telecommuting", "Telecommuting (0/1):", 
                                           value = 0, min = 0, max = 1)
                         ),
                         column(4,
                                numericInput("has_company_logo", "Company Logo (0/1):", 
                                           value = 1, min = 0, max = 1)
                         ),
                         column(4,
                                numericInput("has_questions", "Has Questions (0/1):", 
                                           value = 0, min = 0, max = 1)
                         )
                       ),
                       
                       actionButton("predict", "Analyze Job Post", class = "btn-predict")
                   )
            ),
            
            # Right Column - Results and Information
            column(6,
                   # Prediction Results
                   uiOutput("predictionResult"),
                   
                   # Probability Visualization
                   plotOutput("probabilityPlot", height = "200px"),
                   
                   # Feature Importance
                   div(class = "custom-card",
                       h4(tags$i(class = "fas fa-chart-bar"), "Key Detection Factors"),
                       uiOutput("featureExplanation")
                   ),
                   
                   # Suggestions
                   div(class = "suggestion-box",
                       h4(tags$i(class = "fas fa-lightbulb"), "Safety Tips"),
                       tags$ul(
                         tags$li("Always verify company details through official websites"),
                         tags$li("Be cautious of jobs offering unusually high pay for simple work"),
                         tags$li("Never pay money to apply for a job"),
                         tags$li("Check for professional communication and proper grammar"),
                         tags$li("Look for detailed job requirements and company information")
                       )
                   )
            )
          )
      )
    ),
    
    # Tab 2: Project Information
    tabPanel(
      "About Project",
      icon = icon("info-circle"),
      
      div(class = "container-fluid",
          div(class = "main-header",
              h1("About Fake Job Detection"),
              h4("Understanding the Need and Methodology")
          ),
          
          fluidRow(
            column(8, offset = 2,
                   div(class = "custom-card",
                       h3(tags$i(class = "fas fa-exclamation-triangle"), "The Problem"),
                       p("Fake job postings are a growing concern in online job markets, affecting millions of job seekers worldwide. These fraudulent listings can lead to:"),
                       tags$ul(
                         tags$li("Identity theft through stolen personal information"),
                         tags$li("Financial scams requiring upfront payments"),
                         tags$li("Time wasted on fake application processes"),
                         tags$li("Emotional distress and false hopes")
                       ),
                       
                       h3(tags$i(class = "fas fa-brain"), "Our Solution"),
                       p("This application uses Machine Learning and Natural Language Processing to analyze job postings and identify potential fraud indicators:"),
                       
                       div(class = "feature-box",
                           h5("Text Analysis"),
                           p("Analyzes language patterns, keywords, and writing style commonly found in fake job posts")
                       ),
                       
                       div(class = "feature-box",
                           h5("Feature Engineering"),
                           p("Examines multiple factors including job description length, urgency indicators, and company details")
                       ),
                       
                       div(class = "feature-box",
                           h5("XGBoost Algorithm"),
                           p("Uses advanced machine learning to make accurate predictions based on trained patterns")
                       ),
                       
                       h3(tags$i(class = "fas fa-shield-alt"), "Key Features Analyzed"),
                       fluidRow(
                         column(6,
                                tags$ul(
                                  tags$li("Job description complexity"),
                                  tags$li("Urgency indicators"),
                                  tags$li("Salary mentions"),
                                  tags$li("Company profile completeness")
                                )
                         ),
                         column(6,
                                tags$ul(
                                  tags$li("Telecommuting flags"),
                                  tags$li("Presence of company logo"),
                                  tags$li("Application questions"),
                                  tags$li("Keyword patterns")
                                )
                         )
                       )
                   )
            )
          )
      )
    ),
    
    # Tab 3: Model Visualizations
    tabPanel(
      "Model Insights",
      icon = icon("chart-line"),
      
      div(class = "container-fluid",
          div(class = "main-header",
              h1("Model Performance & Visualizations"),
              h4("Training Results and Analysis")
          ),
          
          fluidRow(
            column(12,
                   div(class = "custom-card",
                       h3(tags$i(class = "fas fa-images"), "Training Visualizations"),
                       p("Below are the visualizations generated during model training:"),
                       
                       # Dynamic image loading from visualizations folder
                       uiOutput("imageGallery")
                   )
            )
          )
      )
    ),
    
    # Tab 4: Example Analysis
    tabPanel(
      "Examples",
      icon = icon("list-check"),
      
      div(class = "container-fluid",
          div(class = "main-header",
              h1("Example Job Posts"),
              h4("See how the model analyzes different job descriptions")
          ),
          
          fluidRow(
            column(6,
                   div(class = "custom-card",
                       h4("🚩 Likely Fake Job Post"),
                       tags$blockquote(
                         "URGENT HIRING! Work from home and earn $5000 weekly! No experience needed. 
                         Immediate start. Quick money guaranteed. Just sign up and start earning today. 
                         Limited positions available!"
                       ),
                       actionButton("load_fake", "Load This Example", class = "btn-predict")
                   )
            ),
            column(6,
                   div(class = "custom-card",
                       h4("✅ Likely Genuine Job Post"),
                       tags$blockquote(
                         "We are seeking a qualified Data Analyst with 2+ years experience. 
                         Responsibilities include data analysis, reporting, and visualization. 
                         Competitive salary with benefits. Bachelor's degree in related field required."
                       ),
                       actionButton("load_genuine", "Load This Example", class = "btn-predict")
                   )
            )
          )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Load model and prediction functions
  model_loaded <- reactiveVal(FALSE)
  
  observe({
    tryCatch({
      # This will load the model when the app starts
      if (file.exists("models/xgboost_model.rds")) {
        cat("Model loaded successfully!\n")
        model_loaded(TRUE)
      } else {
        showNotification("Model file not found. Please ensure the model is trained first.", 
                        type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error loading model:", e$message), type = "error")
    })
  })
  
  # Prediction results
  prediction_data <- reactiveVal()
  
  # Make prediction when button is clicked
  observeEvent(input$predict, {
    req(input$description)
    
    # Create job post object
    job_post <- list(
      title = input$title,
      description = input$description,
      requirements = input$requirements,
      benefits = input$benefits,
      company_profile = input$company_profile,
      telecommuting = as.numeric(input$telecommuting),
      has_company_logo = as.numeric(input$has_company_logo),
      has_questions = as.numeric(input$has_questions)
    )
    
    tryCatch({
      # Call prediction function
      result <- predict_fake_job(job_post)
      prediction_data(result)
      
      # Show notification
      if (result$Prediction == "Yes") {
        showNotification("⚠️ This job post appears to be FAKE. Please verify carefully.", 
                        type = "warning", duration = 10)
      } else {
        showNotification("✅ This job post appears to be GENUINE.", 
                        type = "message", duration = 10)
      }
      
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
    })
  })
  
  # Display prediction results
  output$predictionResult <- renderUI({
    result <- prediction_data()
    if (is.null(result)) {
      return(
        div(class = "custom-card",
            h4(tags$i(class = "fas fa-clock"), "Awaiting Analysis"),
            p("Enter job details and click 'Analyze Job Post' to get started.")
        )
      )
    }
    
    if (result$Prediction == "Yes") {
      div(class = "result-fake",
          tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 2em; margin-bottom: 10px;"),
          h3("LIKELY FAKE JOB POST"),
          p(result$Message),
          p(paste("Confidence:", round(result$Probability_Fake * 100, 1), "%"))
      )
    } else {
      div(class = "result-genuine",
          tags$i(class = "fas fa-check-circle", style = "font-size: 2em; margin-bottom: 10px;"),
          h3("LIKELY GENUINE JOB POST"),
          p(result$Message),
          p(paste("Confidence:", round(result$Probability_Genuine * 100, 1), "%"))
      )
    }
  })
  
  # Probability plot
  output$probabilityPlot <- renderPlot({
    result <- prediction_data()
    if (is.null(result)) return(NULL)
    
    prob_data <- data.frame(
      Category = c("Genuine", "Fake"),
      Probability = c(result$Probability_Genuine, result$Probability_Fake)
    )
    
    ggplot(prob_data, aes(x = Category, y = Probability, fill = Category)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0(round(Probability * 100, 1), "%")), 
                vjust = -0.5, size = 6, fontface = "bold") +
      scale_fill_manual(values = c("Genuine" = "#2ecc71", "Fake" = "#e74c3c")) +
      ylim(0, 1) +
      labs(title = "Prediction Confidence",
           y = "Probability") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  })
  
  # Feature explanation
  output$featureExplanation <- renderUI({
    result <- prediction_data()
    if (is.null(result)) {
      return(
        tags$ul(
          tags$li("Language patterns and keyword analysis"),
          tags$li("Job description complexity and length"),
          tags$li("Company information completeness"),
          tags$li("Urgency indicators and payment mentions")
        )
      )
    }
    
    # Simple feature insights based on prediction
    if (result$Prediction == "Yes") {
      tags$ul(
        tags$li("🚩 High urgency keywords detected"),
        tags$li("🚩 Unusual payment structure mentions"),
        tags$li("🚩 Lack of specific requirements"),
        tags$li("🚩 Suspicious language patterns")
      )
    } else {
      tags$ul(
        tags$li("✅ Professional language used"),
        tags$li("✅ Detailed job requirements"),
        tags$li("✅ Appropriate company information"),
        tags$li("✅ Realistic job expectations")
      )
    }
  })
  
  # Image gallery for visualizations
  output$imageGallery <- renderUI({
    vis_folder <- "visualizations"
    if (!dir.exists(vis_folder)) {
      return(p("Visualizations folder not found. Please run the training script first."))
    }
    
    image_files <- list.files(vis_folder, pattern = "\\.(png|jpg|jpeg)$", full.names = FALSE)
    
    if (length(image_files) == 0) {
      return(p("No visualization images found in the folder."))
    }
    
    # Create a fluid row with images
    tagList(
      lapply(image_files, function(img) {
        column(6,
               div(style = "text-align: center;",
                   h5(tools::file_path_sans_ext(img)),
                   tags$img(src = file.path(vis_folder, img), 
                          class = "gallery-img",
                          style = "max-width: 100%; height: auto;")
               )
        )
      })
    )
  })
  
  # Load example fake job
  observeEvent(input$load_fake, {
    updateTextInput(session, "title", value = "URGENT WORK FROM HOME - EARN $5000 WEEKLY")
    updateTextAreaInput(session, "description", 
                       value = "Immediate hiring! Work from home and earn unlimited income. No experience required. Quick start. High payments guaranteed. Contact us now to get started with this amazing opportunity. Limited positions available!")
    updateTextAreaInput(session, "requirements", 
                       value = "Basic computer knowledge. Internet connection. No specific qualifications needed.")
    updateTextAreaInput(session, "benefits", 
                       value = "Quick payments. High earnings. Flexible schedule.")
    updateTextAreaInput(session, "company_profile", 
                       value = "We are a fast-growing online company.")
    updateNumericInput(session, "telecommuting", value = 1)
    updateNumericInput(session, "has_company_logo", value = 0)
    updateNumericInput(session, "has_questions", value = 0)
  })
  
  # Load example genuine job
  observeEvent(input$load_genuine, {
    updateTextInput(session, "title", value = "Data Analyst - Full Time Position")
    updateTextAreaInput(session, "description", 
                       value = "We are seeking an experienced Data Analyst to join our team. The ideal candidate will have strong analytical skills and experience with data visualization tools. Responsibilities include data analysis, report generation, and collaborating with cross-functional teams.")
    updateTextAreaInput(session, "requirements", 
                       value = "Bachelor's degree in Computer Science, Statistics, or related field. 2+ years of experience in data analysis. Proficiency in SQL and data visualization tools. Strong analytical and problem-solving skills.")
    updateTextAreaInput(session, "benefits", 
                       value = "Competitive salary, health insurance, retirement plan, professional development opportunities, flexible work arrangements.")
    updateTextAreaInput(session, "company_profile", 
                       value = "We are a well-established technology company with over 10 years in the industry, serving clients worldwide with innovative solutions.")
    updateNumericInput(session, "telecommuting", value = 0)
    updateNumericInput(session, "has_company_logo", value = 1)
    updateNumericInput(session, "has_questions", value = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)