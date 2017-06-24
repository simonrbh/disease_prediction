# Define UI

# setwd("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes")

shinyUI(fluidPage(
  titlePanel("Diabetes risk"),
  p("Prediction of diabetes (type 1 or 2) being recognised within 12 months"),
  plotOutput("diab_plot", height = "200px"),
  br(),
  p("Estimated probability, with 66% and 95% confidence ranges"),
  hr(),
  fluidRow(
    column(4,
           h4("Chronic conditions"),
           radioButtons("CHRONIC_INDICATOR_KEY", label = "Chronic condition present?", choiceNames = c("No", "Yes"), choiceValues = c("N", "Y"),
                        selected = "N"),
           br(),
           br(),
           radioButtons("HYPERTENSION_DURATION_DEG_ACTIVE", label = "Hypertension present?", choiceNames = c("No", "Yes"), choiceValues = c(-1, 1),
                        selected = -1),
           sliderInput("HYPERTENSION_DURATION_y", label = "Hypertension duration", min = -1, max = 10, value = -1),
           numericInput("HYPERTENSION_DRUGS_k", label = "Hypertension drug spend (R000 pa)", value = 0)
          ),
    column(4,
           br(),
           br(),
           radioButtons("CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE", label = "Chronic renal disease present?", choiceNames = c("No", "Yes"), 
                        choiceValues = c(-1, 1), selected = -1),
           br(),
           br(),
           radioButtons("HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE", label = "Hyperlipidaemia present?", choiceNames = c("No", "Yes"), 
                        choiceValues = c(-1, 1), selected = -1),    
           sliderInput("HYPERLIPIDAEMIA_DURATION_y", label = "Hyperlipidaemia duration", min = -1, max = 10, value = -1),
           numericInput("HYPERLIPIDAEMIA_DRUGS_k", label = "Hyperlipidaemia drug spend (R000 pa)", value = 0)
           ),
    column(4,
           h4("Demographics"),
           sliderInput("AGE", label = "Age", min = 0, max = 100, value = 40),
           selectInput("BMI", label = "Body Mass Index", choices = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"), 
                       selected = "Unknown"),
           sliderInput("VEM_SCORE_k", label = "Vitality Engagement (000 points pa)", min = -1, max = 200, value = -1),
           br(),
           h4("Health system usage"),
           numericInput("GP_VISITS", label = "GP visits pa", value = 0),     
           numericInput("AMT_CLAIMED_k", label = "Claims (R000 pa)", value = 0),
           numericInput("TOTAL_PAID_HCC_k", label = "Paid from risk (R000 pa)", value = 0)  
           )
  )
  
  # sidebarLayout(
  #   sidebarPanel(
  #     # sliderInput("TOTAL_PAID_HCC", label = "Risk Claims", min = 0, max = 100000, value = 0),
  #     # sliderInput("AMT_CLAIMED", label = "Amount claimed", min = 0, max = 100000, value = 0), 
  #     # sliderInput("PHARMACY_PAID_HCC", label = "Pharmacy paid from risk", min = 0, max = 10000, value = 0),
  #     # sliderInput("HYPERTENSION_DRUGS", label = "Hypertension drugs", min = 0, max = 10000, value = 0),
  #     # sliderInput("HIV_DRUGS", label = "HIV drugs", min = 0, max = 10000, value = 0),
  #     # sliderInput("HYPERLIPIDAEMIA_DRUGS", label = "Hyperlipidaemia drugs", min = 0, max = 10000, value = 0),
  #     # sliderInput("DIABETES_DRUGS", label = "Diabetes drugs", min = 0, max = 10000, value = 0),   
  #     # numericInput("GP_VISITS", label = "GP visits", value = 0),
  #     # numericInput("SPECIALIST_VISITS", label = "Specialist visits", value = 0),      
  #     # numericInput("EMERGENCY_VISITS", label = "Emergency visits", value = 0),
  #     # numericInput("EVENT_COUNT", label = "Event count", value = 0)
  #   
  #     numericInput("AGE", label = "Age", value = 40),  
  #     radioButtons("CHRONIC_INDICATOR_KEY", label = "Chronic condition present?", choiceNames = c("No", "Yes"), choiceValues = c("N", "Y"),
  #                  selected = "N"),
  #     radioButtons("CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE", label = "Chronic renal disease present?", choiceNames = c("No", "Yes"), 
  #                  choiceValues = c(-1, 1), selected = -1),
  #     
  #     radioButtons("HYPERTENSION_DURATION_DEG_ACTIVE", label = "Hypertension present?", choiceNames = c("No", "Yes"), choiceValues = c(-1, 1),
  #                  selected = -1),
  #     sliderInput("HYPERTENSION_DURATION_y", label = "Hypertension duration", min = -1, max = 10, value = -1),
  #     sliderInput("HYPERTENSION_DRUGS_k", label = "Hypertension drug spend (R000 pa)", min = 0, max = 100, value = 0),
  # 
  #     radioButtons("HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE", label = "Hyperlipidaemia present?", choiceNames = c("No", "Yes"), 
  #                  choiceValues = c(-1, 1), selected = -1),    
  #     sliderInput("HYPERLIPIDAEMIA_DURATION_y", label = "Hyperlipidaemia duration", min = -1, max = 10, value = -1),
  #     sliderInput("HYPERLIPIDAEMIA_DRUGS_k", label = "Hyperlipidaemia drug spend (R000 pa)", min = 0, max = 15, value = 0),
  # 
  #     radioButtons("BMI", label = "Body Mass Index", choiceNames = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"), 
  #                  choiceValues = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"), selected = "Unknown"),
  #     sliderInput("VEM_SCORE_k", label = "Vitality Engagement Measure (000 points pa)", min = -1, max = 200, value = -1),
  #     
  #     sliderInput("GP_VISITS", label = "GP visits pa", min = 0, max = 300, value = 0),     
  #     sliderInput("AMT_CLAIMED_k", label = "Claims in last year (R000 pa)", min = 0, max = 10000, value = 0),
  #     sliderInput("TOTAL_PAID_HCC_k", label = "Paid from risk last year (R000 pa)", min = 0, max = 10000, value = 0)
  #     ),
  #     
  #   mainPanel(
  #     p("Estimated probability of member developing diabetes (type 1 or 2) in the next year:"),
  #     plotOutput("diab_plot"),
  #     br(),
  #     br(),
  #     br(),
  #     br(),
  #     br(),
  #     img(src = "diabetes.png", width = 333, height = 252)
  #   )
  # )
  
))

