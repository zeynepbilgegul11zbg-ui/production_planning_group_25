
library(shiny)

source("R/forecast_helpers.R", local = TRUE)
source("R/learning_curve.R", local = TRUE)
source("R/moving_average.R", local = TRUE)
source("R/weighted_ma.R", local = TRUE)
source("R/ses.R", local = TRUE)
source("R/seasonal_factors.R", local = TRUE)
source("R/holt.R", local = TRUE)
source("R/decomposition.R", local = TRUE)
source("R/winters.R", local = TRUE)
source("R/box_jenkins.R", local = TRUE)
source("R/chase_strategy.R", local = TRUE)
source("R/level_strategy.R", local = TRUE)
source("R/mixed_strategy.R", local = TRUE)
source("R/linear_programming.R", local = TRUE)
source("R/transportation_model.R", local = TRUE)
source("R/disaggregation.R", local = TRUE)

ui <- navbarPage(
  "Production Planning",
  
  tabPanel(
    "Learning Curve",
    learning_curve_ui("learning_curve")
  ),
  
  navbarMenu(
    "Forecasting",
    
    tabPanel(
      "Moving Average",
      moving_average_ui("moving_average")
    ),
    
    tabPanel(
      "Weighted Moving Average",
      weighted_ma_ui("weighted_ma")
    ),
    
    tabPanel(
      "Simple Exponential Smoothing",
      ses_ui("ses")
    ),
    
    tabPanel(
      "Seasonal Factors",
      seasonal_factors_ui("seasonal_factors")
    ),
    
    tabPanel(
      "Holt's Method",
      holt_ui("holt")
    ),
    
    tabPanel(
      "Seasonal Decomposition",
      decomposition_ui("decomposition")
    ),
    
    tabPanel(
      "Winters' Method",
      winters_ui("winters")
    ),
    
    tabPanel(
      "Box-Jenkins",
      box_jenkins_ui("box_jenkins")
    )
  ),
  navbarMenu(
    "Aggregate Planning",
    
    tabPanel(
      "Chase Strategy",
      chase_strategy_ui("chase_strategy")
    ),
    
    tabPanel(
      "Level Strategy",
      level_strategy_ui("level_strategy")
    ),
    
    tabPanel(
      "Mixed Strategy",
      mixed_strategy_ui("mixed_strategy")
    ),
    
    tabPanel(
      "Linear Programming",
      linear_programming_ui("linear_programming")
    ),
    
    tabPanel(
      "Transportation Model",
      transportation_model_ui("transportation_model")
    ),
    tabPanel(
      "Disaggregation",
      disaggregation_ui("disaggregation")
    )
  )
)

server <- function(input, output, session) {
  
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  weighted_ma_server("weighted_ma")
  ses_server("ses")
  seasonal_factors_server("seasonal_factors")
  holt_server("holt")
  decomposition_server("decomposition")
  winters_server("winters")
  box_jenkins_server("box_jenkins")
  chase_strategy_server("chase_strategy")
  level_strategy_server("level_strategy")
  mixed_strategy_server("mixed_strategy")
  linear_programming_server("linear_programming")
  transportation_model_server("transportation_model")
  disaggregation_server("disaggregation")
  
}

shinyApp(ui, server)
