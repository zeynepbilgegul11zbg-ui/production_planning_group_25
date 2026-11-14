
# =========================================================
# SIMPLE EXPONENTIAL SMOOTHING (SES)
# =========================================================

# Calculate SES forecasts
ses_forecast <- function(d, alpha, initial_level) {
  
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  
  F[1] <- initial_level
  
  for (t in 2:(n + 1)) {
    F[t] <- alpha * d[t - 1] +
      (1 - alpha) * F[t - 1]
  }
  
  F
}


# =========================================================
# USER INTERFACE
# =========================================================

ses_ui <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        ns("file"),
        "Upload Excel or CSV",
        accept = c(".xlsx", ".xls", ".csv")
      ),
      
      selectInput(
        ns("column"),
        "Demand column",
        choices = NULL
      ),
      
      radioButtons(
        ns("mode"),
        "Alpha selection",
        choices = c(
          "Manual alpha" = "manual",
          "Optimize alpha" = "auto"
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'manual'",
        ns = ns,
        
        sliderInput(
          ns("alpha"),
          "Smoothing parameter (alpha)",
          min = 0,
          max = 1,
          value = 0.3,
          step = 0.01
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'auto'",
        ns = ns,
        
        selectInput(
          ns("criterion"),
          "Optimization criterion",
          choices = c("MAD", "MSE", "MAPE")
        )
      ),
      
      radioButtons(
        ns("initial_mode"),
        "Initial level",
        choices = c(
          "Regression estimate" = "regression",
          "Manual initial level" = "manual"
        )
      ),
      
      conditionalPanel(
        condition = "input.initial_mode == 'manual'",
        ns = ns,
        
        numericInput(
          ns("initial_level"),
          "Initial level value",
          value = 100
        )
      )
      
    ),
    
    mainPanel(
      
      h4(textOutput(ns("title"))),
      
      h4("Forecast Plot"),
      plotOutput(ns("forecast_plot"), height = "350px"),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Parameters Used"),
      tableOutput(ns("parameter_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
      
    )
  )
}


# =========================================================
# SERVER
# =========================================================

ses_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded data
    data <- reactive({
      
      req(input$file)
      
      read_forecast_data(
        input$file$datapath,
        input$file$name
      )
    })
    
    
    # Select numeric columns
    observeEvent(data(), {
      
      numeric_cols <- names(data())[
        sapply(data(), is.numeric)
      ]
      
      updateSelectInput(
        session,
        "column",
        choices = numeric_cols,
        selected = if ("Demand" %in% numeric_cols) {
          "Demand"
        } else {
          tail(numeric_cols, 1)
        }
      )
    })
    
    
    # Extract demand
    demand <- reactive({
      
      req(input$column)
      
      d <- data()[[input$column]]
      
      validate(
        need(
          is.numeric(d) &&
            all(is.finite(d[!is.na(d)])),
          "Demand must contain finite numeric values."
        )
      )
      
      d <- d[!is.na(d)]
      
      validate(
        need(length(d) >= 3,
             "At least 3 observations are required.")
      )
      
      d
    })
    
    
    # Initial level
    initial_level <- reactive({
      
      d <- demand()
      
      if (input$initial_mode == "manual") {
        
        req(input$initial_level)
        input$initial_level
        
      } else {
        
        # Fit a linear regression to estimate
        # the initial level at period 1
        period <- seq_along(d)
        
        fit <- lm(d ~ period)
        
        as.numeric(
          coef(fit)[1] + coef(fit)[2]
        )
      }
    })
    
    
    # Choose alpha
    chosen_alpha <- reactive({
      
      d <- demand()
      level <- initial_level()
      
      if (input$mode == "manual") {
        
        req(input$alpha)
        input$alpha
        
      } else {
        
        req(input$criterion)
        
        objective <- function(alpha) {
          
          F <- ses_forecast(d, alpha, level)
          
          errors <- calculate_forecast_errors(
            d[-1],
            F[2:length(d)]
          )
          
          value <- errors[input$criterion]
          
          if (!is.finite(value)) {
            return(1e12)
          }
          
          as.numeric(value)
        }
        
        fit <- optimize(
          objective,
          interval = c(0, 1)
        )
        
        fit$minimum
      }
    })
    
    
    # Calculate results
    result <- reactive({
      
      d <- demand()
      alpha <- chosen_alpha()
      level <- initial_level()
      
      F <- ses_forecast(d, alpha, level)
      
      errors <- calculate_forecast_errors(
        d[-1],
        F[2:length(d)]
      )
      
      list(
        d = d,
        F = F,
        alpha = alpha,
        level = level,
        errors = errors
      )
    })
    
    
    # Forecast title
    output$title <- renderText({
      
      r <- result()
      
      paste0(
        "SES - Next-period forecast: ",
        round(tail(r$F, 1), 2)
      )
    })
    
    
    # Forecast plot
    output$forecast_plot <- renderPlot({
      
      r <- result()
      
      plot_forecast(
        r$d,
        r$F,
        "Simple Exponential Smoothing"
      )
    })
    
    
    # Error measures
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = names(r$errors),
        Value = round(
          as.numeric(r$errors), 3
        )
      )
    })
    
    
    # Parameter table
    output$parameter_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Parameter = c(
          "Alpha",
          "Initial level"
        ),
        Value = round(
          c(r$alpha, r$level), 4
        )
      )
    })
    
    
    # Period table
    output$period_table <- renderTable({
      
      r <- result()
      
      create_forecast_table(
        r$d,
        r$F
      )
      
    }, na = "")
    
  })
}
