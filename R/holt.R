
# =========================================================
# HOLT'S DOUBLE EXPONENTIAL SMOOTHING
# =========================================================

holt_forecast <- function(d, alpha, beta,
                          initial_level, initial_trend) {
  
  n <- length(d)
  
  level <- rep(NA_real_, n)
  trend <- rep(NA_real_, n)
  F <- rep(NA_real_, n + 1)
  
  level[1] <- initial_level
  trend[1] <- initial_trend
  
  # No fitted forecast for the first observation
  F[2] <- level[1] + trend[1]
  
  for (t in 2:n) {
    
    level[t] <- alpha * d[t] +
      (1 - alpha) * (level[t - 1] + trend[t - 1])
    
    trend[t] <- beta * (level[t] - level[t - 1]) +
      (1 - beta) * trend[t - 1]
    
    F[t + 1] <- level[t] + trend[t]
  }
  
  list(
    forecast = F,
    level = level,
    trend = trend
  )
}


# =========================================================
# USER INTERFACE
# =========================================================

holt_ui <- function(id) {
  
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
        "Parameter selection",
        choices = c(
          "Manual alpha and beta" = "manual",
          "Optimize alpha and beta" = "auto"
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'manual'",
        ns = ns,
        
        sliderInput(
          ns("alpha"),
          "Alpha (level)",
          min = 0,
          max = 1,
          value = 0.3,
          step = 0.01
        ),
        
        sliderInput(
          ns("beta"),
          "Beta (trend)",
          min = 0,
          max = 1,
          value = 0.2,
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
        "Initial values",
        choices = c(
          "Regression estimates" = "regression",
          "Manual initial values" = "manual"
        )
      ),
      
      conditionalPanel(
        condition = "input.initial_mode == 'manual'",
        ns = ns,
        
        numericInput(
          ns("initial_level"),
          "Initial level",
          value = 100
        ),
        
        numericInput(
          ns("initial_trend"),
          "Initial trend",
          value = 0
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

holt_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded data
    data <- reactive({
      
      req(input$file)
      
      read_forecast_data(
        input$file$datapath,
        input$file$name
      )
    })
    
    
    # Update demand column
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
    
    
    # Demand series
    demand <- reactive({
      
      req(input$column)
      
      d <- data()[[input$column]]
      d <- d[!is.na(d)]
      
      validate(
        need(
          length(d) >= 3 &&
            all(is.finite(d)),
          "At least 3 finite observations are required."
        )
      )
      
      d
    })
    
    
    # Initial level and trend
    initial_values <- reactive({
      
      d <- demand()
      
      if (input$initial_mode == "manual") {
        
        req(input$initial_level, input$initial_trend)
        
        c(
          level = input$initial_level,
          trend = input$initial_trend
        )
        
      } else {
        
        # Regression-based initialization
        period <- seq_along(d)
        
        fit <- lm(d ~ period)
        
        intercept <- unname(coef(fit)[1])
        slope <- unname(coef(fit)[2])
        
        c(
          level = intercept + slope,
          trend = slope
        )
      }
    })
    
    
    # Select alpha and beta
    chosen_parameters <- reactive({
      
      d <- demand()
      initial <- initial_values()
      
      if (input$mode == "manual") {
        
        req(input$alpha, input$beta)
        
        c(
          alpha = input$alpha,
          beta = input$beta
        )
        
      } else {
        
        req(input$criterion)
        
        objective <- function(par) {
          
          alpha <- par[1]
          beta <- par[2]
          
          fit <- holt_forecast(
            d,
            alpha,
            beta,
            initial["level"],
            initial["trend"]
          )
          
          errors <- calculate_forecast_errors(
            d[-1],
            fit$forecast[2:length(d)]
          )
          
          value <- errors[input$criterion]
          
          if (!is.finite(value)) {
            return(1e12)
          }
          
          as.numeric(value)
        }
        
        fit <- optim(
          par = c(0.3, 0.2),
          fn = objective,
          method = "L-BFGS-B",
          lower = c(0, 0),
          upper = c(1, 1)
        )
        
        c(
          alpha = fit$par[1],
          beta = fit$par[2]
        )
      }
    })
    
    
    # Results
    result <- reactive({
      
      d <- demand()
      par <- chosen_parameters()
      initial <- initial_values()
      
      fit <- holt_forecast(
        d,
        par["alpha"],
        par["beta"],
        initial["level"],
        initial["trend"]
      )
      
      errors <- calculate_forecast_errors(
        d[-1],
        fit$forecast[2:length(d)]
      )
      
      list(
        d = d,
        F = fit$forecast,
        level = fit$level,
        trend = fit$trend,
        alpha = unname(par["alpha"]),
        beta = unname(par["beta"]),
        initial_level = unname(initial["level"]),
        initial_trend = unname(initial["trend"]),
        errors = errors
      )
    })
    
    
    # Title
    output$title <- renderText({
      
      r <- result()
      
      paste0(
        "Holt - Next-period forecast: ",
        round(tail(r$F, 1), 2)
      )
    })
    
    
    # Plot
    output$forecast_plot <- renderPlot({
      
      r <- result()
      
      plot_forecast(
        r$d,
        r$F,
        "Holt's Double Exponential Smoothing"
      )
    })
    
    
    # Error table
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = names(r$errors),
        Value = round(as.numeric(r$errors), 3)
      )
    })
    
    
    # Parameter table
    output$parameter_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Parameter = c(
          "Alpha",
          "Beta",
          "Initial level",
          "Initial trend"
        ),
        Value = round(
          c(
            r$alpha,
            r$beta,
            r$initial_level,
            r$initial_trend
          ),
          4
        )
      )
    })
    
    
    # Forecast table
    output$period_table <- renderTable({
      
      r <- result()
      
      create_forecast_table(
        r$d,
        r$F
      )
      
    }, na = "")
    
  })
}
