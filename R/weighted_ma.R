
# =========================================================
# WEIGHTED MOVING AVERAGE (WMA)
# =========================================================

weighted_ma_forecast <- function(d, weights) {
  
  n <- length(d)
  N <- length(weights)
  
  F <- rep(NA_real_, n + 1)
  
  for (t in seq.int(N + 1, n + 1)) {
    past <- rev(d[(t - N):(t - 1)])
    F[t] <- sum(weights * past)
  }
  
  F
}


weighted_ma_ui <- function(id) {
  
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
      
      numericInput(
        ns("N"),
        "Number of past periods (N)",
        value = 3,
        min = 1,
        step = 1
      ),
      
      radioButtons(
        ns("mode"),
        "Weight selection",
        choices = c(
          "Manual weights" = "manual",
          "Optimize weights" = "auto"
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'manual'",
        ns = ns,
        
        textInput(
          ns("weights"),
          "Weights (most recent first)",
          value = "0.5,0.3,0.2"
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
      )
      
    ),
    
    mainPanel(
      h4(textOutput(ns("title"))),
      
      plotOutput(ns("forecast_plot")),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Weights Used"),
      tableOutput(ns("weight_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
    )
  )
}


weighted_ma_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file)
      
      read_forecast_data(
        input$file$datapath,
        input$file$name
      )
    })
    
    observeEvent(data(), {
      numeric_cols <- names(data())[
        sapply(data(), is.numeric)
      ]
      
      updateSelectInput(
        session,
        "column",
        choices = numeric_cols
      )
    })
    
    demand <- reactive({
      req(input$column)
      
      d <- data()[[input$column]]
      d <- d[!is.na(d)]
      
      validate(
        need(all(is.finite(d)),
             "Demand must contain finite numeric values.")
      )
      
      d
    })
    
    chosen_weights <- reactive({
      
      d <- demand()
      N <- input$N
      
      validate(
        need(
          is.finite(N) && N == as.integer(N) &&
            N >= 1 && N < length(d),
          "N must be a positive integer smaller than the data length."
        )
      )
      
      if (input$mode == "manual") {
        
        w <- suppressWarnings(as.numeric(
          trimws(strsplit(input$weights, ",")[[1]])
        ))
        
        validate(
          need(
            length(w) == N &&
              all(is.finite(w)) &&
              all(w >= 0) &&
              sum(w) > 0,
            "Enter N non-negative weights with a positive total."
          )
        )
        
        w / sum(w)
        
      } else {
        
        objective <- function(par) {
          
          ex <- exp(par - max(par))
          w <- ex / sum(ex)
          
          F <- weighted_ma_forecast(d, w)
          
          errors <- calculate_forecast_errors(
            d, F[seq_along(d)]
          )
          
          value <- errors[input$criterion]
          
          if (!is.finite(value)) return(1e12)
          
          as.numeric(value)
        }
        
        fit <- optim(
          par = rep(0, N),
          fn = objective,
          method = "BFGS"
        )
        
        ex <- exp(fit$par - max(fit$par))
        ex / sum(ex)
      }
    })
    
    result <- reactive({
      
      d <- demand()
      w <- chosen_weights()
      
      F <- weighted_ma_forecast(d, w)
      
      errors <- calculate_forecast_errors(
        d, F[seq_along(d)]
      )
      
      list(
        d = d,
        F = F,
        w = w,
        errors = errors
      )
    })
    
    output$title <- renderText({
      
      r <- result()
      
      paste0(
        "WMA(", length(r$w), ") - Next Forecast: ",
        round(tail(r$F, 1), 2)
      )
    })
    
    output$forecast_plot <- renderPlot({
      
      r <- result()
      
      plot_forecast(
        r$d,
        r$F,
        "Weighted Moving Average"
      )
    })
    
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = names(r$errors),
        Value = round(as.numeric(r$errors), 3)
      )
    })
    
    output$weight_table <- renderTable({
      
      w <- chosen_weights()
      
      data.frame(
        Lag = seq_along(w),
        Weight = round(w, 4)
      )
    })
    
    output$period_table <- renderTable({
      
      r <- result()
      
      create_forecast_table(r$d, r$F)
      
    }, na = "")
    
  })
}
