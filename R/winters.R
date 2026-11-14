
# =========================================================
# WINTERS' METHOD - MULTIPLICATIVE
# =========================================================

winters_fit <- function(d, m, alpha, beta, gamma,
                        initial_mode = "automatic",
                        initial_level = NULL,
                        initial_trend = NULL) {
  
  ts_data <- ts(d, frequency = m)
  
  if (initial_mode == "manual") {
    
    # Estimate seasonal indices from the first two seasons
    first_seasons <- d[seq_len(2 * m)]
    season_means <- sapply(seq_len(m), function(s) {
      mean(first_seasons[seq(s, 2 * m, by = m)])
    })
    
    seasonal <- season_means / mean(season_means)
    seasonal <- seasonal / mean(seasonal)
    
    fit <- HoltWinters(
      ts_data,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      seasonal = "multiplicative",
      start.periods = 2,
      l.start = initial_level,
      b.start = initial_trend,
      s.start = seasonal
    )
    
  } else {
    
    fit <- HoltWinters(
      ts_data,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      seasonal = "multiplicative",
      start.periods = 2
    )
  }
  
  fit
}


# =========================================================
# USER INTERFACE
# =========================================================

winters_ui <- function(id) {
  
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
        ns("season_length"),
        "Season length",
        value = 12,
        min = 2,
        step = 1
      ),
      
      radioButtons(
        ns("mode"),
        "Parameter selection",
        choices = c(
          "Manual alpha, beta and gamma" = "manual",
          "Optimize parameters" = "auto"
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
        ),
        
        sliderInput(
          ns("gamma"),
          "Gamma (seasonality)",
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
          "Automatic initialization" = "automatic",
          "Manual level and trend" = "manual"
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
      
      h4("Seasonal Indices"),
      tableOutput(ns("seasonal_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
      
    )
  )
}


# =========================================================
# SERVER
# =========================================================

winters_server <- function(id) {
  
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
        choices = numeric_cols,
        selected = if ("Demand" %in% numeric_cols) {
          "Demand"
        } else {
          tail(numeric_cols, 1)
        }
      )
    })
    
    
    demand <- reactive({
      
      req(input$column)
      
      d <- data()[[input$column]]
      d <- d[!is.na(d)]
      
      validate(
        need(
          length(d) >= 3 &&
            all(is.finite(d)) &&
            all(d > 0),
          "Demand must contain positive finite values."
        )
      )
      
      d
    })
    
    
    result <- reactive({
      
      d <- demand()
      m <- input$season_length
      
      validate(
        need(
          is.finite(m) &&
            m == as.integer(m) &&
            m >= 2 &&
            length(d) >= 2 * m + 1,
          "At least two complete seasons plus one observation are required."
        )
      )
      
      if (input$initial_mode == "manual") {
        
        req(input$initial_level, input$initial_trend)
        
        initial_level <- input$initial_level
        initial_trend <- input$initial_trend
        
      } else {
        
        initial_level <- NULL
        initial_trend <- NULL
      }
      
      if (input$mode == "manual") {
        
        req(input$alpha, input$beta, input$gamma)
        
        fit <- winters_fit(
          d, m,
          input$alpha,
          input$beta,
          input$gamma,
          input$initial_mode,
          initial_level,
          initial_trend
        )
        
      } else {
        
        req(input$criterion)
        
        objective <- function(par) {
          
          fit <- tryCatch(
            winters_fit(
              d, m,
              par[1], par[2], par[3],
              input$initial_mode,
              initial_level,
              initial_trend
            ),
            error = function(e) NULL
          )
          
          if (is.null(fit)) {
            return(1e12)
          }
          
          
          fitted_values <- as.numeric(fit$fitted[, "xhat"])
          
          actual_values <- tail(
            as.numeric(fit$x),
            length(fitted_values)
          )
          
          errors <- calculate_forecast_errors(
            actual_values,
            fitted_values
          )
          
          
          value <- errors[input$criterion]
          
          if (!is.finite(value)) {
            return(1e12)
          }
          
          as.numeric(value)
        }
        
        opt <- optim(
          par = c(0.3, 0.2, 0.2),
          fn = objective,
          method = "L-BFGS-B",
          lower = c(0, 0, 0),
          upper = c(1, 1, 1)
        )
        print(opt)
        
        fit <- winters_fit(
          d, m,
          opt$par[1],
          opt$par[2],
          opt$par[3],
          input$initial_mode,
          initial_level,
          initial_trend
        )
      }
      
      # Fitted values are available after initialization
      fitted_values <- as.numeric(fit$fitted[, "xhat"])
      
      # Forecast the next full season
      future <- as.numeric(
        predict(fit, n.ahead = m)
      )
      
      # Align fitted values with the original demand
      fitted_full <- rep(NA_real_, length(d))
      start_index <- length(d) - length(fitted_values) + 1
      
      fitted_full[start_index:length(d)] <- fitted_values
      
      full_forecast <- c(fitted_full, future)
      
      errors <- calculate_forecast_errors(
        d,
        fitted_full
      )
      
      list(
        d = d,
        m = m,
        fit = fit,
        F = full_forecast,
        errors = errors,
        future = future
      )
    })
    
    
    output$title <- renderText({
      
      r <- result()
      
      paste0(
        "Winters - Next-period forecast: ",
        round(r$future[1], 2)
      )
    })
    
    
    output$forecast_plot <- renderPlot({
      
      r <- result()
      n <- length(r$d)
      
      plot(
        seq_len(n),
        r$d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + r$m),
        ylim = range(c(r$d, r$F), na.rm = TRUE),
        xlab = "Period",
        ylab = "Demand",
        main = "Winters' Method Forecast"
      )
      
      lines(
        seq_along(r$F),
        r$F,
        type = "o",
        pch = 17,
        lty = 2,
        col = "#2F6FB0"
      )
      
      legend(
        "topleft",
        legend = c("Actual Demand", "Forecast"),
        col = c("grey30", "#2F6FB0"),
        pch = c(19, 17),
        lty = c(1, 2),
        bty = "n"
      )
    })
    
    
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = names(r$errors),
        Value = round(as.numeric(r$errors), 3)
      )
    })
    
    
    output$parameter_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Parameter = c("Alpha", "Beta", "Gamma"),
        Value = round(
          c(
            r$fit$alpha,
            r$fit$beta,
            r$fit$gamma
          ),
          4
        )
      )
    })
    
    
    
    output$seasonal_table <- renderTable({
      
      r <- result()
      
      seasonal_values <- tail(
        r$fit$fitted[, "season"],
        r$m
      )
      
      data.frame(
        Season = seq_len(r$m),
        Seasonal_Index = round(
          as.numeric(seasonal_values), 4
        )
      )
      
    })
    
    
    
    output$period_table <- renderTable({
      
      r <- result()
      n <- length(r$d)
      
      data.frame(
        Period = seq_len(n + r$m),
        Demand = c(
          r$d,
          rep(NA_real_, r$m)
        ),
        Forecast = round(r$F, 2)
      )
      
    }, na = "")
    
  })
}
