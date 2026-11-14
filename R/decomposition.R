
# =========================================================
# SEASONAL DECOMPOSITION USING MOVING AVERAGES
# Multiplicative model
# =========================================================

# Calculate centered moving average
decomposition_cma <- function(d, m) {
  
  n <- length(d)
  result <- rep(NA_real_, n)
  
  if (m %% 2 == 1) {
    
    half <- (m - 1) / 2
    
    for (t in seq.int(half + 1, n - half)) {
      result[t] <- mean(d[(t - half):(t + half)])
    }
    
  } else {
    
    # Even season length: center two moving averages
    ma <- sapply(seq_len(n - m + 1), function(i) {
      mean(d[i:(i + m - 1)])
    })
    
    if (length(ma) >= 2) {
      positions <- seq.int(m / 2 + 1, n - m / 2)
      
      result[positions] <- (
        ma[-length(ma)] + ma[-1]
      ) / 2
    }
  }
  
  result
}


# Calculate seasonal indices and trend
decomposition_fit <- function(d, m) {
  
  n <- length(d)
  period <- seq_len(n)
  season <- ((period - 1) %% m) + 1
  
  cma <- decomposition_cma(d, m)
  
  ratios <- d / cma
  ratios[!is.finite(ratios)] <- NA_real_
  
  factors <- sapply(seq_len(m), function(s) {
    mean(ratios[season == s], na.rm = TRUE)
  })
  
  if (any(!is.finite(factors)) || mean(factors) <= 0) {
    stop("Unable to calculate seasonal indices.")
  }
  
  # Normalize indices to an average of 1
  factors <- factors / mean(factors)
  
  deseasonalized <- d / factors[season]
  
  # Fit linear trend to deseasonalized demand
  trend_model <- lm(deseasonalized ~ period)
  
  trend <- as.numeric(
    predict(
      trend_model,
      newdata = data.frame(period = seq_len(n + m))
    )
  )
  
  # Forecast for all periods
  future_season <- ((seq_len(n + m) - 1) %% m) + 1
  
  forecast <- trend * factors[future_season]
  
  list(
    cma = cma,
    factors = factors,
    deseasonalized = deseasonalized,
    trend = trend,
    forecast = forecast,
    intercept = unname(coef(trend_model)[1]),
    slope = unname(coef(trend_model)[2])
  )
}


# =========================================================
# USER INTERFACE
# =========================================================

decomposition_ui <- function(id) {
  
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
      )
      
    ),
    
    mainPanel(
      
      h4("Seasonal Decomposition Using Moving Averages"),
      
      h4("Forecast Plot"),
      plotOutput(ns("forecast_plot"), height = "350px"),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Seasonal Indices"),
      tableOutput(ns("factor_table")),
      
      h4("Trend Parameters"),
      tableOutput(ns("trend_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
      
    )
  )
}


# =========================================================
# SERVER
# =========================================================

decomposition_server <- function(id) {
  
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
      
      decomposition_fit(d, m)
    })
    
    
    output$forecast_plot <- renderPlot({
      
      d <- demand()
      r <- result()
      n <- length(d)
      m <- input$season_length
      
      plot(
        seq_len(n),
        d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + m),
        ylim = range(c(d, r$forecast), na.rm = TRUE),
        xlab = "Period",
        ylab = "Demand",
        main = "Seasonal Decomposition Forecast"
      )
      
      lines(
        seq_along(r$forecast),
        r$forecast,
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
      
      d <- demand()
      r <- result()
      
      errors <- calculate_forecast_errors(
        d,
        r$forecast[seq_along(d)]
      )
      
      data.frame(
        Measure = names(errors),
        Value = round(as.numeric(errors), 3)
      )
    })
    
    
    output$factor_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Season = seq_along(r$factors),
        Seasonal_Index = round(r$factors, 4)
      )
    })
    
    
    output$trend_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Parameter = c("Intercept", "Slope"),
        Value = round(
          c(r$intercept, r$slope), 4
        )
      )
    })
    
    
    output$period_table <- renderTable({
      
      d <- demand()
      r <- result()
      n <- length(d)
      m <- input$season_length
      
      data.frame(
        Period = seq_len(n + m),
        Demand = c(d, rep(NA_real_, m)),
        Forecast = round(r$forecast, 2)
      )
      
    }, na = "")
    
  })
}
