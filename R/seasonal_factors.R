
# =========================================================
# SEASONAL FACTORS - QUICK AND DIRTY METHOD
# =========================================================

seasonal_factors_calculate <- function(d, season_length) {
  
  n <- length(d)
  m <- season_length
  
  season <- ((seq_len(n) - 1) %% m) + 1
  
  overall_mean <- mean(d)
  
  if (!is.finite(overall_mean) || overall_mean == 0) {
    stop("Overall mean must be non-zero.")
  }
  
  seasonal_means <- sapply(seq_len(m), function(s) {
    mean(d[season == s])
  })
  
  factors <- seasonal_means / overall_mean
  
  # Normalize factors so their average is 1
  factors <- factors / mean(factors)
  
  factors
}


seasonal_factors_forecast <- function(d, season_length) {
  
  n <- length(d)
  m <- season_length
  
  factors <- seasonal_factors_calculate(d, m)
  
  # Use completed seasons to estimate the base level
  completed <- floor(n / m)
  
  if (completed < 2) {
    stop("At least two complete seasons are required.")
  }
  
  # Rolling historical forecasts using only earlier seasons
  F <- rep(NA_real_, n + m)
  
  for (t in seq.int(2 * m + 1, n + m)) {
    
    past <- d[seq_len(min(t - 1, n))]
    
    if (length(past) < 2 * m) {
      next
    }
    
    past_factors <- seasonal_factors_calculate(past, m)
    
    base <- mean(
      tail(past, m) /
        past_factors[((seq.int(length(past) - m + 1,
                               length(past)) - 1) %% m) + 1]
    )
    
    target_season <- ((t - 1) %% m) + 1
    
    F[t] <- base * past_factors[target_season]
  }
  
  list(
    factors = factors,
    forecast = F
  )
}


# =========================================================
# USER INTERFACE
# =========================================================

seasonal_factors_ui <- function(id) {
  
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
      
      h4("Seasonal Factors (Quick and Dirty Method)"),
      
      h4("Forecast Plot"),
      plotOutput(ns("forecast_plot")),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Seasonal Factors"),
      tableOutput(ns("factor_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
      
    )
  )
}


# =========================================================
# SERVER
# =========================================================

seasonal_factors_server <- function(id) {
  
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
          all(is.finite(d)),
          "Demand must contain finite numeric values."
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
            length(d) >= 2 * m,
          "At least two complete seasons are required."
        ),
        need(
          all(d >= 0) && mean(d) > 0,
          "Demand must be non-negative with a positive mean."
        )
      )
      
      seasonal_factors_forecast(d, m)
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
        main = "Seasonal Factors Forecast"
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
        Seasonal_Factor = round(r$factors, 4)
      )
    })
    
    
    output$period_table <- renderTable({
      
      d <- demand()
      r <- result()
      m <- input$season_length
      n <- length(d)
      
      data.frame(
        Period = seq_len(n + m),
        Demand = c(d, rep(NA_real_, m)),
        Forecast = round(r$forecast, 2)
      )
      
    }, na = "")
    
  })
}
