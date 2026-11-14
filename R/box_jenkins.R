
# =========================================================
# BOX-JENKINS: AR, MA, ARMA, ARIMA
# =========================================================

box_jenkins_ui <- function(id) {
  
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
      
      selectInput(
        ns("model_type"),
        "Model type",
        choices = c("AR", "MA", "ARMA", "ARIMA")
      ),
      
      radioButtons(
        ns("mode"),
        "Model selection",
        choices = c(
          "Manual parameters" = "manual",
          "Automatic selection" = "auto"
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'manual'",
        ns = ns,
        
        numericInput(
          ns("p"), "AR order (p)",
          value = 1, min = 0, max = 5
        ),
        
        numericInput(
          ns("d"), "Differencing order (d)",
          value = 1, min = 0, max = 2
        ),
        
        numericInput(
          ns("q"), "MA order (q)",
          value = 1, min = 0, max = 5
        )
      ),
      
      conditionalPanel(
        condition = "input.mode == 'auto'",
        ns = ns,
        
        selectInput(
          ns("criterion"),
          "Model selection criterion",
          choices = c("AIC", "BIC")
        )
      ),
      
      numericInput(
        ns("horizon"),
        "Forecast horizon",
        value = 12,
        min = 1,
        max = 100
      )
      
    ),
    
    mainPanel(
      
      h4("Box-Jenkins Forecasting"),
      
      h4("ACF and PACF"),
      plotOutput(ns("acf_plot"), height = "300px"),
      
      h4("Forecast Plot"),
      plotOutput(ns("forecast_plot"), height = "350px"),
      
      h4("Selected Model"),
      tableOutput(ns("model_table")),
      
      h4("Error Measures"),
      tableOutput(ns("error_table")),
      
      h4("Model Coefficients"),
      tableOutput(ns("coefficient_table")),
      
      h4("Forecasts by Period"),
      tableOutput(ns("period_table"))
      
    )
  )
}


box_jenkins_server <- function(id) {
  
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
      
      validate(
        need(
          length(d) >= 10 &&
            all(is.finite(d)),
          "At least 10 finite demand observations are required."
        )
      )
      
      as.numeric(d)
    })
    
    
    model_orders <- reactive({
      
      type <- input$model_type
      
      if (input$mode == "manual") {
        
        p <- if (type %in% c("AR", "ARMA", "ARIMA")) {
          input$p
        } else {
          0
        }
        
        d <- if (type == "ARIMA") input$d else 0
        
        q <- if (type %in% c("MA", "ARMA", "ARIMA")) {
          input$q
        } else {
          0
        }
        
        validate(
          need(
            all(is.finite(c(p, d, q))) &&
              all(c(p, d, q) >= 0) &&
              all(c(p, d, q) == floor(c(p, d, q))) &&
              p <= 5 && d <= 2 && q <= 5,
            "Model orders must be integers within the allowed range."
          ),
          need(
            p + q > 0,
            "Select at least one AR or MA term."
          )
        )
        
        c(p, d, q)
        
      } else {
        
        NULL
      }
    })
    
    
    result <- reactive({
      
      d <- demand()
      n <- length(d)
      type <- input$model_type
      
      if (input$mode == "manual") {
        
        orders <- model_orders()
        
        fit <- tryCatch(
          stats::arima(
            d,
            order = orders,
            method = "ML"
          ),
          error = function(e) NULL
        )
        
        validate(
          need(
            !is.null(fit),
            "Model fitting failed. Try different orders."
          )
        )
        
        selected <- orders
        
      } else {
        
        req(input$criterion)
        
        candidates <- list()
        
        for (p in 0:3) {
          for (q in 0:3) {
            for (dd in 0:2) {
              
              valid <- switch(
                type,
                AR = (p > 0 && q == 0 && dd == 0),
                MA = (p == 0 && q > 0 && dd == 0),
                ARMA = (p > 0 && q > 0 && dd == 0),
                ARIMA = (p + q > 0)
              )
              
              if (!valid) next
              
              fit_try <- tryCatch(
                suppressWarnings(
                  stats::arima(
                    d,
                    order = c(p, dd, q),
                    method = "ML"
                  )
                ),
                error = function(e) NULL
              )
              
              if (is.null(fit_try)) next
              
              k <- length(fit_try$coef) + 1
              
              aic <- fit_try$aic
              bic <- -2 * fit_try$loglik +
                log(n) * k
              
              score <- if (input$criterion == "AIC") {
                aic
              } else {
                bic
              }
              
              if (!is.finite(score)) next
              
              candidates[[length(candidates) + 1]] <- list(
                fit = fit_try,
                orders = c(p, dd, q),
                score = score
              )
            }
          }
        }
        
        validate(
          need(
            length(candidates) > 0,
            "No suitable model was found."
          )
        )
        
        scores <- sapply(candidates, function(x) x$score)
        best <- candidates[[which.min(scores)]]
        
        fit <- best$fit
        selected <- best$orders
      }
      
      h <- input$horizon
      
      prediction <- tryCatch(
        predict(fit, n.ahead = h),
        error = function(e) NULL
      )
      
      validate(
        need(
          !is.null(prediction),
          "Forecasting failed for the selected model."
        )
      )
      
      future <- as.numeric(prediction$pred)
      
      # In-sample one-step prediction residuals
      fitted_values <- d - as.numeric(residuals(fit))
      
      errors <- calculate_forecast_errors(
        d,
        fitted_values
      )
      
      k <- length(fit$coef) + 1
      
      aic <- fit$aic
      bic <- -2 * fit$loglik + log(n) * k
      
      list(
        d = d,
        fit = fit,
        orders = selected,
        fitted = fitted_values,
        future = future,
        errors = errors,
        aic = aic,
        bic = bic
      )
    })
    
    
    output$acf_plot <- renderPlot({
      
      d <- demand()
      
      par(mfrow = c(1, 2))
      on.exit(par(mfrow = c(1, 1)))
      
      stats::acf(
        d,
        main = "Autocorrelation (ACF)"
      )
      
      stats::pacf(
        d,
        main = "Partial Autocorrelation (PACF)"
      )
    })
    
    
    output$forecast_plot <- renderPlot({
      
      r <- result()
      n <- length(r$d)
      h <- length(r$future)
      
      plot(
        seq_len(n),
        r$d,
        type = "o",
        pch = 19,
        col = "grey30",
        xlim = c(1, n + h),
        ylim = range(
          c(r$d, r$fitted, r$future),
          na.rm = TRUE
        ),
        xlab = "Period",
        ylab = "Demand",
        main = "Box-Jenkins Forecast"
      )
      
      lines(
        seq_len(n),
        r$fitted,
        col = "#2F6FB0",
        lty = 2,
        lwd = 2
      )
      
      lines(
        n:(n + h),
        c(tail(r$fitted, 1), r$future),
        col = "#D97706",
        lwd = 2,
        type = "o",
        pch = 17
      )
      
      legend(
        "topleft",
        legend = c(
          "Actual Demand",
          "Fitted Values",
          "Future Forecast"
        ),
        col = c(
          "grey30",
          "#2F6FB0",
          "#D97706"
        ),
        lty = c(1, 2, 1),
        pch = c(19, NA, 17),
        bty = "n"
      )
    })
    
    
    output$model_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Model = paste0(
          "ARIMA(",
          paste(r$orders, collapse = ","),
          ")"
        ),
        AIC = round(r$aic, 3),
        BIC = round(r$bic, 3)
      )
    })
    
    
    output$error_table <- renderTable({
      
      r <- result()
      
      data.frame(
        Measure = names(r$errors),
        Value = round(as.numeric(r$errors), 3)
      )
    })
    
    
    output$coefficient_table <- renderTable({
      
      r <- result()
      
      if (length(r$fit$coef) == 0) {
        return(data.frame(
          Parameter = "None",
          Estimate = NA_real_
        ))
      }
      
      data.frame(
        Parameter = names(r$fit$coef),
        Estimate = round(
          as.numeric(r$fit$coef), 4
        )
      )
    })
    
    
    output$period_table <- renderTable({
      
      r <- result()
      
      n <- length(r$d)
      h <- length(r$future)
      
      data.frame(
        Period = seq_len(n + h),
        Demand = c(r$d, rep(NA_real_, h)),
        Forecast = round(
          c(r$fitted, r$future),
          2
        )
      )
      
    }, na = "")
    
  })
}
