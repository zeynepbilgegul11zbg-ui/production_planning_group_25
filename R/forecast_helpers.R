
# =====================================================
# FORECASTING - SHARED HELPER FUNCTIONS
# =====================================================

# Read Excel or CSV files

read_forecast_data <- function(path, filename) {
  
  extension <- tolower(tools::file_ext(filename))
  
  if (extension %in% c("xlsx", "xls")) {
    
    data <- readxl::read_excel(path)
    
  } else if (extension == "csv") {
    
    first_line <- readLines(path, n = 1, warn = FALSE)
    
    semicolons <- lengths(
      regmatches(first_line, gregexpr(";", first_line, fixed = TRUE))
    )
    
    commas <- lengths(
      regmatches(first_line, gregexpr(",", first_line, fixed = TRUE))
    )
    
    separator <- if (semicolons > commas) ";" else ","
    
    data <- read.csv(
      path,
      sep = separator,
      check.names = FALSE
    )
    
  } else {
    stop("Unsupported file format.")
  }
  
  as.data.frame(data)
}



# Calculate forecast error measures
calculate_forecast_errors <- function(actual, forecast) {
  
  valid <- is.finite(actual) & is.finite(forecast)
  
  actual <- actual[valid]
  forecast <- forecast[valid]
  
  if (length(actual) == 0) {
    return(c(MAD = NA_real_,
             MSE = NA_real_,
             MAPE = NA_real_))
  }
  
  errors <- forecast - actual
  
  MAD <- mean(abs(errors))
  MSE <- mean(errors^2)
  
  nonzero <- actual != 0
  
  MAPE <- if (any(nonzero)) {
    mean(abs(errors[nonzero] / actual[nonzero])) * 100
  } else {
    NA_real_
  }
  
  c(MAD = MAD, MSE = MSE, MAPE = MAPE)
}


# Create a forecast results table
create_forecast_table <- function(actual, forecast) {
  
  n <- length(actual)
  
  if (length(forecast) != n + 1) {
    stop("Forecast must contain n + 1 values.")
  }
  
  data.frame(
    Period = 1:(n + 1),
    Demand = c(actual, NA_real_),
    Forecast = forecast,
    Error = forecast - c(actual, NA_real_)
  )
}


# Plot actual demand and forecasts
plot_forecast <- function(actual, forecast, method) {
  
  n <- length(actual)
  
  if (length(forecast) != n + 1) {
    stop("Forecast must contain n + 1 values.")
  }
  
  y_range <- range(c(actual, forecast), na.rm = TRUE)
  
  plot(
    1:n, actual,
    type = "o",
    pch = 19,
    col = "grey30",
    xlim = c(1, n + 1),
    ylim = y_range,
    xlab = "Period",
    ylab = "Demand",
    main = paste(method, "Forecast")
  )
  
  lines(
    1:(n + 1), forecast,
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
}
