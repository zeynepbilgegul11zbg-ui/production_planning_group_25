
chase_strategy_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Chase Strategy (Zero Inventory)"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file"),
          "Upload Demand Data (Excel/CSV)",
          accept = c(".xlsx", ".xls", ".csv")
        ),
        
        uiOutput(ns("columns")),
        
        numericInput(ns("initial_workforce"),
                     "Initial Workforce", 10, min = 0),
        
        numericInput(ns("productivity"),
                     "Units per Worker per Day", 5,
                     min = 0.01),
        
        numericInput(ns("wage"),
                     "Regular Wage per Worker per Day", 100,
                     min = 0),
        
        numericInput(ns("hiring_cost"),
                     "Hiring Cost per Worker", 500,
                     min = 0),
        
        numericInput(ns("firing_cost"),
                     "Firing Cost per Worker", 800,
                     min = 0)
      ),
      
      mainPanel(
        h3("Aggregate Planning Results"),
        tableOutput(ns("results")),
        
        h3("Cost Summary"),
        tableOutput(ns("costs")),
        
        h3("Workforce and Demand"),
        plotOutput(ns("plot"))
      )
    )
  )
}


chase_strategy_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file)
      
      read_forecast_data(
        input$file$datapath,
        input$file$name
      )
    })
    
    output$columns <- renderUI({
      req(data())
      ns <- session$ns
      
      tagList(
        selectInput(
          ns("demand_col"),
          "Demand Column",
          choices = names(data())
        ),
        
        selectInput(
          ns("days_col"),
          "Working Days Column",
          choices = names(data()),
          selected = if (ncol(data()) >= 2)
            names(data())[2] else names(data())[1]
        )
      )
    })
    
    results <- reactive({
      req(data(), input$demand_col, input$days_col)
      
      validate(
        need(input$demand_col != input$days_col,
             "Demand and working days must use different columns.")
      )
      
      demand <- suppressWarnings(
        as.numeric(data()[[input$demand_col]])
      )
      
      days <- suppressWarnings(
        as.numeric(data()[[input$days_col]])
      )
      
      validate(
        need(
          all(is.finite(demand)) &&
            all(is.finite(days)) &&
            all(demand >= 0) &&
            all(days > 0),
          "Demand must be nonnegative and working days must be positive."
        )
      )
      
      n <- length(demand)
      
      required_workers <- ceiling(
        demand / (days * input$productivity)
      )
      
      previous_workers <- c(
        input$initial_workforce,
        head(required_workers, -1)
      )
      
      hired <- pmax(
        required_workers - previous_workers, 0
      )
      
      fired <- pmax(
        previous_workers - required_workers, 0
      )
      
      production <- demand
      
      regular_cost <- required_workers *
        days * input$wage
      
      hiring_cost <- hired * input$hiring_cost
      
      firing_cost <- fired * input$firing_cost
      
      total_cost <- regular_cost +
        hiring_cost + firing_cost
      
      data.frame(
        Period = seq_len(n),
        Demand = demand,
        Working_Days = days,
        Required_Workforce = required_workers,
        Hired = hired,
        Fired = fired,
        Production = production,
        Ending_Inventory = rep(0, n),
        Regular_Cost = regular_cost,
        Hiring_Cost = hiring_cost,
        Firing_Cost = firing_cost,
        Total_Cost = total_cost
      )
    })
    
    output$results <- renderTable({
      results()
    }, digits = 2)
    
    output$costs <- renderTable({
      r <- results()
      
      data.frame(
        Cost_Component = c(
          "Regular Production",
          "Hiring",
          "Firing",
          "Total Cost"
        ),
        Amount = c(
          sum(r$Regular_Cost),
          sum(r$Hiring_Cost),
          sum(r$Firing_Cost),
          sum(r$Total_Cost)
        )
      )
    }, digits = 2)
    
    output$plot <- renderPlot({
      r <- results()
      
      old_par <- par(mfrow = c(2, 1))
      on.exit(par(old_par))
      
      plot(
        r$Period, r$Demand,
        type = "o",
        pch = 19,
        col = "#2F6FB0",
        xlab = "Period",
        ylab = "Units",
        main = "Demand and Production"
      )
      
      lines(
        r$Period, r$Production,
        type = "o",
        pch = 17,
        col = "#D95F02",
        lty = 2
      )
      
      legend(
        "topleft",
        legend = c("Demand", "Production"),
        col = c("#2F6FB0", "#D95F02"),
        lty = c(1, 2),
        pch = c(19, 17),
        bty = "n"
      )
      
      plot(
        r$Period, r$Required_Workforce,
        type = "o",
        pch = 19,
        col = "#238B45",
        xlab = "Period",
        ylab = "Workers",
        main = "Required Workforce"
      )
    })
  })
}
