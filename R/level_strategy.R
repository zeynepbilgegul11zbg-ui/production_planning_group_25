
level_strategy_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Level Strategy (Constant Workforce)"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file"),
          "Upload Demand Data (Excel/CSV)",
          accept = c(".xlsx", ".xls", ".csv")
        ),
        
        uiOutput(ns("columns")),
        
        numericInput(
          ns("initial_workforce"),
          "Initial Workforce",
          value = 1, min = 0, step = 1
        ),
        
        numericInput(
          ns("level_workforce"),
          "Constant Workforce",
          value = 2, min = 0, step = 1
        ),
        
        numericInput(
          ns("productivity"),
          "Units per Worker per Day",
          value = 5, min = 0.01
        ),
        
        numericInput(
          ns("wage"),
          "Regular Wage per Worker per Day",
          value = 100, min = 0
        ),
        
        numericInput(
          ns("initial_inventory"),
          "Initial Inventory",
          value = 0, min = 0
        ),
        
        numericInput(
          ns("holding_cost"),
          "Inventory Holding Cost per Unit per Period",
          value = 2, min = 0
        ),
        
        numericInput(
          ns("backlog_cost"),
          "Backlog Cost per Unit per Period",
          value = 10, min = 0
        ),
        
        numericInput(
          ns("hiring_cost"),
          "Hiring Cost per Worker",
          value = 500, min = 0
        ),
        
        numericInput(
          ns("firing_cost"),
          "Firing Cost per Worker",
          value = 800, min = 0
        )
      ),
      
      mainPanel(
        h3("Level Strategy Results"),
        tableOutput(ns("results")),
        
        h3("Cost Summary"),
        tableOutput(ns("costs")),
        
        h3("Demand and Production"),
        plotOutput(ns("production_plot")),
        
        h3("Inventory and Backlog"),
        plotOutput(ns("inventory_plot"))
      )
    )
  )
}


level_strategy_server <- function(id) {
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
          choices = names(data()),
          selected = if ("Demand" %in% names(data()))
            "Demand" else names(data())[1]
        ),
        
        selectInput(
          ns("days_col"),
          "Working Days Column",
          choices = names(data()),
          selected = if ("WorkingDays" %in% names(data()))
            "WorkingDays" else
              if (ncol(data()) >= 2)
                names(data())[2] else names(data())[1]
        )
      )
    })
    
    results <- reactive({
      req(
        data(), input$demand_col, input$days_col,
        input$level_workforce, input$productivity
      )
      
      validate(
        need(input$demand_col != input$days_col,
             "Select different columns for demand and working days."),
        need(input$level_workforce >= 0 &&
               input$level_workforce %% 1 == 0,
             "Constant workforce must be a nonnegative integer."),
        need(input$initial_workforce >= 0 &&
               input$initial_workforce %% 1 == 0,
             "Initial workforce must be a nonnegative integer."),
        need(input$productivity > 0,
             "Productivity must be positive.")
      )
      
      demand <- suppressWarnings(
        as.numeric(data()[[input$demand_col]])
      )
      days <- suppressWarnings(
        as.numeric(data()[[input$days_col]])
      )
      
      validate(
        need(length(demand) > 0 &&
               all(is.finite(demand)) &&
               all(is.finite(days)) &&
               all(demand >= 0) &&
               all(days > 0),
             "Demand must be nonnegative and working days positive.")
      )
      
      n <- length(demand)
      workforce <- input$level_workforce
      
      capacity <- workforce * days * input$productivity
      
      # Production is planned at regular capacity.
      production <- capacity
      
      ending_inventory <- numeric(n)
      backlog <- numeric(n)
      
      net_inventory <- input$initial_inventory
      
      for (i in seq_len(n)) {
        net_inventory <- net_inventory +
          production[i] - demand[i]
        
        ending_inventory[i] <- max(net_inventory, 0)
        backlog[i] <- max(-net_inventory, 0)
      }
      
      hired <- max(workforce - input$initial_workforce, 0)
      fired <- max(input$initial_workforce - workforce, 0)
      
      regular_cost <- workforce * days * input$wage
      
      hiring_cost <- c(
        hired * input$hiring_cost,
        rep(0, n - 1)
      )
      
      firing_cost <- c(
        fired * input$firing_cost,
        rep(0, n - 1)
      )
      
      inventory_cost <- ending_inventory * input$holding_cost
      backlog_cost <- backlog * input$backlog_cost
      
      total_cost <- regular_cost +
        hiring_cost + firing_cost +
        inventory_cost + backlog_cost
      
      data.frame(
        Period = seq_len(n),
        Demand = demand,
        Working_Days = days,
        Workforce = rep(workforce, n),
        Production = production,
        Ending_Inventory = ending_inventory,
        Backlog = backlog,
        Regular_Cost = regular_cost,
        Hiring_Cost = hiring_cost,
        Firing_Cost = firing_cost,
        Inventory_Cost = inventory_cost,
        Backlog_Cost = backlog_cost,
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
          "Inventory Holding",
          "Backlog",
          "Total Cost"
        ),
        Amount = c(
          sum(r$Regular_Cost),
          sum(r$Hiring_Cost),
          sum(r$Firing_Cost),
          sum(r$Inventory_Cost),
          sum(r$Backlog_Cost),
          sum(r$Total_Cost)
        )
      )
    }, digits = 2)
    
    output$production_plot <- renderPlot({
      r <- results()
      
      plot(
        r$Period, r$Demand,
        type = "o", pch = 19,
        col = "#2F6FB0",
        ylim = range(c(r$Demand, r$Production)),
        xlab = "Period", ylab = "Units",
        main = "Demand vs Production"
      )
      
      lines(
        r$Period, r$Production,
        type = "o", pch = 17,
        col = "#D95F02", lty = 2
      )
      
      legend(
        "topleft",
        legend = c("Demand", "Production"),
        col = c("#2F6FB0", "#D95F02"),
        pch = c(19, 17),
        lty = c(1, 2),
        bty = "n"
      )
    })
    
    output$inventory_plot <- renderPlot({
      r <- results()
      
      plot(
        r$Period, r$Ending_Inventory,
        type = "o", pch = 19,
        col = "#238B45",
        ylim = range(c(0, r$Ending_Inventory, r$Backlog)),
        xlab = "Period", ylab = "Units",
        main = "Ending Inventory and Backlog"
      )
      
      lines(
        r$Period, r$Backlog,
        type = "o", pch = 17,
        col = "#CB181D", lty = 2
      )
      
      legend(
        "topleft",
        legend = c("Inventory", "Backlog"),
        col = c("#238B45", "#CB181D"),
        pch = c(19, 17),
        lty = c(1, 2),
        bty = "n"
      )
    })
  })
}
