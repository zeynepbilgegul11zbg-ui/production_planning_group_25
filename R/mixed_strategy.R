
mixed_strategy_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Mixed Strategy (Selected Workforce Changes)"),
    
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
        
        textInput(
          ns("change_periods"),
          "Workforce Change Periods",
          value = "2",
          placeholder = "Example: 2,4,6"
        ),
        
        textInput(
          ns("workforce_levels"),
          "New Workforce Levels",
          value = "2",
          placeholder = "Example: 3,4,2"
        ),
        
        helpText(
          "Enter matching comma-separated values. ",
          "For example, periods 2,4 and levels 3,2 ",
          "mean 3 workers from period 2 and ",
          "2 workers from period 4."
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
        h3("Mixed Strategy Results"),
        tableOutput(ns("results")),
        
        h3("Cost Summary"),
        tableOutput(ns("costs")),
        
        h3("Demand and Production"),
        plotOutput(ns("production_plot")),
        
        h3("Workforce by Period"),
        plotOutput(ns("workforce_plot")),
        
        h3("Inventory and Backlog"),
        plotOutput(ns("inventory_plot"))
      )
    )
  )
}


mixed_strategy_server <- function(id) {
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
        data(),
        input$demand_col,
        input$days_col,
        input$change_periods,
        input$workforce_levels
      )
      
      demand <- suppressWarnings(
        as.numeric(data()[[input$demand_col]])
      )
      
      days <- suppressWarnings(
        as.numeric(data()[[input$days_col]])
      )
      
      n <- length(demand)
      
      validate(
        need(input$demand_col != input$days_col,
             "Demand and working days must be different columns."),
        need(n > 0 && all(is.finite(demand)) &&
               all(is.finite(days)) &&
               all(demand >= 0) && all(days > 0),
             "Demand must be nonnegative and working days positive."),
        need(
          is.finite(input$initial_workforce) &&
            input$initial_workforce >= 0 &&
            input$initial_workforce %% 1 == 0,
          "Initial workforce must be a nonnegative integer."
        ),
        need(
          is.finite(input$productivity) &&
            input$productivity > 0,
          "Productivity must be positive."
        )
      )
      
      parse_numbers <- function(x) {
        x <- trimws(x)
        
        if (!nzchar(x)) {
          return(numeric(0))
        }
        
        suppressWarnings(
          as.numeric(trimws(strsplit(x, ",")[[1]]))
        )
      }
      
      change_periods <- parse_numbers(input$change_periods)
      workforce_levels <- parse_numbers(input$workforce_levels)
      
      validate(
        need(
          length(change_periods) == length(workforce_levels),
          "Change periods and workforce levels must have equal lengths."
        ),
        need(
          all(is.finite(change_periods)) &&
            all(change_periods >= 1) &&
            all(change_periods <= n) &&
            all(change_periods %% 1 == 0) &&
            !anyDuplicated(change_periods),
          "Change periods must be unique integers within the data range."
        ),
        need(
          all(is.finite(workforce_levels)) &&
            all(workforce_levels >= 0) &&
            all(workforce_levels %% 1 == 0),
          "Workforce levels must be nonnegative integers."
        )
      )
      
      workforce <- rep(input$initial_workforce, n)
      
      if (length(change_periods) > 0) {
        ord <- order(change_periods)
        change_periods <- change_periods[ord]
        workforce_levels <- workforce_levels[ord]
        
        current_workforce <- input$initial_workforce
        
        for (i in seq_len(n)) {
          match_index <- match(i, change_periods)
          
          if (!is.na(match_index)) {
            current_workforce <- workforce_levels[match_index]
          }
          
          workforce[i] <- current_workforce
        }
      }
      
      previous_workforce <- c(
        input$initial_workforce,
        head(workforce, -1)
      )
      
      hired <- pmax(workforce - previous_workforce, 0)
      fired <- pmax(previous_workforce - workforce, 0)
      
      capacity <- workforce * days * input$productivity
      
      # Full regular capacity is produced each period.
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
      
      regular_cost <- workforce * days * input$wage
      hiring_cost <- hired * input$hiring_cost
      firing_cost <- fired * input$firing_cost
      
      inventory_cost <- ending_inventory * input$holding_cost
      backlog_cost <- backlog * input$backlog_cost
      
      total_cost <- regular_cost +
        hiring_cost + firing_cost +
        inventory_cost + backlog_cost
      
      data.frame(
        Period = seq_len(n),
        Demand = demand,
        Working_Days = days,
        Workforce = workforce,
        Hired = hired,
        Fired = fired,
        Capacity = capacity,
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
        xlab = "Period",
        ylab = "Units",
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
    
    output$workforce_plot <- renderPlot({
      r <- results()
      
      plot(
        r$Period, r$Workforce,
        type = "s", lwd = 2,
        col = "#238B45",
        xlab = "Period",
        ylab = "Workers",
        main = "Workforce Changes"
      )
      
      points(
        r$Period, r$Workforce,
        pch = 19, col = "#238B45"
      )
    })
    
    output$inventory_plot <- renderPlot({
      r <- results()
      
      plot(
        r$Period, r$Ending_Inventory,
        type = "o", pch = 19,
        col = "#238B45",
        ylim = range(c(0, r$Ending_Inventory, r$Backlog)),
        xlab = "Period",
        ylab = "Units",
        main = "Inventory and Backlog"
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
