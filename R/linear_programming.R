
linear_programming_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Linear Programming - Aggregate Planning"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file"),
          "Upload Demand Data (Excel/CSV)",
          accept = c(".xlsx", ".xls", ".csv")
        ),
        
        uiOutput(ns("columns")),
        
        numericInput(
          ns("workforce"), "Constant Workforce",
          value = 2, min = 0, step = 1
        ),
        
        numericInput(
          ns("productivity"),
          "Units per Worker per Day",
          value = 5, min = 0.01
        ),
        
        numericInput(
          ns("overtime_ratio"),
          "Maximum Overtime (% of Regular Capacity)",
          value = 25, min = 0, max = 100
        ),
        
        numericInput(
          ns("initial_inventory"),
          "Initial Inventory",
          value = 0, min = 0
        ),
        
        numericInput(
          ns("regular_cost"),
          "Regular Production Cost per Unit",
          value = 20, min = 0
        ),
        
        numericInput(
          ns("overtime_cost"),
          "Overtime Production Cost per Unit",
          value = 30, min = 0
        ),
        
        numericInput(
          ns("idle_cost"),
          "Idle Capacity Cost per Unit",
          value = 0, min = 0
        ),
        
        numericInput(
          ns("subcontract_cost"),
          "Subcontracting Cost per Unit",
          value = 40, min = 0
        ),
        
        numericInput(
          ns("holding_cost"),
          "Inventory Holding Cost per Unit per Period",
          value = 2, min = 0
        ),
        
        numericInput(
          ns("subcontract_limit"),
          "Maximum Subcontracting per Period",
          value = 100, min = 0
        )
      ),
      
      mainPanel(
        h3("Optimization Status"),
        textOutput(ns("status")),
        
        h3("Optimal Production Plan"),
        tableOutput(ns("results")),
        
        h3("Cost Summary"),
        tableOutput(ns("costs")),
        
        h3("Production Plan"),
        plotOutput(ns("production_plot")),
        
        h3("Ending Inventory"),
        plotOutput(ns("inventory_plot"))
      )
    )
  )
}


linear_programming_server <- function(id) {
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
    
    solution <- reactive({
      req(
        data(), input$demand_col, input$days_col,
        input$workforce, input$productivity
      )
      
      validate(
        need(
          input$demand_col != input$days_col,
          "Select different demand and working days columns."
        )
      )
      
      demand <- suppressWarnings(
        as.numeric(data()[[input$demand_col]])
      )
      
      days <- suppressWarnings(
        as.numeric(data()[[input$days_col]])
      )
      
      validate(
        need(
          length(demand) > 0 &&
            all(is.finite(demand)) &&
            all(is.finite(days)) &&
            all(demand >= 0) &&
            all(days > 0),
          "Demand must be nonnegative and working days positive."
        )
      )
      
      parameters <- c(
        input$workforce,
        input$productivity,
        input$overtime_ratio,
        input$initial_inventory,
        input$regular_cost,
        input$overtime_cost,
        input$idle_cost,
        input$subcontract_cost,
        input$holding_cost,
        input$subcontract_limit
      )
      
      validate(
        need(
          all(is.finite(parameters)) &&
            all(parameters >= 0) &&
            input$productivity > 0 &&
            input$workforce %% 1 == 0,
          "Check the input parameters."
        )
      )
      
      n <- length(demand)
      
      capacity <- input$workforce *
        days * input$productivity
      
      overtime_capacity <- capacity *
        input$overtime_ratio / 100
      
      # Decision variables per period:
      # R = regular production
      # O = overtime production
      # S = subcontracting
      # I = ending inventory
      # U = idle regular capacity
      
      variable_names <- c("R", "O", "S", "I", "U")
      
      index <- function(variable, period) {
        (match(variable, variable_names) - 1) * n +
          period
      }
      
      number_variables <- 5 * n
      
      objective <- rep(0, number_variables)
      
      unit_costs <- c(
        R = input$regular_cost,
        O = input$overtime_cost,
        S = input$subcontract_cost,
        I = input$holding_cost,
        U = input$idle_cost
      )
      
      for (v in variable_names) {
        objective[index(v, seq_len(n))] <-
          unit_costs[v]
      }
      
      constraints <- matrix(
        0,
        nrow = 4 * n,
        ncol = number_variables
      )
      
      directions <- rep("=", 4 * n)
      rhs <- numeric(4 * n)
      
      for (t in seq_len(n)) {
        
        # Inventory balance:
        # R + O + S + previous inventory
        # = demand + ending inventory
        
        row_balance <- t
        
        constraints[
          row_balance,
          index("R", t)
        ] <- 1
        
        constraints[
          row_balance,
          index("O", t)
        ] <- 1
        
        constraints[
          row_balance,
          index("S", t)
        ] <- 1
        
        constraints[
          row_balance,
          index("I", t)
        ] <- -1
        
        if (t > 1) {
          constraints[
            row_balance,
            index("I", t - 1)
          ] <- 1
          
          rhs[row_balance] <- demand[t]
        } else {
          rhs[row_balance] <-
            demand[t] - input$initial_inventory
        }
        
        # Regular production + idle = capacity
        
        row_regular <- n + t
        
        constraints[
          row_regular,
          index("R", t)
        ] <- 1
        
        constraints[
          row_regular,
          index("U", t)
        ] <- 1
        
        rhs[row_regular] <- capacity[t]
        
        # Overtime capacity constraint
        
        row_overtime <- 2 * n + t
        
        constraints[
          row_overtime,
          index("O", t)
        ] <- 1
        
        directions[row_overtime] <- "<="
        rhs[row_overtime] <- overtime_capacity[t]
        
        # Subcontracting limit
        
        row_subcontract <- 3 * n + t
        
        constraints[
          row_subcontract,
          index("S", t)
        ] <- 1
        
        directions[row_subcontract] <- "<="
        rhs[row_subcontract] <- input$subcontract_limit
      }
      
      model <- lpSolve::lp(
        direction = "min",
        objective.in = objective,
        const.mat = constraints,
        const.dir = directions,
        const.rhs = rhs
      )
      
      validate(
        need(
          model$status == 0,
          paste(
            "No optimal solution found.",
            "Solver status:", model$status
          )
        )
      )
      
      x <- model$solution
      
      get_values <- function(variable) {
        pmax(0, x[index(variable, seq_len(n))])
      }
      
      regular <- get_values("R")
      overtime <- get_values("O")
      subcontract <- get_values("S")
      inventory <- get_values("I")
      idle <- get_values("U")
      
      regular_cost <- regular * input$regular_cost
      overtime_cost <- overtime * input$overtime_cost
      subcontract_cost <- subcontract *
        input$subcontract_cost
      inventory_cost <- inventory *
        input$holding_cost
      idle_cost <- idle * input$idle_cost
      
      results <- data.frame(
        Period = seq_len(n),
        Demand = demand,
        Working_Days = days,
        Regular_Capacity = capacity,
        Regular_Production = regular,
        Overtime_Production = overtime,
        Subcontracting = subcontract,
        Idle_Capacity = idle,
        Ending_Inventory = inventory,
        Total_Production = regular +
          overtime + subcontract,
        Total_Cost = regular_cost +
          overtime_cost +
          subcontract_cost +
          inventory_cost +
          idle_cost
      )
      
      costs <- data.frame(
        Cost_Component = c(
          "Regular Production",
          "Overtime Production",
          "Subcontracting",
          "Idle Capacity",
          "Inventory Holding",
          "Total Cost"
        ),
        Amount = c(
          sum(regular_cost),
          sum(overtime_cost),
          sum(subcontract_cost),
          sum(idle_cost),
          sum(inventory_cost),
          model$objval
        )
      )
      
      list(
        results = results,
        costs = costs,
        objective = model$objval
      )
    })
    
    output$status <- renderText({
      req(solution())
      "Optimal solution found."
    })
    
    output$results <- renderTable({
      solution()$results
    }, digits = 2)
    
    output$costs <- renderTable({
      solution()$costs
    }, digits = 2)
    
    output$production_plot <- renderPlot({
      r <- solution()$results
      
      production <- rbind(
        Regular = r$Regular_Production,
        Overtime = r$Overtime_Production,
        Subcontracting = r$Subcontracting
      )
      
      barplot(
        production,
        beside = FALSE,
        col = c("#2F6FB0", "#D95F02", "#238B45"),
        names.arg = r$Period,
        xlab = "Period",
        ylab = "Units",
        main = "Optimal Production Plan"
      )
      
      legend(
        "topright",
        legend = rownames(production),
        fill = c("#2F6FB0", "#D95F02", "#238B45"),
        bty = "n"
      )
    })
    
    output$inventory_plot <- renderPlot({
      r <- solution()$results
      
      plot(
        r$Period,
        r$Ending_Inventory,
        type = "o",
        pch = 19,
        col = "#238B45",
        xlab = "Period",
        ylab = "Ending Inventory",
        main = "Optimal Inventory Levels"
      )
    })
  })
}
