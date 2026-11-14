
transportation_model_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Transportation Model - Aggregate Planning"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          ns("file"),
          "Upload Demand Data (Excel/CSV)",
          accept = c(".xlsx", ".xls", ".csv")
        ),
        
        uiOutput(ns("columns")),
        
        numericInput(
          ns("workforce"),
          "Constant Workforce",
          value = 1, min = 0, step = 1
        ),
        
        numericInput(
          ns("productivity"),
          "Units per Worker per Day",
          value = 5, min = 0.01
        ),
        
        numericInput(
          ns("overtime_ratio"),
          "Maximum Overtime (%)",
          value = 25, min = 0, max = 100
        ),
        
        numericInput(
          ns("subcontract_limit"),
          "Maximum Subcontracting per Period",
          value = 100, min = 0
        ),
        
        numericInput(
          ns("regular_cost"),
          "Regular Production Cost per Unit",
          value = 20, min = 0
        ),
        
        numericInput(
          ns("overtime_cost"),
          "Overtime Cost per Unit",
          value = 30, min = 0
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
        
        selectInput(
          ns("method"),
          "Transportation Method",
          choices = c(
            "Northwest Corner",
            "Least Cost"
          )
        )
      ),
      
      mainPanel(
        h3("Transportation Status"),
        textOutput(ns("status")),
        
        h3("Transportation Allocation"),
        tableOutput(ns("allocation")),
        
        h3("Production by Period"),
        tableOutput(ns("production")),
        
        h3("Cost Summary"),
        tableOutput(ns("costs")),
        
        h3("Demand and Production"),
        plotOutput(ns("production_plot"))
      )
    )
  )
}


transportation_model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$file)
      
      ext <- tolower(tools::file_ext(input$file$name))
      
      validate(
        need(
          ext %in% c("csv", "xlsx", "xls"),
          "Please upload a CSV or Excel file."
        )
      )
      
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
        data(),
        input$demand_col,
        input$days_col,
        input$method
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
      
      parameters <- c(
        input$workforce,
        input$productivity,
        input$overtime_ratio,
        input$subcontract_limit,
        input$regular_cost,
        input$overtime_cost,
        input$subcontract_cost,
        input$holding_cost
      )
      
      validate(
        need(
          length(demand) > 0 &&
            all(is.finite(demand)) &&
            all(is.finite(days)) &&
            all(demand >= 0) &&
            all(days > 0),
          "Check demand and working days."
        ),
        
        need(
          all(is.finite(parameters)) &&
            all(parameters >= 0) &&
            input$productivity > 0 &&
            input$workforce %% 1 == 0 &&
            input$overtime_ratio <= 100,
          "Check the input parameters."
        )
      )
      
      n <- length(demand)
      
      regular_capacity <- input$workforce *
        days * input$productivity
      
      overtime_capacity <- regular_capacity *
        input$overtime_ratio / 100
      
      subcontract_capacity <- rep(
        input$subcontract_limit, n
      )
      
      sources <- data.frame(
        Source = rep(
          c("Regular", "Overtime", "Subcontract"),
          times = n
        ),
        
        Period = rep(seq_len(n), each = 3),
        
        Capacity = as.vector(
          rbind(
            regular_capacity,
            overtime_capacity,
            subcontract_capacity
          )
        ),
        
        Unit_Cost = rep(
          c(
            input$regular_cost,
            input$overtime_cost,
            input$subcontract_cost
          ),
          times = n
        )
      )
      
      m <- nrow(sources)
      
      # Transportation cost matrix.
      # Earlier production can supply later demand.
      # Later production cannot supply earlier demand.
      
      cost_matrix <- matrix(
        Inf, nrow = m, ncol = n
      )
      
      for (i in seq_len(m)) {
        for (j in seq_len(n)) {
          if (sources$Period[i] <= j) {
            cost_matrix[i, j] <-
              sources$Unit_Cost[i] +
              (j - sources$Period[i]) *
              input$holding_cost
          }
        }
      }
      
      # Verify that the available sources can
      # meet every demand deadline.
      
      feasible_remaining <- function(
    supply_left, demand_left) {
        
        if (sum(supply_left) + 1e-7 <
            sum(demand_left)) {
          return(FALSE)
        }
        
        if (sum(demand_left) < 1e-7) {
          return(TRUE)
        }
        
        allowed <- which(
          is.finite(cost_matrix),
          arr.ind = TRUE
        )
        
        k <- nrow(allowed)
        
        A <- matrix(
          0, nrow = m + n, ncol = k
        )
        
        for (a in seq_len(k)) {
          A[allowed[a, 1], a] <- 1
          A[m + allowed[a, 2], a] <- 1
        }
        
        result <- lpSolve::lp(
          direction = "min",
          objective.in = rep(0, k),
          const.mat = A,
          const.dir = c(
            rep("<=", m),
            rep("=", n)
          ),
          const.rhs = c(
            supply_left,
            demand_left
          )
        )
        
        result$status == 0
      }
      
      supply_left <- sources$Capacity
      demand_left <- demand
      
      validate(
        need(
          feasible_remaining(
            supply_left, demand_left
          ),
          paste(
            "Insufficient capacity to meet demand.",
            "Increase workforce, overtime,",
            "or subcontracting capacity."
          )
        )
      )
      
      allocation <- matrix(
        0, nrow = m, ncol = n
      )
      
      # Northwest Corner: scan the transportation matrix in
      # row-major order (upper-left first). Forbidden cells
      # are skipped. Check that remaining demand is feasible.
      # Least Cost: retain the existing chronological
      # demand-by-demand allocation rule.
      
      if (input$method == "Northwest Corner") {
        
        tolerance <- 1e-7
        
        while (sum(demand_left) > tolerance) {
          eligible <- which(
            outer(supply_left > tolerance,
                  demand_left > tolerance, "&") &
              is.finite(cost_matrix),
            arr.ind = TRUE
          )
          
          validate(need(nrow(eligible) > 0,
                        "No eligible transportation cells remain."))
          
          eligible <- eligible[
            order(eligible[, 1], eligible[, 2]),
            , drop = FALSE
          ]
          
          assigned <- FALSE
          
          for (k in seq_len(nrow(eligible))) {
            i <- eligible[k, 1]
            j <- eligible[k, 2]
            maximum <- min(supply_left[i], demand_left[j])
            
            trial_supply <- supply_left
            trial_demand <- demand_left
            trial_supply[i] <- trial_supply[i] - maximum
            trial_demand[j] <- trial_demand[j] - maximum
            
            if (feasible_remaining(trial_supply, trial_demand)) {
              quantity <- maximum
            } else {
              # Some constrained problems require only a
              # partial allocation to preserve feasibility.
              lower <- 0
              upper <- maximum
              for (iteration in seq_len(45)) {
                trial <- (lower + upper) / 2
                test_supply <- supply_left
                test_demand <- demand_left
                test_supply[i] <- test_supply[i] - trial
                test_demand[j] <- test_demand[j] - trial
                if (feasible_remaining(test_supply, test_demand)) {
                  lower <- trial
                } else {
                  upper <- trial
                }
              }
              quantity <- lower
            }
            
            if (quantity > tolerance) {
              allocation[i, j] <- allocation[i, j] + quantity
              supply_left[i] <- supply_left[i] - quantity
              demand_left[j] <- demand_left[j] - quantity
              supply_left[abs(supply_left) < tolerance] <- 0
              demand_left[abs(demand_left) < tolerance] <- 0
              assigned <- TRUE
              break
            }
          }
          
          validate(need(assigned,
                        "No feasible allocation found. Check capacities."))
        }
        
      } else {
        
        # Existing Least Cost method, preserved because
        # it passed the earlier numerical test.
        for (j in seq_len(n)) {
          while (demand_left[j] > 1e-7) {
            candidates <- which(
              supply_left > 1e-7 & is.finite(cost_matrix[, j])
            )
            
            validate(need(length(candidates) > 0,
                          "No feasible allocation found."))
            
            candidates <- candidates[
              order(cost_matrix[candidates, j],
                    sources$Period[candidates], candidates)
            ]
            
            assigned <- FALSE
            
            for (i in candidates) {
              maximum <- min(supply_left[i], demand_left[j])
              trial_supply <- supply_left
              trial_demand <- demand_left
              trial_supply[i] <- trial_supply[i] - maximum
              trial_demand[j] <- trial_demand[j] - maximum
              
              if (feasible_remaining(trial_supply, trial_demand)) {
                quantity <- maximum
              } else {
                lower <- 0
                upper <- maximum
                for (iteration in seq_len(45)) {
                  trial <- (lower + upper) / 2
                  test_supply <- supply_left
                  test_demand <- demand_left
                  test_supply[i] <- test_supply[i] - trial
                  test_demand[j] <- test_demand[j] - trial
                  if (feasible_remaining(test_supply, test_demand)) {
                    lower <- trial
                  } else {
                    upper <- trial
                  }
                }
                quantity <- lower
              }
              
              if (quantity > 1e-7) {
                allocation[i, j] <- allocation[i, j] + quantity
                supply_left[i] <- supply_left[i] - quantity
                demand_left[j] <- demand_left[j] - quantity
                supply_left[abs(supply_left) < 1e-7] <- 0
                demand_left[abs(demand_left) < 1e-7] <- 0
                assigned <- TRUE
                break
              }
            }
            
            validate(need(assigned,
                          "Allocation could not be completed."))
          }
        }
      }
      
      allocation[abs(allocation) < 1e-6] <- 0
      
      allocation_table <- data.frame(
        Source = sources$Source,
        Production_Period = sources$Period,
        Capacity = sources$Capacity
      )
      
      for (j in seq_len(n)) {
        allocation_table[[
          paste0("Demand_P", j)
        ]] <- allocation[, j]
      }
      
      allocation_table$Used_Capacity <-
        rowSums(allocation)
      
      allocation_table$Unused_Capacity <-
        pmax(
          0,
          sources$Capacity -
            allocation_table$Used_Capacity
        )
      
      # Dummy demand represents unused production capacity.
      # Its transportation cost is zero. This balances the
      # displayed transportation table without changing
      # either allocation heuristic.
      allocation_table$Dummy_Unused <-
        allocation_table$Unused_Capacity
      
      allocation_table$Cost <- rowSums(
        allocation *
          ifelse(
            is.finite(cost_matrix),
            cost_matrix,
            0
          )
      )
      
      regular <- numeric(n)
      overtime <- numeric(n)
      subcontract <- numeric(n)
      
      for (t in seq_len(n)) {
        
        regular[t] <- sum(
          allocation[
            sources$Source == "Regular" &
              sources$Period == t,
            ,
            drop = FALSE
          ]
        )
        
        overtime[t] <- sum(
          allocation[
            sources$Source == "Overtime" &
              sources$Period == t,
            ,
            drop = FALSE
          ]
        )
        
        subcontract[t] <- sum(
          allocation[
            sources$Source == "Subcontract" &
              sources$Period == t,
            ,
            drop = FALSE
          ]
        )
      }
      
      total_production <- regular +
        overtime + subcontract
      
      ending_inventory <- numeric(n)
      stock <- 0
      
      for (t in seq_len(n)) {
        stock <- stock +
          total_production[t] - demand[t]
        
        ending_inventory[t] <- stock
      }
      
      regular_cost <- sum(regular) *
        input$regular_cost
      
      overtime_cost <- sum(overtime) *
        input$overtime_cost
      
      subcontract_cost <- sum(subcontract) *
        input$subcontract_cost
      
      inventory_cost <- sum(
        pmax(ending_inventory, 0)
      ) * input$holding_cost
      
      total_cost <- regular_cost +
        overtime_cost +
        subcontract_cost +
        inventory_cost
      
      production_table <- data.frame(
        Period = seq_len(n),
        Demand = demand,
        Regular_Production = regular,
        Overtime_Production = overtime,
        Subcontracting = subcontract,
        Total_Production = total_production,
        Ending_Inventory = pmax(
          ending_inventory, 0
        )
      )
      
      cost_table <- data.frame(
        Cost_Component = c(
          "Regular Production",
          "Overtime Production",
          "Subcontracting",
          "Inventory Holding",
          "Total Cost"
        ),
        Amount = c(
          regular_cost,
          overtime_cost,
          subcontract_cost,
          inventory_cost,
          total_cost
        )
      )
      
      list(
        allocation = allocation_table,
        production = production_table,
        costs = cost_table,
        total_cost = total_cost
      )
    })
    
    output$status <- renderText({
      req(solution())
      
      if (input$method == "Northwest Corner") {
        "Adapted Northwest Corner allocation completed (feasibility-constrained)."
      } else {
        "Least Cost allocation completed."
      }
    })
    
    output$allocation <- renderTable({
      solution()$allocation
    }, digits = 2)
    
    output$production <- renderTable({
      solution()$production
    }, digits = 2)
    
    output$costs <- renderTable({
      solution()$costs
    }, digits = 2)
    
    output$production_plot <- renderPlot({
      r <- solution()$production
      
      production <- rbind(
        Regular = r$Regular_Production,
        Overtime = r$Overtime_Production,
        Subcontracting = r$Subcontracting
      )
      
      barplot(
        production,
        col = c(
          "#2F6FB0",
          "#D95F02",
          "#238B45"
        ),
        names.arg = r$Period,
        xlab = "Period",
        ylab = "Units",
        main = "Production by Period"
      )
      
      legend(
        "topright",
        legend = rownames(production),
        fill = c(
          "#2F6FB0",
          "#D95F02",
          "#238B45"
        ),
        bty = "n"
      )
    })
  })
}
