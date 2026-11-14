
disaggregation_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Disaggregation - Square Root Rule"),
    
    sidebarLayout(
      sidebarPanel(
        
        numericInput(
          ns("current_inventory"),
          "Current Total Safety Stock (Units)",
          value = 2000,
          min = 0
        ),
        
        numericInput(
          ns("current_locations"),
          "Current Number of Locations",
          value = 4,
          min = 1,
          step = 1
        ),
        
        numericInput(
          ns("new_locations"),
          "New Number of Locations",
          value = 9,
          min = 1,
          step = 1
        ),
        
        actionButton(
          ns("calculate"),
          "Calculate"
        )
      ),
      
      mainPanel(
        
        h3("Calculation Status"),
        textOutput(ns("status")),
        
        h3("Disaggregation Results"),
        tableOutput(ns("results")),
        
        h3("Inventory Comparison"),
        plotOutput(ns("inventory_plot")),
        
        h3("Interpretation"),
        verbatimTextOutput(ns("interpretation")),
        
        h3("Formula"),
        helpText(
          "New Safety Stock = Current Safety Stock *",
          "sqrt(New Locations / Current Locations)"
        )
      )
    )
  )
}


disaggregation_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    calculation <- eventReactive(
      input$calculate,
      {
        
        inventory <- input$current_inventory
        n1 <- input$current_locations
        n2 <- input$new_locations
        
        validate(
          need(
            all(is.finite(c(inventory, n1, n2))),
            "Please enter valid numeric values."
          ),
          
          need(
            inventory >= 0,
            "Current inventory cannot be negative."
          ),
          
          need(
            n1 >= 1 && n2 >= 1 &&
              n1 %% 1 == 0 &&
              n2 %% 1 == 0,
            "Location counts must be positive integers."
          )
        )
        
        new_inventory <- inventory * sqrt(n2 / n1)
        
        inventory_change <- new_inventory - inventory
        
        percentage_change <- if (inventory > 0) {
          (inventory_change / inventory) * 100
        } else {
          NA_real_
        }
        
        results <- data.frame(
          Metric = c(
            "Current Total Safety Stock",
            "Current Locations",
            "New Locations",
            "Estimated New Safety Stock",
            "Inventory Change",
            "Percentage Change (%)"
          ),
          
          Value = c(
            inventory,
            n1,
            n2,
            new_inventory,
            inventory_change,
            percentage_change
          )
        )
        
        list(
          current_inventory = inventory,
          new_inventory = new_inventory,
          current_locations = n1,
          new_locations = n2,
          inventory_change = inventory_change,
          percentage_change = percentage_change,
          results = results
        )
      }
    )
    
    output$status <- renderText({
      req(calculation())
      "Square Root Rule calculation completed."
    })
    
    output$results <- renderTable({
      calculation()$results
    }, digits = 2)
    
    output$inventory_plot <- renderPlot({
      
      r <- calculation()
      
      values <- c(
        r$current_inventory,
        r$new_inventory
      )
      
      barplot(
        values,
        names.arg = c(
          "Current Inventory",
          "New Inventory"
        ),
        col = c("#2F6FB0", "#238B45"),
        main = "Safety Stock Comparison",
        ylab = "Total Safety Stock (Units)",
        ylim = c(0, max(1, values) * 1.2)
      )
    })
    
    output$interpretation <- renderText({
      
      r <- calculation()
      
      if (r$current_inventory == 0) {
        
        return(
          "Current safety stock is zero. The square root rule estimates zero safety stock for the new network."
        )
      }
      
      if (r$inventory_change > 0) {
        
        paste0(
          "Increasing the number of locations from ",
          r$current_locations,
          " to ",
          r$new_locations,
          " increases estimated total safety stock by ",
          round(r$percentage_change, 2),
          "%. The new estimated safety stock is ",
          round(r$new_inventory, 2),
          " units."
        )
        
      } else if (r$inventory_change < 0) {
        
        paste0(
          "Reducing the number of locations from ",
          r$current_locations,
          " to ",
          r$new_locations,
          " decreases estimated total safety stock by ",
          round(abs(r$percentage_change), 2),
          "%. The new estimated safety stock is ",
          round(r$new_inventory, 2),
          " units."
        )
        
      } else {
        
        "The number of locations has not changed. Estimated total safety stock remains the same."
      }
    })
  })
}
