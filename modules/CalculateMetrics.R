### 11. Dynamic tabs for calculate metrics
# Function to create the UI for layer dropdowns and multipliers
createLayerUI <- function() {
  # Get the current layer names
  # Create a fluidRow with a column containing the dropdown
  fluidRow(
      renderUI({
      createDownloadDropdown(id = "tabMetrics", multiple = FALSE, populated = FALSE, current_layers = names (all_shapefiles()))
    }),
    column(6,
           # Conditionally render the shapefile or raster inputs based on selection
           DTOutput("cost_per_lu"),
           # Action button to calculate metric
           actionButton("calculate_metric_button", "Calculate Costs", icon = icon("calculator")),
           verbatimTextOutput("result_output")  # Output for displaying results
    )
  )
}

# Function to create input fields for a shapefile
createShapefileInputs <- function(index, layer_name) {
  tagList(
    h4("Costs per land use type"),
    lapply(seq_along(land_use_labels), function(i) {
      numericInput(paste0("cost_", index, "_", i),
                   label = land_use_labels[i],
                   value = 1
      )
    })
  )
}

CostsTable <- function(id, input, output, session, LandUse_rst, costs_landuse_df, reactive_costs) {
  # costs_landuse_df_rv <- reactiveVal(costs_landuse_df)
  # Render the data table
  output[[id]] <- renderDT({
    datatable(costs_landuse_df_rv(), editable = list(target = "cell", disable = list(columns = c(1))))
  })
  # Observe cell edit events
  observeEvent(input[[paste0(id, "_cell_edit")]], {
    new_data <- input[[paste0(id, "_cell_edit")]]
    row <- new_data$row
    col <- new_data$col
    value <- as.numeric(new_data$value)
    
    # Update the costs_landuse_df reactive value
    updated_df <- costs_landuse_df_rv()
    updated_df[row, col] <- value
    costs_landuse_df_rv(updated_df)  # Update the reactive value
    print ("costs_landuse_df_rv()")
    print(costs_landuse_df_rv())
    
})
}
  