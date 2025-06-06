### Dynamic tabs for Future Scenario
# Function to create the UI for layer dropdowns and multipliers
FutureScenarioUI <- function() {
  # Get the current layer names
  # Create a fluidRow with a column containing the dropdown
  fluidRow(
    renderUI({
      createDownloadDropdown(id = "tabFutureScenario", multiple = FALSE, populated = FALSE, current_layers = names (all_shapefiles()))
    }),
    column(6,
           # Action button to calculate metric
           actionButton("calculate_FutureScenario_button", "Calculate Future Scenario"),
           # verbatimTextOutput("result_output")  # Output for displaying results
    )
  )
}