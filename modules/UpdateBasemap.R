### 2. Update the basemap when selection changes
UpdateBasemapUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("basemap"),
      "Choose Basemap:",
      choices = list("OpenStreetMap" = "OpenStreetMap", "Satellite" = "Satellite")
    )
  )
}

UpdateBasemapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$basemap, {
        leafletProxy("map") %>%
          clearTiles() %>%
          addProviderTiles(providers[[input$basemap]], group = "OpenStreetMap", layerId = "basemap")
      })  
    }
  )
}