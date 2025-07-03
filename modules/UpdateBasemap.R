# ### 2. Update the basemap when selection changes
# UpdateBasemapUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     selectInput(
#       ns("basemap"),
#       "Choose Basemap:",
#       choices = list("OpenStreetMap" = "OpenStreetMap", "Satellite" = "Satellite")
#     )
#   )
# }
# 
# UpdateBasemapServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       observeEvent(input$basemap, {
#         leafletProxy("map") %>%
#           clearTiles() %>%
#           addProviderTiles(providers[[input$basemap]], group = "OpenStreetMap", layerId = "basemap")
#       })  
#     }
#   )
# }
# 
# # This module provides a UI for selecting a basemap and toggling overlay layers on a Leaflet map.
# MapControlsUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     selectInput(
#       ns("basemap"),
#       "Escolha o Mapa Base:",
#       choices = c("OpenStreetMap", "Satellite")
#     ),
#     checkboxGroupInput(
#       ns("overlays"),
#       "Sobreposições visíveis:",
#       choices = custom_control,
#       selected = c("Imóveis", "Uso do solo")
#     )
#   )
# }
# 
# ### 2. NEW Update the basemap when selection changes
# # Function to observe and handle base layer switching
# MapControlsServer <- function(id, mapId, overlay_groups) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       # BASMAP SELECTOR - Only one shown at a time, not via showGroup/hideGroup!
#       observeEvent(input$basemap, {
#         proxy <- leafletProxy(mapId)
#         proxy %>% clearTiles()
#         if (input$basemap == "OpenStreetMap") {
#           proxy %>% addTiles(group = "OpenStreetMap")
#         } else if (input$basemap == "Satellite") {
#           proxy %>% addTiles(
#             urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
#             group = "Satellite"
#           )
#         }
#       })
#       # OVERLAY CHECKBOX HANDLER
#       observe({
#         req(input$overlays)
#         for (g in overlay_groups) {
#           leafletProxy(mapId) %>%
#             { if (g %in% input$overlays) showGroup(., g) else hideGroup(., g) }
#         }
#       })
#     }
#   )
# }