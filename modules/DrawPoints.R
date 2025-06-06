DrawPoints <- function(input, output, session, all_points) {
  ### Handle Point Drawing
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    print (feature)
    feature_id <- feature$properties$`_leaflet_id`# Extract the feature ID
    print (feature_id)
    # Extract coordinates
    lng <- as.numeric(feature$geometry$coordinates[[1]])
    lat <- as.numeric(feature$geometry$coordinates[[2]])
    
    # Create new point
    coords <- unlist(feature$geometry$coordinates) # Extract and unlist the coordinates for point
    print (coords)
    feature_sf <- st_sfc(st_point(coords), crs = 4326) # Create the sf object for point
    feature_sf <- st_sf(id = feature_id, geometry = feature_sf) # Add the name as a column in the sf object
    print (feature_sf)
    
    # Get the current points and append the new point
    current_points <- all_points()
    print (current_points)
    current_points[[as.character(feature_id)]] <- feature_sf # Add the new feature to the list
    
    # Update the reactive value
    all_points(current_points)
    print ("all_points reactive value")
    print (all_points)
    
    updateShapefileList (all_points, "Pontos a conectar")
    
    showNotification(
      HTML(paste("Added point with<br>Latitude:", lat, "<br>Longitude:", lng)),
      type = "message"
    )
  })
  
  ### Handle Point Editing
  # Observer for edited features
  observeEvent(input$map_draw_edited_features, {
    feature <- input$map_draw_edited_features
    feature_id <- feature$features[[1]]$properties$`_leaflet_id`
    # Extract coordinates
    lng <- as.numeric(feature$features[[1]]$geometry$coordinates[[1]])
    lat <- as.numeric(feature$features[[1]]$geometry$coordinates[[2]])
    
    # Create new point
    coords <- feature$features[[1]]$geometry$coordinates
    coords <- unlist(coords)  # Flatten the list if necessary
    feature_sf <- st_sfc(st_point(coords), crs = 4326)
    
    # Get the current points and append the new point
    current_points <- all_points()
    current_points[[as.character(feature_id)]]$geometry <- feature_sf
    
    # Update all points list
    all_points(current_points)
  
    updateShapefileList (all_points, "Pontos a conectar")
    
  
    showNotification(
      HTML(paste("Edited point with<br>Latitude:", lat, "<br>Longitude:", lng)),
      type = "message"
    )
  })
  
  ### Handle Point Deleting
  observeEvent(input$map_draw_deleted_features, {
    feature <- input$map_draw_deleted_features
    feature_id <- feature$features[[1]]$properties$`_leaflet_id`
    # Extract coordinates
    lng <- as.numeric(feature$features[[1]]$geometry$coordinates[[1]])
    lat <- as.numeric(feature$features[[1]]$geometry$coordinates[[2]])
    
    # Create new point
    coords <- feature$features[[1]]$geometry$coordinates
    coords <- unlist(coords)  # Flatten the list if necessary
    feature_sf <- st_sfc(st_point(coords))
    
    # Get the current points and delete from current points
    current_points <- all_points()
    current_points <- current_points[!names(current_points) %in% feature_id] # Remove the deleted features
    
    # Update all points list
    all_points(current_points)
    
    updateShapefileList (all_points, "Pontos a conectar")
    
    showNotification(
      HTML(paste("Deleted point with<br>Latitude:", lat, "<br>Longitude:", lng)),
      type = "message"
    )
  })
}