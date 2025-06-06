executeShortestPath <- function(result_raster, all_points, session) {
  showNotification("Análise de caminho mais curto iniciada. Isso pode levar algum tempo.", type = "message")
  
  # Extract CRS from the raster
  crs_proj <- st_crs(result_raster)$proj4string
  # Report progress for coordinates extraction
  all_points <- st_transform(all_points, st_crs(crs_proj))
  
  # add all points to all shapefiles list
  updateShapefileList (all_points, "Pontos a conectar")
  
  # Perform shortest path analysis
  # Calculate the bounding box of all_points and add a 1 km buffer on all sides
  bbox <- st_bbox(all_points) + c(-1000, -1000, 1000, 1000)
  print("bbox")
  print(bbox)
  
  # Perform shortest path analysis
  # Create a terra extent directly using the bounding box
  terra_extent <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
  # # Crop the raster to this extent
  # cropped_raster <- mask(result_raster, terra_extent)
  cropped_raster <- crop(result_raster, terra_extent)
  
  start_time <- Sys.time()
  cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  # Create cost surface
  tr_cs <- create_cs(x = cropped_raster, neighbours = 8)
  end_time <- Sys.time()
  cat("create_cs finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total processing time: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
  
  # Perform the shortest path analysis
  start_time <- Sys.time()
  cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  # corridor <- create_FETE_lcps(x = tr_cs, locations = all_points, ncores = detectCores() - 1)
  # Check if 'all_points' is NULL
  if (is.null(all_points)) {
    stop("The 'all_points' object is NULL. Please provide a valid spatial points object.")
  }
  
  # Ensure 'all_points' is an sf object
  if (!inherits(all_points, "sf")) {
    all_points <- st_as_sf(all_points)
  }
  
  # Check if geometries in 'all_points' are valid
  if (!all(st_is_valid(all_points))) {
    stop("The 'all_points' object contains invalid geometries.")
  }
  corridor <- create_FETE_lcps(x = tr_cs, locations = all_points)
  end_time <- Sys.time()
  cat("create_FETE_lcps finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total processing time: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
  
  # Convert to sf object
  corridor_sf <- st_as_sf(corridor)
  
  # Transform corridor_sf to WGS 84 for Leaflet
  corridor_sf <- st_transform(corridor_sf, crs = 4326)
  corridor_sf <- corridor_sf %>%
    mutate(pair_id = ifelse(fromCell < toCell, paste(fromCell, toCell, sep = ","), paste(toCell, fromCell, sep = ","))) %>%
    distinct(pair_id, .keep_all = TRUE) %>%
    select(-pair_id)

  return(corridor_sf)
}

### 8. Function add the shortest path to map
addShortestPathToMap <- function(corridor_sf, map_id = "map", new_layer_name, color) {
  # Get the names of the current layers and add the new tab name to the list
  current_layers <- getCurrentLayerNames()
  current_layers <- c(current_layers, new_layer_name)
  
  leafletProxy(map_id) %>%
    # Add the outline polyline
    addPolylines(data = corridor_sf, color = "black", weight = 4, opacity = 1, group = new_layer_name) %>%
    # Add the main polyline
    addPolylines(data = corridor_sf, color = color, weight = 2, opacity = .7, group = new_layer_name) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = c(custom_control, current_layers),  # Ensure unique layers are listed
      options = layersControlOptions(collapsed = TRUE)
    )
}