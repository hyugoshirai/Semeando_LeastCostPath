### 6. Function to add rasters to the map and update them to the combined legend
addRasterToMap <- function(raster, name) {
  
  factor <- 5
  start_time <- Sys.time()
  cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  aggregated_raster <- raster::aggregate(raster, fact = factor, fun = modal)
  # aggregated_raster <- raster
  # Record the end time
  end_time <- Sys.time()
  cat("Processing finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total aggregate processing time: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
  layer_name <- paste (name, "aggregated", sep = " ")
  
  # Masking value 0
  mask_raster <- aggregated_raster == 0
  aggregated_raster[mask_raster] <- NA
  
  # Update the reactive list of aggregated rasters
  updateList (aggregated_raster, layer_name, all_reclassified_rasters_aggregated)

  # Function to extract all values from current rasters for the combined legend
  all_reclassified_rasters_values <- function(rasters) {
    all_values <- NULL
    for (r in rasters) {
      all_values <- c(all_values, values(r))
    }
    return(all_values)
  }
  # Assuming all_reclassified_rasters() returns a list of rasters
  rasters <- c(all_reclassified_rasters(),result_raster())
  print ("rasters")
  print (rasters)
  
  # Get all values from rasters
  all_values <- all_reclassified_rasters_values(rasters)
  
  # Efficient calculation of min and max values, excluding NA
  min_value <- min(all_values, na.rm = TRUE)
  max_value <- max(all_values, na.rm = TRUE)
  
  # Output the min and max values
  cat("Min value:", min_value, "\n")
  cat("Max value:", max_value, "\n")
  
   # Adjust min_value and max_value
   if (min_value == max_value || max_value > 0) {
     min_value <- 0
   } else {
     max_value <- 0
   }
  
   # Create color palette for the reclassified raster
   color_palette <- createColorPalette(min_value, max_value)
   
   # Get the names of the current dynamically added layers
   current_layers <- getCurrentLayerNames()
   # current_layers <-  names(all_reclassified_rasters_aggregated())
   
   # Ensure the reclassified raster has been updated
   if (is.null(raster)) {
     return(NULL)
   }
  
   # Use leafletProxy to clear only dynamically added raster layers and their legends
   proxy <- leafletProxy("map")
   # Clear existing dynamically added raster images and controls
   for (layer_name in names(rasters)) {
         cat(paste("Clearing layer", layer_name, "\n"))

     proxy <- proxy %>%
       clearGroup(layer_name)
   }
   all_reclassified_rasters_aggregated <- all_reclassified_rasters_aggregated()

   # Add updated reclassified raster layers
   for (layer_name in names(all_reclassified_rasters_aggregated)) {
     print("layer_name")
     print(layer_name)
     cat(paste("Adding layer", layer_name, "\n"))
     proxy <- proxy %>%
       addRasterImage(all_reclassified_rasters_aggregated [[layer_name]],
                      colors = color_palette,
                      group = layer_name,
                      project = TRUE,
                      method = 'ngb')
     proxy <- proxy %>%
       addLegend(
         pal = color_palette,
         values = c(min_value, max_value),
         title = "Prioridades (do menor para maior)",
         position = "bottomright",
         layerId = "persistent_legend",
         className = "small-legend"
       ) %>%
       addLayersControl(
         baseGroups = c("OpenStreetMap", "Satellite"),
         overlayGroups = c(custom_control,current_layers),
         options = layersControlOptions(collapsed = TRUE)
       )
   }
   }