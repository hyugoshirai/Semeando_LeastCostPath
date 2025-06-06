### 1.  Function to create a color palette based on the raster values
processShapefile <- function(shapefile_zip, shapefile_data) {
  if (!is.null(shapefile_zip) && endsWith(shapefile_zip$name, ".zip")) {
    tmpdir <- tempdir()
    
    # Unzip the contents of the ZIP file to a temporary directory
    unzip(shapefile_zip$datapath, exdir = tmpdir, junkpaths = TRUE)
    # List all shapefiles in the temporary directory
    shp_files <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) > 0) {
      # Retrieve file information including modification times
      file_info <- file.info(shp_files)
      # Sort files by modification time (most recent first)
      shp_files <- shp_files[order(file_info$atime, decreasing = TRUE)]

      # Read the most recently modified shapefile
      shapefile_data <- st_read(shp_files[1])
      showNotification(paste("Shapefile", shp_files[1], "successfully read."))
      return(shapefile_data)
    }
  } else {
    showNotification("Please upload a ZIP file containing the shapefile.")
  }
}

### 3.  Function to create a color palette based on the raster values
createColorPalette <- function(min_value, max_value) {
  values_range <- c(min_value, max_value)
  colorNumeric(
    palette = c("#FF0000", "#FFFF00", "#00FFFF", "#0000FF"),
    domain = values_range,
    na.color = "transparent"  # Set NA values to transparent
  )
}

### 7. Function to create and transform an sf point
create_and_transform_sf <- function(coords, crs_from, crs_to) {
  sf_point <- st_sf(geometry = st_sfc(st_point(coords), crs = crs_from))
  transformed_sf <- st_transform(sf_point, crs = crs_to)
  return(st_coordinates(transformed_sf)[1, 1:2])
}

### 8. Function to rasterize the shapefile
rasterizeShapefile <- function(shapefile, LandUse_rst) {
  # Ensure the shapefile is loaded correctly
  if (is.null(shapefile)) {
    showNotification("Shapefile is NULL", type = "error")
    return(NULL)
  }
  
  # Project the shapefile to match the raster CRS
  shapefile <- st_transform(shapefile, crs = st_crs(LandUse_rst))
  
  # Create a blank raster with the same extent, resolution, and CRS as LandUse_rst
  r <- raster(ext = extent(LandUse_rst), resolution = res(LandUse_rst), crs = crs(LandUse_rst))
  
  # Let the user choose the field to rasterize, e.g., "value"
  field <- "value"
  
  # Convert field to numeric if it's not already
  if (!is.numeric(shapefile[[field]])) {
    shapefile[[field]] <- as.numeric(as.character(shapefile[[field]]))
  }
  
  # Rasterize the shapefile with error handling
  raster_layer <- tryCatch({
    # Rasterize the shapefile
    rasterized <- raster::rasterize(shapefile, r, field = field)
    
    # Return the rasterized layer
    return(rasterized)
  }, error = function(e) {
    # Handle any errors that occur during rasterization
    showNotification(paste("Error rasterizing shapefile:", e$message), type = "error")
    traceback()  # Display traceback information
    return(NULL)
  })
  
  # Return the raster layer
  return(raster_layer)
}

### 9. Function to get the names of the current raster layers
getCurrentLayerNames <- reactive({
  # raster_names <- names(all_reclassified_rasters())
  aggregated_raster_names <- names(all_reclassified_rasters_aggregated())
  # shapefile_list <- shapefile_names_list()
  shapefiles_names <- names (all_shapefiles())
  # Combine raster names with shapefile names
  # c(names(Uso_do_solo), raster_names, shapefiles_names)#, "Result raster", shapefile_list)
  c(shapefiles_names, aggregated_raster_names)#, "Result raster", shapefile_list)
})

### 10. Define the function to replace NA values with zero
replaceNAwithZero <- function(raster_layer) {
  # Ensure input is a RasterLayer
  if (is(raster_layer, "RasterLayer")) {
    # Replace NA values with zero
    calc(raster_layer, fun = function(x) ifelse(is.na(x), 0, x))
  } else {
    stop("Input must be a RasterLayer")
  }
}

### 11. Function to execute the euclidean distance calculation
euclideanDistance <- function(raster_layer) {
  # Ensure input is a RasterLayer
  if (is(raster_layer, "RasterLayer")) {
    # Calculate the Euclidean distance
    euclidean_raster <- raster::distance(raster_layer)
    return(euclidean_raster)
  } else {
    stop("Input must be a RasterLayer")
  }
}

### 12. Function to reclassify a raster based on a reclassification data frame with min and maximum values
reclassify_raster <- function(raster, reclass_df) {
  # Convert the reclassification data frame into a matrix for easier lookup
  reclass_matrix <- as.matrix(reclass_df[, c("min_val", "max_val", "value")])
  
  # Create a function to apply reclassification
  classify_value <- function(value) {
    # Find the row in reclass_matrix where the value falls into the interval
    interval_index <- which(reclass_matrix[, "min_val"] <= value & reclass_matrix[, "max_val"] >= value)
    if (length(interval_index) == 0) return(NA)  # Return NA if value does not fall into any interval
    return(reclass_matrix[interval_index [1], "value"])
  }
  
  # Apply reclassification to each cell in the raster
  raster_reclassified <- calc(raster, fun = classify_value)
  return(raster_reclassified)
}

### 13. Function to generate a random color
generateColor <- function() {
  colors <- c("#FF0000", "#0000FF", "#FFFF00", "#00FF00", "#FFA500", "#800080", "#00FFFF", "#FF00FF", "#000000", "#808080")
  return(sample(colors, 1))
}

### 14.  Function to create the UI for all dropdowns
createDownloadDropdown <- function(id, multiple = TRUE, populated = FALSE, current_layers) {
  # Get the current layer names
  # Create a fluidRow with one column containing the dropdown
  fluidRow(
    column(6,
           # Dropdown for raster selection with multiple selection enabled
           selectInput(id,
                       label = "Select Layers:",
                       choices = c("Select Layers" = "", current_layers),
                       multiple = multiple  # Allow multiple selection
           )
    )
  )
}


# Function to update all shapefiles list
updateShapefileList <- function(shapefile, shapefile_name) {
  # Get the current shapefile names
  current_shps <- all_shapefiles()
  
  # Add the new shapefile name to the list
  current_shps[[shapefile_name]]  <- shapefile
  
  # Update the reactive value
  all_shapefiles(current_shps)
  # updateFeatureLabels()
}


# Function to replace NA values with zero in a raster
replaceNAwithZero <- function(raster_layer) {
  raster_layer[is.na(raster_layer)] <- 0
  return(raster_layer)
}

# Combine sf objects from list to a single sf object for downloading all the points
combine_sf_objects <- function(sf_list) {
  combined_sf <- do.call(rbind, sf_list)
  return(st_as_sf(combined_sf))
}

# Function to update the reactive list
updateList <- function(layer, name, reactive_list) {
  # Get the current layers
  current_layer <- reactive_list()
  
  # Add or update the layer with the given name
  current_layer[[name]] <- layer
  
  # Update the reactive list
  reactive_list(current_layer)
  
  # Print for debugging (optional)
  print(paste("Updated list:", name))
  print(reactive_list())
}

# Function to create HTML popups, handling NA values dynamically
create_popup <- function(fields) {
  popup_content <- sapply(fields, function(field) {
    value <- field$value
    if (is.na(value)) {
      value <- ""
    }
    paste0("<strong>", field$name, "</strong>: ", value, "<br/>")
  })
  paste(popup_content, collapse = "")
}