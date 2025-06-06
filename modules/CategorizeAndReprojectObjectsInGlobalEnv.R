# Function to classify objects in the global environment and reproject sf objects
CategorizeAndReprojectObjectsInGlobalEnv <- function() {
  # Get all object names in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Loop through each object to check its class and categorize
  for (object_name in all_objects) { 
    obj <- get(object_name, envir = .GlobalEnv)
    
    # Check if the object is a GeoJSON (sf)
    if (inherits(obj, "sf")) {
      # Reproject to WGS 84 (EPSG:4326)
      obj <- st_transform(obj, crs = 4326)
      # Update the object in the global environment
      assign(object_name, obj, envir = .GlobalEnv)
      # Assign to the default_shapefiles list
      default_shapefiles[[object_name]] <- obj
    }
    
    # Check if the object is a Raster (SpatRaster)
    if (inherits(obj, "SpatRaster")) {
      # Assign to the default_layers list
      default_layers[[object_name]] <- obj
    }
  }
  
  # Print a summary of categorized objects
  message("Shapefiles assigned to list: ", paste(names(default_shapefiles), collapse = ", "))
  message("Rasters assigned to list: ", paste(names(default_layers), collapse = ", "))
  
  # Assign the lists to the global environment
  assign("default_shapefiles", default_shapefiles, envir = .GlobalEnv)
  assign("default_layers", default_layers, envir = .GlobalEnv)
}

# Example usage:
# Call the function to categorize and reproject already loaded objects
# CategorizeAndReprojectObjectsInGlobalEnv()