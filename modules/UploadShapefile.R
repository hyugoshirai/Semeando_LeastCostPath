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
      if (!"name" %in% names(shapefile_data)) {
        shapefile_data$name <- paste0("ID_", seq_len(nrow(shapefile_data)))
        showNotification("The 'name' attribute was not found and has been created with unique IDs.")
      }
      # Create a new field called 'value' and assign it the value 1
      shapefile_data$value <- 1
      showNotification(paste("Shapefile", shp_files[1], "successfully read."))
      return(shapefile_data)
    }
  } else {
    showNotification("Please upload a ZIP file containing the shapefile.")
  }
}



# # Render the data table with editable cells
# ShapefileTable <- function(id, input, output, session, shapefile_name, shapefile_data_processed) {
#   # Render the data table
#   output[[id]] <- renderDT({
#     datatable(shapefile_data_processed[,c("name","value")], editable = list(target = "cell", disable = list(columns = c(0,2))),
#               rownames = FALSE,
#               options = list(pageLength = 10, scrollX = TRUE)  # Adjust options as needed
#     )
#   })
# 
#   # Observe cell edits
#   observeEvent(input[[paste0(id, "_cell_edit")]], {
#     new_data <- input[[paste0(id, "_cell_edit")]]
#     row <- new_data$row
#     value <- as.numeric(new_data$value)
# 
#     # Ensure the value is not less than 2
#     if (value < 2) {
#       showNotification("Value must be greater than or equal to 2.", type = "warning")
#       value <- 2
#     }
# 
#     # Update the reactive data with the new value
#     updated_data <- shapefile_data_processed
#     updated_data[row, "value"] <- value
#     shapefile_data_processed(updated_data)  # Store the updated reactiveVal
#     
#   })
# }

### 12. Function to rasterize the shapefile
rasterizeShapefile <- function(shapefile, LandUse_rst) {
  # Ensure the shapefile is loaded correctly
  if (is.null(shapefile)) {
    showNotification("Shapefile is NULL", type = "error")
    return(NULL)
  }
  
    # Project the shapefile to match the raster CRS
    shapefile <- st_transform(shapefile, crs = st_crs(LandUse_rst))

    # Create a blank raster with the same extent, resolution, and CRS as LandUse_rst
    r <- terra::rast(ext(LandUse_rst), resolution = res(LandUse_rst), crs = crs(LandUse_rst)) 
    
    # Ensure the chosen field exists in the shapefile
    field <- "value"  # Default field to rasterize
    if (!field %in% names(shapefile)) {
      showNotification(paste("Field", field, "does not exist in the shapefile."), type = "error")
      return(NULL)
    }
    
    # Convert field to numeric if it's not already
    if (!is.numeric(shapefile[[field]])) {
      shapefile[[field]] <- as.numeric(as.character(shapefile[[field]]))
    }
    
    # Check for NA values and handle accordingly
    if (any(is.na(shapefile[[field]]))) {
      showNotification("Field contains NA values. These will be set to 0 in the raster.", type = "warning")
      shapefile[[field]][is.na(shapefile[[field]])] <- 0
    }
    
    # Rasterize the shapefile
    rasterized <- raster::rasterize(shapefile, r, field = field, fun = mean)
    
    return(rasterized)
}