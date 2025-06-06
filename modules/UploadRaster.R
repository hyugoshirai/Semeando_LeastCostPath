# processRaster <- function(raster_zip, raster_data) {
#   if (!is.null(raster_zip) && endsWith(raster_zip$name, ".zip")) {
#     tmpdir <- tempdir()
#     
#     # Unzip the contents of the ZIP file to a temporary directory
#     unzip(raster_zip$datapath, exdir = tmpdir, junkpaths = TRUE)
#     # List all rasters in the temporary directory
#     rst_files <- list.files(tmpdir, pattern = "\\.tif$", full.names = TRUE)
#     
#     if (length(rst_files) > 0) {
#       # Retrieve file information including modification times
#       file_info <- file.info(rst_files)
#       # Sort files by modification time (most recent first)
#       rst_files <- rst_files[order(file_info$atime, decreasing = TRUE)]
#       
#       # Read the most recently modified raster
#       raster_data <- raster(rst_files[1])
#       if (!"name" %in% names(raster_data)) {
#         raster_data$name <- paste0("ID_", seq_len(nrow(raster_data)))
#         showNotification("The 'name' attribute was not found and has been created with unique IDs.")
#       }
#     }
#   } else {
#     showNotification("Please upload a ZIP file containing the raster.")
#   }
# }


### 15.  Function to update all raster list
updateRasterList <- function(raster, raster_name) {
  # Get the current raster names
  current_rst <- all_reclassified_rasters()
  
  # Add the new shapefile name to the list
  current_rst[[raster_name]]  <- raster
  
  # Update the reactive value
  all_reclassified_rasters(current_rst)
}

processRaster <- function(raster, raster_name) {
  raster_path <- raster$datapath[1]
  raster_data <- raster(raster_path)
  return(raster_data)
}
  
# Function to calculate equal intervals
calc_intervals <- function(raster_values, n_intervals) {
  breaks <- classIntervals(raster_values, n = n_intervals, style = "equal")$brks
  min_val <- round(breaks[-length(breaks)], 2)
  max_val <- round(breaks[-1], 2)
  raster_df <- data.frame(
    min_val = min_val,
    max_val = max_val,
    value = rep(1, length(breaks) - 1)
  )
  return (raster_df)
}