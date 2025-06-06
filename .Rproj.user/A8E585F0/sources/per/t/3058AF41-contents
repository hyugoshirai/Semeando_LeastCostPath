# Function to standardize and calculate with a raster based on the reference raster
# It also accumulates the result within the function itself
standardize_and_calculate <- function(raster, ref_raster, multiplier, accumulated_result) {
  print("Raster a ser somado:")
  print(raster)
  if (!is.null(raster)) {
    tryCatch({
      if (!inherits(raster, "SpatRaster")) {
        raster <- rast(raster)  # Convert RasterLayer to SpatRaster
      }
      # Resample the raster to match the reference raster's resolution and extent
      standardized_raster <- resample(raster, ref_raster, method = 'bilinear')
      
      # Replace NA values with zero in the raster
      standardized_raster <- replaceNAwithZero(standardized_raster)
      
      # Multiply by the multiplier
      calculated_raster <- standardized_raster * multiplier
      
      # Accumulate the result
      if (is.null(accumulated_result)) {
        accumulated_result <- calculated_raster
      } else {
        accumulated_result <- accumulated_result + calculated_raster
      }
      
      # Normalize the result to range from 0 to 100
      min_val <- min(values(accumulated_result), na.rm = TRUE)
      max_val <- max(values(accumulated_result), na.rm = TRUE)
      
      # Avoid division by zero in case max_val equals min_val
      if (min_val != max_val) {
        accumulated_result <- ((accumulated_result - min_val) / (max_val - min_val)) * 100
      } else {
        accumulated_result <- accumulated_result * 0 + 50  # Set to 50 if no range exists
      }
      
      return(accumulated_result)
    }, error = function(e) {
      message(paste("Error processing raster:", e))
      return(accumulated_result)  # Return the accumulated result as-is in case of error
    })
  } else {
    return(accumulated_result)
  }
}

# Creating a dummy replaceNAwithZero function for completeness (replace with actual function)
replaceNAwithZero <- function(r) {
  values(r)[is.na(values(r))] <- 0
  return(r)
}
