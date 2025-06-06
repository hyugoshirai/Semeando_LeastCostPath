# Function to read the Google Sheet and assign objects
AssignObjectsFromGsheet <- function(sheet_url) {
  
  # Construct the CSV URL from the sheet URL
  sheet_id <- sub(".*\\/d\\/([a-zA-Z0-9_-]+).*", "\\1", sheet_url)
  csv_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
  
  # Read the CSV file into a data frame
  df <- read.csv(csv_url)
  
  # Loop through each row of the dataframe
  for (i in 1:nrow(df)) {
    file_name <- df$Nome[i]
    file_type <- df$Tipo[i]
    file_link <- df$Link[i]
    
    if (file_type == "Raster") {
      assign(file_name, ReadGDriveRaster(file_link), envir = .GlobalEnv)
    } else if (file_type == "GeoJSON") {
      assign(file_name, ReadGDriveGeoJson(file_link), envir = .GlobalEnv)
    }
  }
}

# # # Example usage:
# # Input Google Sheet shareable URL
# AssignObjectsFromGsheet("https://docs.google.com/spreadsheets/d/1AR3T45pZ2y5CO1A2PYKXn4HxVh9xyB7CZnjnY1zy26E/edit?usp=sharing")
# 
# # Check if the objects are assigned (example)
# plot (`Imóveis`)
# plot (`Uso do solo`)