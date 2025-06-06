# Function to read a GeoJSON file from Google Drive in R

# Load necessary library
if (!require("sf")) install.packages("sf")
library(sf)

# Function to read remote GeoJSON files from Google Drive
ReadGDriveGeoJson <- function(shared_link) {
  # Extract the file ID from the shared link
  file_id <- sub(".*?/d/([^/]+).*", "\\1", shared_link)
  
  # Create the direct download URL
  download_link <- paste0("https://drive.usercontent.google.com/download?id=", file_id)
  
  # Read the GeoJSON file directly from the URL using st_read
  geojson_data <- st_read(download_link)
  
  # Print and plot to check the data
  print(geojson_data)
  return(geojson_data)
}

# # Example usage
# # Input shareable link to a GeoJson file on Google Drive
# geojson_data <- ReadGDriveGeoJson("https://drive.google.com/file/d/1CtGG6dU3OCsQxGxG27lWv88WRGW8Oy3u/view?usp=sharing")
# # Optionally plot the data
# plot(geojson_data)