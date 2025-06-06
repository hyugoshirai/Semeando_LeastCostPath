# Function to read a raster file from Google Drive using terra package in R

# Load necessary library
if (!require("terra")) install.packages("terra")
library(terra)

# Function to read remote raster (TIFF) files from Google Drive
ReadGDriveRaster <- function(shared_link) {
  # Extract the file ID from the shared link
  file_id <- sub(".*?/d/([^/]+).*", "\\1", shared_link)
  
  # Create the direct download URL
  download_link <- paste0("https://drive.usercontent.google.com/download?id=", file_id)
  
  # Prepend with /vsicurl/ for GDAL's usage in terra
  myurl <- paste0("/vsicurl/", download_link)
  
  # Read the raster data directly from the URL using terra
  raster <- rast(myurl)
  
  # Check and print raster information
  print(raster)

  return(raster)
}

# # Example usage:
# # Input shareable link to a TIFF file on Google Drive
# d_rodovias <- ReadGDriveRaster ("https://drive.google.com/file/d/1-4VYjJC5Rak2NYUmy4zecfpniHbDa3sW/view?usp=drive_link")
# # Optionally plot the raster
# plot(d_rodovias)