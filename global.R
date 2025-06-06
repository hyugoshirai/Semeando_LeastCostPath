# Check if 'remotes' package is installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Function to check if a package is installed, if not, install it
install_if_needed <- function(package, github_repo = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    if (!is.null(github_repo)) {
      message(paste(package, "is not installed. Installing from GitHub repository:", github_repo))
      remotes::install_github(github_repo)
    } else {
      message(paste(package, "is not installed. Installing from CRAN."))
      install.packages(package)
    }
  } else {
    message(paste(package, "is already installed."))
  }
}

# Check and install the 'icons' package from GitHub if not already installed
install_if_needed("icons", github_repo = "mitchelloharawild/icons")

# Lista de pacotes
packages <- c("shiny", "leaflet", "raster", "DT", "shinyWidgets", 
              "sf", "leafem", "mapview", "gdistance", "dplyr", 
              "shinyFiles", "zip", "leaflet.extras", 
              "shinyjs", "classInt", "leastcostpath", "terra", "units", "tools",
              "shinyalert", "icons", "fontawesome", "RColorBrewer")
# library(icons)
# download_fontawesome() #only if needed

# Instalar pacotes que não estão instalados
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Carregar pacotes
invisible(lapply(packages, function(pkg) {
  suppressWarnings(library(pkg, character.only = TRUE))
}))

terraOptions(memfrac = 0.9, progress  = 1) # Set terra options

### Load shp in folder using a for loop
# Directory containing the shapefiles
shapefile_directory <- "../_4_Dados/Shapefiles"

# List all .shp files in the directory
shapefile_files <- list.files(shapefile_directory, pattern = "\\.shp$", full.names = TRUE)

# Initialize the list to store the default shapefile objects
default_shapefiles <- list()

# Manually specify the names you want to assign to each shapefile
custom_names <- c("Area_do_Projeto", "AAVC","Area_oficina", "Areas_Especiais", 
                  "IIC", "Propriedades", "RPPN")  # Update this with appropriate names

# Check if the number of custom names matches the number of shapefile files
if (length(custom_names) == length(shapefile_files)) {
  # Loop through each shapefile file and create a shapefile object with custom names
  for (i in seq_along(shapefile_files)) {
    shapefile_file <- shapefile_files[i]
    custom_name <- custom_names[i]
    
    # Read the shapefile
    shapefile_object <- st_read(shapefile_file)
    
    # Transform the shapefile object to the desired CRS for plotting on map (e.g., WGS84, EPSG:4326)
    shapefile_object <- st_transform(shapefile_object, crs = 4326)
    
    # Assign the shapefile object to a variable named after the custom name
    assign(custom_name, shapefile_object, envir = .GlobalEnv)
    
    # Assign the shapefile object to a list element with the custom name
    default_shapefiles[[custom_name]] <- shapefile_object
  }
  
} else {
  stop("The number of custom names does not match the number of shapefile files.")
}

### Load raster in folder using a for loop
# Directory containing the raster files
raster_directory <- "../_4_Dados/Raster"

# List all .tif files in the directory
raster_files <- list.files(raster_directory, pattern = "\\.tif$", full.names = TRUE)

# Initialize the list to store the default raster objects
default_layers <- list()

# Manually specify the names you want to assign to each raster
custom_names <- c("Areas_Especiais_rst",
                  "Uso_do_Solo", "propriedades_rst", "PUC")

# Check if the number of custom names matches the number of raster files
if (length(custom_names) == length(raster_files)) {
  # Loop through each raster file and create a raster object with custom names
  for (i in seq_along(raster_files)) {
    raster_file <- raster_files[i]
    custom_name <- custom_names[i]
    
    # Assign the raster object to a variable named after the custom name
    assign(custom_name, rast(raster_file))
    
    # Assign the raster object to a list element with the custom name
    default_layers[[custom_name]] <- rast(raster_file)
  }
} else {
  stop("The number of custom names does not match the number of raster files.")
}

# Define custom control names
custom_control <- setdiff (c(names (default_layers), names (default_shapefiles), "CMPC"),
                           c("Areas_Especiais_rst", "AAVC", "RPPN", "SEUC", "areas_umidas_FZB", 
                             "AZE", "BAZE","IBA", "RBMA", "propriedades_rst"))

### 1. Define reactive values 
all_points <- reactiveVal(list()) # For storing all points
all_reclassified_rasters <- reactiveVal(list()) # Create a reactive list to hold all reclassified rasters
all_reclassified_rasters_aggregated <- reactiveVal(list()) # Create a reactive list to hold all aggregated reclassified rasters 
all_shapefiles <- reactiveVal(list()) # For storing the shapefiles
checkbox_layers <- reactiveVal(c("Original Raster")) # Initialize a reactive value to store the checkbox layers
# default_layers <- reactiveVal() # Create a reactive value to store the default layers
current_layers <- reactiveVal(character(0)) # Create a reactive list to hold current layers name
default_layers_name <- reactiveVal() # Create a reactive value to store the default layers name
default_layers_reclass_df <- reactiveVal() # Create a reactive value to store the default layers reclassification dataframe
reclassification_dfs <- reactiveVal(default_layers)
downldropdown <- reactiveValues(dropdowns = "layers_dropdown") # Reactive value to store the dropdown identifier
euclidean_dist_rasters <- reactiveValues() # Create a reactive value to store the euclidean distance rasters
euclidean_reclass_df <- reactiveVal() # Reactive value to store the reclassification dataframe for euclidean distance
feature_values_data <- reactiveVal(NULL) # For storing feature values data
layer_colors <- reactiveValues(list = list()) # Redirect layer colors to observe later listItem
rasterized_layer <- reactiveVal(NULL) # For storing rasterized shapefile
reactive_tabs <- reactiveVal(list()) # For storing tab names
raster_data_processed <- reactiveVal(NULL) # For storing the processed raster data
# Rec_LandUse <- reactiveVal(LandUse_rst) # For storing reclassified LandUse data
raster_df <- reactiveVal() # For storing raster data frames
raster_df_list <- reactiveVal(list()) # For storing raster data frames list
result_raster <- reactiveVal(NULL) # For storing result raster
result_raster_counter <- reactiveVal(0) # Counter for result raster
shapefile_data <- reactiveValues(shp = NULL, colors = list()) # For storing the uploaded shapefile data
shapefile_data_processed <- reactiveVal(NULL) # For storing the processed shapefile data
shapefile_names_list <- reactiveVal(character(0)) # For storing the names of uploaded shapefiles
shortest_path_counter <- reactiveVal(0) # Counter for shortest path
shortest_path_layers <- reactiveValues(list = list()) # Create a reactive value to store the shortest path layers
shortest_path_result <- reactiveVal(NULL) # For storing shortest path result
selected_layer <- reactiveVal(NULL) # For storing the selected layer in the dropdown list