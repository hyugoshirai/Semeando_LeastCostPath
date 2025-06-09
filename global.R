# # Check if 'remotes' package is installed
# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# 
# # Function to check if a package is installed, if not, install it
# install_if_needed <- function(package, github_repo = NULL) {
#   if (!requireNamespace(package, quietly = TRUE)) {
#     if (!is.null(github_repo)) {
#       message(paste(package, "is not installed. Installing from GitHub repository:", github_repo))
#       remotes::install_github(github_repo)
#     } else {
#       message(paste(package, "is not installed. Installing from CRAN."))
#       install.packages(package)
#     }
#   } else {
#     message(paste(package, "is already installed."))
#   }
# }
# 
# # Check and install the 'icons' package from GitHub if not already installed
# install_if_needed("icons", github_repo = "mitchelloharawild/icons")
# 
# # Packages list
# packages <- c("shiny", "leaflet", "raster", "DT", "shinyWidgets", 
#               "sf", "leafem", "mapview", "gdistance", "dplyr", 
#               "shinyFiles", "zip", "leaflet.extras", 
#               "shinyjs", "classInt", "leastcostpath", "terra", "units", "tools",
#               "shinyalert", "icons", "fontawesome", "RColorBrewer")
# # library(icons)
# # download_fontawesome() #only if needed
# 
# # Instalar pacotes que não estão instalados
# new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
# if (length(new_packages)) {
#   install.packages(new_packages)
# }

# Load required packages ----
library("classInt")
library("dplyr")
library ("DT")
library("fontawesome")
library("gdistance")
library("icons")
library("leafem")
library("leaflet")
library("leaflet.extras")
library("mapview")
library("raster")
library("RColorBrewer")
library("shiny")
library("shinyFiles")
library("shinyWidgets")
library("shinyalert")
library("shinyjs")
library("sf")
library("terra")
library("tools")
library("units")
library("zip")
# terraOptions(memfrac = 0.9, progress  = 1) # Set terra options

#### Source Modules
# Define the directory containing the R script files
modules_directory <- "modules"

# List all .R files in the directory
script_files <- list.files(modules_directory, pattern = "\\.R$", full.names = TRUE)

# Loop through each R file and source it
for (script_file in script_files) {
  source(script_file)
  print (paste("Sourced", script_file))
}

### Initialize objects ----

# Create objects from sheet 
AssignObjectsFromGsheet("https://docs.google.com/spreadsheets/d/1AR3T45pZ2y5CO1A2PYKXn4HxVh9xyB7CZnjnY1zy26E/edit?usp=sharing")

# Initialize the lists to store the default objects
default_shapefiles <- list() # List for vectors
default_layers <- list() # List for rasters

# Add the objects to the reactive lists based on their type
CategorizeAndReprojectObjectsInGlobalEnv()

# Define custom control names
custom_control <- setdiff (c(names (default_layers), names (default_shapefiles)),
                           c())

### Define reactive values ----
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