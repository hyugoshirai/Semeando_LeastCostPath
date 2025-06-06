# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# Note: not necessary to load packages here: this all happens in global.R

# Increase the file upload size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

source("global.R") # Global settings, e.g. reading in of packages, data

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

# Run the application
# shinyApp(ui = ui, server = server)
runApp()