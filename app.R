# This is the main app page, which will run and read in all the other pages
# and modules, and render the dashboard

# Note: not necessary to load packages here: this all happens in global.R

# Increase the file upload size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Load global settings and packages
# source("global.R") # Global settings, e.g. reading in of packages, data

# Run the application
library (shiny)
# shinyApp(ui = ui, server = server)
runApp()
