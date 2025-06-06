ui <- fluidPage(
  # Spinner overlay
  div(
    id = "loading-overlay",
    div(
      id = "loading",
      img(src = "loading.gif", height = 75, width = 75),
      style = "position: fixed; right: 50%; top: 50%; z-index: 3001;"
    ),
    style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; background-color: rgba(255, 255, 255, 0.5); z-index: 3000; display: none;"
  ),
  tags$script(
    'function checkifrunning() {
       var is_running = $("html").hasClass("shiny-busy");
       if (is_running){
         $("#loading-overlay").show();
       } else {
         $("#loading-overlay").hide();
       }
     }
     setInterval(checkifrunning, 100);'
  ),
  
  # Include custom CSS to define map size and button panel
  tags$head(
    tags$style(HTML("
      .leaflet-container {
        height: 100%;
        width: 100%;
      }
      #map {
        height: calc(100vh - 60px); /* Adjust based on the height needed for buttons */
        width: 100%;
      }
      .button-panel {
        margin: 10px 0;
      }
    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  titlePanel("Análise de áreas prioritárias"),
  sidebarLayout(
    sidebarPanel(
      actionButton("help_button", "Clique aqui para ajuda", icon = icon("info-circle")),
      fileInput("shapefile", "Carregar Shapefile (ZIP)"),
      actionButton("add_shapefile", "Adicione o Shapefile para o mapa", style = "display: none;"),
      # fileInput("raster", "Carregar Raster (TIF)"),
      uiOutput("ResultRasterList"), # Placeholder to render dropdown for result rasters
      actionButton("execute_shortest_path", "Executar caminho menos custoso", style = "display: none;"),
      uiOutput("sp_list"),  # Placeholder to render dropdown for shortest paths
      
      width = 4  # Make the sidebar panel thicker
    ),
    mainPanel(
      tabsetPanel(
        id = "dynamic_tabs",
        tabPanel("Mapa",
                 fluidRow(
                   column(12, leafletOutput("map", height = "85vh")),
                 )
        ),
        tabPanel("Aba para reclassificação",
                 uiOutput("dropdownDefaultLayers"),  # Placeholder to render dropdown UI components
                 h4("Tabela de reclassificação"),
                 # Hidden numeric input
                 hidden(
                   numericInput("defaultrst_number_of_intervals", "Número de intervalos:", value = 5, min = 2, max = 20)
                 ),
                 # DTOutput("landuse_table"),
                 DTOutput("Reclassify_table"),
                 DTOutput("first_condition"),
                 DTOutput("second_condition"),
                 DTOutput("third_condition"),
                 
                 actionButton("apply_changes", "Aplicar reclassificação")
        ),
        tabPanel(
          title = "Sobreposição de camadas",
          fluidRow(
            numericInput("number_of_layers", "Número de camadas:", value = 4, min = 1, max = 50),
            uiOutput("dropdown_ui"),  # Placeholder to render dropdown UI components
          ),
          actionButton("calculate_button", "Sobrepor camadas"),  # Button to perform the operation
          verbatimTextOutput("result_output")             # Display result or errors
        ),
        tabPanel(
          title = "Download",
          downloadButton("download_selected", "Baixar camadas selecionadas"),
          uiOutput("download_dropdown_tabDownload"),  # Placeholder to render dropdown download
        )
      )
    )
  )
)