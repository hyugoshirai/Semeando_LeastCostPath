ui <- fluidPage(
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
  titlePanel("Análise de áreas prioritárias"),
  actionButton("help_button", "Clique aqui para ajuda", icon = icon("info-circle")),
  tabsetPanel(
    id = "dynamic_tabs",
    tabPanel(
      title = "Mapa",
      fluidRow(
        column(
          width = 4,
          div(
            class = "map-sidebar",
            fileInput("shapefile", "Carregar Shapefile (ZIP)"),
            actionButton("add_shapefile", "Adicione o Shapefile para o mapa", style = "display: none;"),
            uiOutput("sp_list"),
            selectInput(
              "basemap",
              "Escolha o Mapa Base:",
              choices = c("OpenStreetMap", "Satellite")
            ),
            tags$details(
              tags$summary(tags$b("Camadas (Clique para expandir)")),
              checkboxGroupInput(
                "overlays",
                label = NULL,
                choices = custom_control,
                selected = c("Limite Cantareira")
              )
            ),
            uiOutput("ResultRasterList"),
            # actionButton("execute_shortest_path", "Executar caminho menos custoso", style = "display: none;"),
            actionButton("execute_shortest_path", "Executar caminho menos custoso"),
          )
        ),
        column(
          width = 8,
          leafletOutput("map", height = "95vh")
        )
      )
    ),
    tabPanel("Aba para reclassificação",
             uiOutput("dropdownDefaultLayers"),
             h4("Tabela de reclassificação"),
             hidden(numericInput("defaultrst_number_of_intervals", "Número de intervalos:", value = 5, min = 2, max = 20)),
             DTOutput("Reclassify_table"),
             DTOutput("first_condition"),
             DTOutput("second_condition"),
             DTOutput("third_condition"),
             actionButton("apply_changes", "Aplicar reclassificação")
    ),
    tabPanel("Sobreposição de camadas",
             numericInput("number_of_layers", "Número de camadas:", value = 4, min = 1, max = 50),
             uiOutput("dropdown_ui"),
             actionButton("calculate_button", "Sobrepor camadas"),
             verbatimTextOutput("result_output")
    ),
    tabPanel("Download",
             downloadButton("download_selected", "Baixar camadas selecionadas"),
             uiOutput("download_dropdown_tabDownload")
    )
  )
)