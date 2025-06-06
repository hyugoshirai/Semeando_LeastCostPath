# Help Modal
# Function to create the modal dialog
createHelpModal <- function() {
  modalDialog(
    title = "Menu de Ajuda",
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l",
    tags$div(
      # Tabset Panel with IDs for each tabPanel
      tabsetPanel(
        id = "help_tabs",
        tabPanel(
          "Navegação",
          tagList(
            h4("Instruções de Navegação no mapa"),  
            h6(tags$i("(Clique nas figuras para aumentar o tamanho. Clique novamente para retornar ao tamanho normal.)")),
            tags$ul(
              tags$li(
                strong("Desenhar Pontos no Mapa:"),
                " Use o ", tags$img(src = "location-dot-solid.svg", style = "height: 1em; vertical-align: middle;"), " ícone para desenhar seus pontos no mapa."
                )
              ),
            tags$img(src = "draw.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Edit Points:"),
                " Se precisar ajustar um ponto, clique no ícone ", tags$img(src = "google_edit_square.png", style = "height: 1em; vertical-align: middle;"), ", selecione o ponto que deseja editar e salve a edição."
              )
            ),
            tags$img(src = "edit.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Deletar Pontos:"),
                       " Para remover um ponto, clique no ", tags$img(src = "trash-can-solid.svg", style = "height: 1em; vertical-align: middle;"), " ícone e clique no ponto que deseja deletar."
              )
            ),
            tags$img(src = "delete.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong("Pontos Mínimos Necessários:"),
                       " Por favor, note que você deve desenhar pelo menos ", strong("dois pontos"), " para executar a análise do caminho mais curto."
              ),
              tags$li(
                strong("Alterar Basemap:"),
                " Você pode alterar o basemap nas opções disponíveis no mapa."
              )
            ),
            tags$img(src = "basemap.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            tags$ul(
              tags$li(
                strong(actionLink("reclassify_table", "Aba de reclassificação:")),
                " Aplique a reclassificação para as camadas disponíveis com base no critério de priorização."
              ),
              tags$li(
                strong(actionLink("overlay_layers", "Sobrepor camadas:")),  
                " Sobreponha as camadas desejadas para a análise conjunta dos diferentes critérios. Você também pode definir diferentes pesos para cada camada."
              ),
              tags$li(
                strong("Executar Cálculo do Caminho Mais Curto:"),
                " Uma vez que você tenha selecionado seus pontos e feito os ajustes necessários, clique no botão ",
                strong("\"Executar caminho menos custoso\""),
                " para realizar o cálculo."
              ),
              tags$img(src = "shortestPath.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
              tags$li(
                strong("Visualizar Resultados:"),
                " Após o cálculo ser concluído, você pode visualizar os resultados no mapa."
              ),
              tags$li(
                strong(actionLink("download_layers", "Baixar Camadas:")),
                " Vá para a aba ", strong("Download"), " para selecionar e baixar as camadas que você precisa."
              )
            ),
            p("Para navegar no mapa de forma mais eficaz, use os seguintes ícones:"),
            tags$ul(
              tags$li("Zoom In: ", icons::fontawesome$regular$`plus-square`, " para dar zoom no mapa."),
              tags$li("Zoom Out: ", icons::fontawesome$regular$`minus-square`, " para dar zoom out no mapa."),
              tags$li("Alternar Camadas: ", icon_style(fontawesome("layer-group", style = "solid"), fill = "grey"), " para alternar camadas."),
              tags$li("Medir Distâncias: ", icons::fontawesome$solid$`ruler-combined`, " para medir distâncias no mapa.")
            )
          )
        ),
        tabPanel(
          "Aba de reclassificação",
          tagList(
            h4("Reclassificação"),
            p("Na aba de reclassificação, você pode ajustar o raster de uso do solo com base em nossos critérios. Isso permite que o algoritmo priorize áreas com valores mais altos."),
            tags$img(src = "reclassif.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            actionButton("main_page", "Retornar à página principal do menu de ajuda", icon = icon("home"))
          )
        ),
        tabPanel(
          "Sobrepor camadas",
          tagList(
            h4("Sobrepor camadas"),  
            p("Aqui você pode sobrepor várias camadas para construir mapas compostos para análise e visualização."),
            tags$img(src = "overlay.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            actionButton("main_page", "Retornar à página principal do menu de ajuda", icon = icon("home"))
          )
        ),
        tabPanel(
          "Download",
          tagList(
            h4("Baixar Camadas"),
            p("Vá para a aba de Download para selecionar e baixar as camadas que você precisa."),
            tags$img(src = "download.gif", style = "width: 100%;", onclick = "this.style.transform='scale(2)';", ondblclick = "this.style.transform='scale(1)';"),
            actionButton("main_page", "Retornar à página principal do menu de ajuda", icon = icon("home"))
          )
        )
      )
    )
  )
}