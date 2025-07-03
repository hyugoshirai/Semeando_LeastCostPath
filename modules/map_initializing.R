# Function to initialize the map with the land use raster image
initializeMap <- function(LandUse_rst, properties, study_region, APP, PUC, slope, d_roads) 
{
  # aggregate the raster for faster visualization
  factor <- 5
  Uso_do_Solo_agg <- raster::aggregate(LandUse_rst, fact = factor, fun = modal)
  PUC_agg <- raster::aggregate(PUC, fact = factor, fun = modal)
  slope_agg <- raster::aggregate(slope, fact = factor, fun = modal)
  roads_agg <- raster::aggregate(d_roads, fact = factor, fun = modal)
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", group = "Satellite") %>%
    # #==================== study_region ====================
  # # Add Limite Cantareira layer
  addPolygons(data = study_region,
              color = "red",    # Border color of polygons
              weight = 1,        # Border width of polygons
              opacity = 1,       # Border opacity
              fillOpacity = 0,
              group = "Limite Cantareira"
  ) |>
    #   #==================== ProjectArea ====================
  # # Add Área do Projeto layer
  # addPolygons(data = ProjectArea, 
  #             color = "grey",    # Border color of polygons
  #             weight = 1,        # Border width of polygons
  #             opacity = 1,       # Border opacity
  #             group = "Area_do_Projeto"
  # ) |>  
  # #==================== APP ====================
  # # Add APP layer
    addRasterImage(APP,
                   colors = "blue",
                   group = "Áreas de Preservação Permanente",
                   project = TRUE,
                   method = 'ngb'
    ) |> 
    #   #==================== IIC ====================
  #   #  #Add IIC layer
  # addPolygons(data = IIC,
  #             color = ~IIC_pal(category),
  #             weight = 1,
  #             opacity = 1,
  #             fillOpacity = 0.5,
  #             group = "IIC",
  #             # SHow in the pop up: dIIC value and Area
  #             popup = ~paste("IIC: ", dIIC, "<br>", "Área: ", round(area_ha, 2), " ha")
  # ) |>
  #==================== properties ====================
  #  #Add properties layer
  addPolygons(data = properties,
              color = ~Property_pal(Tamanho),
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5,
              group = "Imóveis",
              popup = ~paste(
                "Módulos: ", round(ifelse(is.na(qtd_Mod), 0, qtd_Mod),2), "<br>", 
                "Área: ", round(area_ha, 2), " ha")
  ) |>
    #==================== Uso_do_Solo ====================
  addRasterImage(Uso_do_Solo_agg,
                 colors = land_use_pal,
                 group = "Uso do solo",
                 project = TRUE,
                 method = 'ngb'
  ) |>
    # #==================== Areas_Especiais ====================
  # #Add Areas Especiais layer
  # addPolygons(
  #   data = Areas_Especiais,
  #   color = ~spa_pal(Tipo),
  #   weight = 1,
  #   opacity = 1,
  #   fillOpacity = 0.5,
  #   group = "Areas_Especiais",
  #   popup = ~paste(NOME_UC1, COD_area_1, nm_comunid, terrai_nom, COD_area)
  # ) %>%
  # #==================== PUC ====================
  # Add PUC raster layer
  addRasterImage(PUC_agg,
                 colors = PUC_colors,
                 group = "Classes do PUC (Muito Baixo, Baixo, Médio, Alto e Muito Alto)",
                 project = TRUE,
                 method = 'ngb'
  ) |>
    # #==================== Slope ====================
  # Add slope raster layer
  addRasterImage(slope_agg,
                 colors = slope_colors,
                 group = "Declividade (porcentagem)",
                 project = TRUE,
                 method = 'ngb'
  ) |>
    # #==================== Roads ====================
  # Add roads raster layer
  addRasterImage(roads_agg,
                 colors = roads_pal,
                 group = "Distância de rodovias",
                 project = TRUE,
                 method = 'ngb'
  ) |>
    # Draw tools
    addDrawToolbar(
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      circleOptions = FALSE,
      rectangleOptions = FALSE,
      circleMarkerOptions = FALSE,
      editOptions = editToolbarOptions(remove = TRUE)
    ) %>%
    
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(imperial = FALSE, metric = TRUE)
    ) %>%
    addSearchOSM(
      options = searchOptions(autoCollapse = TRUE, minLength = 2)
    ) %>%
    addMeasure(
      position = "topright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters"
    ) %>%
    # # Layer control
    # addLayersControl(
    #   baseGroups = c("OpenStreetMap", "Satellite"),
    #   overlayGroups = custom_control,  # Ensure unique layers are listed
    #   options = layersControlOptions(collapsed = TRUE)
    # ) |>
    hideGroup(c(
      "Áreas de Preservação Permanente",
      "Imóveis",
      "Uso do solo",
      "Classes do PUC (Muito Baixo, Baixo, Médio, Alto e Muito Alto)",
      "Declividade (porcentagem)",
      "Distância de rodovias"
    ))
}