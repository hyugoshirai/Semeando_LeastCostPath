# Function to initialize the map with the land use raster image
initializeMap <- function(LandUse_rst, RPPN, ProjectArea, AAVC, Areas_Especiais, PUC, 
                          study_region, properties, IIC) 
  {
  # aggregate the raster for faster visualization
  start_time <- Sys.time()
  cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  factor <- 5
  Uso_do_Solo_agg <- raster::aggregate(LandUse_rst, fact = factor, fun = modal)
  PUC_agg <- raster::aggregate(PUC, fact = factor, fun = modal)
  end_time <- Sys.time()
  cat("Processing finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total aggregatetime: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
  
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", group = "Satellite") %>%
    #==================== study_region ====================
    # Add Area_oficina layer
    addPolygons(data = study_region, 
                color = "red",    # Border color of polygons
                weight = 1,        # Border width of polygons
                opacity = 1,       # Border opacity
                fillOpacity = 0,
                group = "Area_oficina"
                ) |>  
    #==================== ProjectArea ====================
  # Add Área do Projeto layer
  addPolygons(data = ProjectArea, 
              color = "grey",    # Border color of polygons
              weight = 1,        # Border width of polygons
              opacity = 1,       # Border opacity
              group = "Area_do_Projeto"
  ) |>  
    #==================== IIC ====================
    #  #Add IIC layer
  addPolygons(data = IIC,
              color = ~IIC_pal(category),
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5,
              group = "IIC",
              # SHow in the pop up: dIIC value and Area
              popup = ~paste("IIC: ", dIIC, "<br>", "Área: ", round(area_ha, 2), " ha")
  ) |>
    
    # IIC legend
    addLegend(
      position = "bottomleft",
      colors = IIC_colors,
      labels = IIC_labels,
      title = "IIC",
      group = "IIC"
    ) |>
    #==================== properties ====================
  #  #Add properties layer
  addPolygons(data = properties,
              color = ~Property_pal(Tamanho),
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5,
              group = "Propriedades",
              popup = ~paste(
                "Módulos: ", round(ifelse(is.na(mod_fiscal), 0, mod_fiscal),2), "<br>", 
                "Área: ", round(area_ha, 2), " ha")
              ) |>
    # properties legend
    addLegend(
      position = "bottomleft",
      colors = Property_colors,
      labels = Property_labels,
      # pal = Property_pal,
      # values = Property_labels,
      title = "Propriedades",
      group = "Propriedades"
    ) |>
    #==================== Uso_do_Solo ====================
    addRasterImage(Uso_do_Solo_agg,
                   colors = land_use_pal,
                   group = "Uso_do_Solo",
                   project = TRUE,
                   method = 'ngb'
    ) |> 
    # Land use legend
    addLegend(
      position = "bottomleft",
      pal = land_use_pal,
      values = sort(unique(values(LandUse_rst))),
      title = "Uso do Solo",
      labFormat = labelFormat(transform = function(x) landuse_df$land_use[match(x, landuse_df$raster_value)]),
      opacity = 1,
      group = "Uso_do_Solo"  # Group the legend with the raster layer
    )%>% 
    #==================== CMPC ====================
    # Add AAVC points layer
    addCircleMarkers(
      data = AAVC,
      lng = st_coordinates(AAVC)[,1],
      lat = st_coordinates(AAVC)[,2],
      group = "CMPC",
      color = "red",
      radius = 5,  # Customize the radius of the circles
      popup = ~AAVC) |>
    
    #Add RPPN points layer
    addCircleMarkers(
      data = RPPN,
      lng = st_coordinates(RPPN)[,1],
      lat = st_coordinates(RPPN)[,2],
      group = "CMPC",
      color = "blue",
      radius = 5,  # Customize the radius of the circles
      popup = ~Nome) |>
    
    # RPPN and AAVC legend
    addLegend(
      position = "bottomleft",
      colors = c("red", "blue"),
      labels = c("AAVC", "RPPN"),
      title = "CMPC",
      group = "CMPC"
      ) |> 
    #==================== Areas_Especiais ====================
    #Add Areas Especiais layer
    addPolygons(
      data = Areas_Especiais,
      color = ~spa_pal(Tipo),
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5,
      group = "Areas_Especiais",
      popup = ~paste(NOME_UC1, COD_area_1, nm_comunid, terrai_nom, COD_area)
    ) %>%
    # Add a legend for the special areas
    addLegend(
        pal = spa_pal,
        values = spa_labels,
        title = "Areas Especiais",
        position = "bottomleft",
        group = "Areas_Especiais"  # Group the legend with the raster layer
      ) |> 
    #==================== PUC ====================
    # Add PUC raster layer
    addRasterImage(PUC_agg,
                 colors = PUC_colors,
                 group = "PUC",
                 project = TRUE,
                 method = 'ngb'
      ) |>
    # PUC legend
    addLegend(
      position = "bottomleft",
      pal = PUC_pal,
      values = PUC_values,
      title = "PUC",
      labFormat = labelFormat(transform = function(x) PUC_df$PUC[match(x, PUC_df$raster_value)]),
      opacity = 1,
      group = "PUC"  # Group the legend with the raster layer
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
    # Layer control
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = custom_control,  # Ensure unique layers are listed
      options = layersControlOptions(collapsed = TRUE)
    ) |> 
  hideGroup("CMPC") |> 
  hideGroup("Areas_Especiais") |> 
  hideGroup("PUC") |>
  hideGroup("Habitats_criticos") |> 
  hideGroup("Propriedades") |> 
  hideGroup("IIC")
}