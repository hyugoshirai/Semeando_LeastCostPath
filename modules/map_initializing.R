# Function to initialize the map with the land use raster image
initializeMap <- function(LandUse_rst, properties) 
{
  # aggregate the raster for faster visualization
  factor <- 5
  Uso_do_Solo_agg <- raster::aggregate(LandUse_rst, fact = factor, fun = modal)
  # PUC_agg <- raster::aggregate(PUC, fact = factor, fun = modal)
  
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", group = "Satellite") %>%
    # #==================== study_region ====================
  # # Add Area_oficina layer
  # addPolygons(data = study_region, 
  #             color = "red",    # Border color of polygons
  #             weight = 1,        # Border width of polygons
  #             opacity = 1,       # Border opacity
  #             fillOpacity = 0,
  #             group = "Area_oficina"
  #             ) |>  
  #   #==================== ProjectArea ====================
  # # Add Área do Projeto layer
  # addPolygons(data = ProjectArea, 
  #             color = "grey",    # Border color of polygons
  #             weight = 1,        # Border width of polygons
  #             opacity = 1,       # Border opacity
  #             group = "Area_do_Projeto"
  # ) |>  
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
  #   
  #   # IIC legend
  #   addLegend(
  #     position = "bottomleft",
  #     colors = IIC_colors,
  #     labels = IIC_labels,
  #     title = "IIC",
  #     group = "IIC"
  #   ) |>
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
    # properties legend
    addLegend(
      position = "bottomleft",
      colors = Property_colors,
      labels = Property_labels,
      # pal = Property_pal,
      # values = Property_labels,
      title = "Propriedades",
      group = "Imóveis"
    ) |>
    #==================== Uso_do_Solo ====================
  addRasterImage(Uso_do_Solo_agg,
                 colors = land_use_pal,
                 group = "Uso do solo",
                 project = TRUE,
                 method = 'ngb'
  ) |> 
    # Land use legend
    addLegend(
      position = "bottomleft",
      pal = land_use_pal,
      values = sort(unique(values(LandUse_rst))),
      title = "Uso do solo",
      labFormat = labelFormat(transform = function(x) landuse_df$land_use[match(x, landuse_df$raster_value)]),
      opacity = 1,
      group = "Uso do solo"  # Group the legend with the raster layer
    )%>% 
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
  # # Add a legend for the special areas
  # addLegend(
  #     pal = spa_pal,
  #     values = spa_labels,
  #     title = "Areas Especiais",
  #     position = "bottomleft",
  #     group = "Areas_Especiais"  # Group the legend with the raster layer
  #   ) |> 
  # #==================== PUC ====================
  # # Add PUC raster layer
  # addRasterImage(PUC_agg,
  #              colors = PUC_colors,
  #              group = "PUC",
  #              project = TRUE,
  #              method = 'ngb'
  #   ) |>
  # # PUC legend
  # addLegend(
  #   position = "bottomleft",
  #   pal = PUC_pal,
  #   values = PUC_values,
  #   title = "PUC",
  #   labFormat = labelFormat(transform = function(x) PUC_df$PUC[match(x, PUC_df$raster_value)]),
  #   opacity = 1,
  #   group = "PUC"  # Group the legend with the raster layer
  # ) |>
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
    hideGroup(c())
}