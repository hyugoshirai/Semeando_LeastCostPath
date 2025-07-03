# # Load Default Layers
# =================== LandUse
# Define the raster values directly
lu_raster_values <- c(1, 2, 4, 5, 31, 32, 33)

# Define land use labels and colors
land_use_colors <- c("#1f8d49", "#B5E61D", "#d4271e","#2532e4", "#edde8e","#E974ED", "#7a5900")
land_use_labels <- c("Floresta", "Vegetação Herbácea e Arbustiva", "Área não Vegetada", "Corpo D'água", "Pastagem", "Agricultura",  "Silvicultura" )

# Create the color factor palette
land_use_pal <- colorFactor(palette = land_use_colors, na.color = "transparent", domain = lu_raster_values)

# Create the data frame with static values
landuse_df <- data.frame(
  raster_value = lu_raster_values,
  land_use = factor(lu_raster_values, levels = lu_raster_values, labels = land_use_labels),
  # land_use = factor(land_use_labels),
  # new_value = lu_raster_values,
  new_value = c(100, 70, 1, 1, 50, 40, 50),
  stringsAsFactors = FALSE
)

# =================== Special Areas
# Define the raster values directly
spa_values <- c(1:2)

# Define land use labels and colors
spa_colors <- c("#345c32", "#BFFF00")
spa_labels <- c ("PI", "US")

# Create the color factor palette
spa_pal <- colorFactor(palette = spa_colors,na.color = "transparent", domain = spa_labels)

# Create the data frame with static values
spa_df <- data.frame(
  raster_value = spa_values,
  spa = factor(spa_values, levels = spa_values, labels = spa_labels),
  new_value =  c(100, 50),
  stringsAsFactors = FALSE
)

# =================== PUC
# Define the raster values directly
PUC_values <- c(1, 2, 3, 4, 5)

# Define land use labels and colors
# 5 color scale, from green to red
PUC_colors <- c("#FE0000", "#FE9900", "#FFFF11", "#67BB41", "#3C78D8")

PUC_labels <- c("Muito Baixo", "Baixo", "Médio", "Alto",  "Muito Alto")
# Create the color factor palette
PUC_pal <- colorFactor(palette = PUC_colors, na.color = "transparent", domain = PUC_values)

# Create the data frame with static values
PUC_df <- data.frame(
  raster_value = PUC_values,
  PUC = factor(PUC_values, levels = PUC_values, labels = PUC_labels),
  new_value = c(20, 40, 60, 80, 100),
  stringsAsFactors = FALSE
)

# =================== IIC
# Define the raster values directly
IIC_values <- c(1, 2, 3, 4, 5)

# Define labels and colors
# 5 color scale, viridis
IIC_colors <- c("#440154", "#3B528B", "#21918C", "#5DC863", "#FDE725")

IIC_labels <- c("Muito Baixo", "Baixo", "Médio", "Alto",  "Muito Alto")
# Create the color factor palette
IIC_pal <- colorFactor(palette = IIC_colors, na.color = "transparent", domain = IIC_values)

# Create the data frame with static values
IIC_df <- data.frame(
  raster_value = IIC_values,
  IIC = factor(IIC_values, levels = IIC_values, labels = IIC_labels),
  new_value = c(20, 40, 60, 80, 100),
  stringsAsFactors = FALSE
)

# =================== Property
# Define the raster values directly
Property_values <- c(3, 2, 1)

# Define labels and colors
# 3 color scale, blind friendly
Property_colors <- c("#FE0000", "#FFFF11", "#3C78D8")

Property_labels <- c("Grande", "Média", "Pequena")
# Create the color factor palette
Property_pal <- colorFactor(palette = Property_colors, na.color = "transparent", domain = Property_labels)

# Create the data frame with static values
Property_df <- data.frame(
  raster_value = Property_values,
  Property = factor(Property_values, levels = Property_values, labels = Property_labels),
  new_value = c(100, 80, 20),
  stringsAsFactors = FALSE
)

# =================== slope
# Define the raster values directly
slope_values <- c(1,2,3,4)

# Define labels and colors
# 3 color scale, blind friendly
slope_colors <- c("#008B00", "#FFFF00", "#FF7F00", "#B22222")

slope_labels <- c("baixa (<3%)", "média (3-8%)", "alta (8-15%)", "muito alta (>15%)")
# Create the color factor palette
slope_pal <- colorFactor(palette = slope_colors, na.color = "transparent", domain = slope_labels)

# Create the data frame with static values
slope_df <- data.frame(
  raster_value = slope_values,
  slope = factor(slope_values, levels = slope_values, labels = slope_labels),
  new_value = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)

# =================== APP
# Define the raster values directly
APP_values <- c(1)

# Define land use labels and colors
APP_colors <- c("#1D0AC4")
APP_labels <- c ("1")

# Create the color factor palette
APP_pal <- colorFactor(palette = APP_colors,na.color = "transparent", domain = APP_labels)

# Create the data frame with static values
APP_df <- data.frame(
  raster_value = APP_values,
  APP = factor(APP_values, levels = APP_values, labels = APP_labels),
  new_value =  c(100),
  stringsAsFactors = FALSE
)
# =================== roads

# Define labels and colors

# Create the color factor palette
roads_dist_pal <- colorNumeric(palette = "viridis", domain = values(`Distância de rodovias`), na.color = "transparent")

# # Create the data frame with static values
# roads_df <- data.frame(
#   raster_value = roads_values,
#   roads = factor(roads_values, levels = roads_values, labels = roads_labels),
#   # new_value = c(10, 20, 30, 40),
#   stringsAsFactors = FALSE
# )
# 
# 
# =================== Legends
# Create a list of legends
# Calculate continuous range for roads
roads_range <- as.numeric(terra::global(`Distância de rodovias`, range, na.rm = TRUE)) # vector of min, max

legends_list <- list(
  "Unidades de Conservação" = list(
    pal = spa_pal,
    values = unique(`Unidades de Conservação`$GRUPO4),
    title = "Unidades de Conservação",
    layerId = "legend_Unidades de Conservação",
    labFormat = labelFormat()
  ),
  "Áreas Especiais" = list( #Only for reclassification
    df = spa_df
  ),
  "Classes do PUC (Muito Baixo, Baixo, Médio, Alto e Muito Alto)" = list(
    pal = PUC_pal,
    values = PUC_values,
    title = "PUC",
    layerId = "legend_Classes do PUC (Muito Baixo, Baixo, Médio, Alto e Muito Alto)",
    labFormat = labelFormat(transform = function(x) PUC_df$PUC[match(x, PUC_df$raster_value)]),
    df = PUC_df
  ),
  "Declividade (porcentagem)" = list(
    pal = slope_pal,
    values = slope_values,
    title = "Declividade (%)",
    layerId = "legend_Declividade (porcentagem)",
    labFormat = labelFormat(transform = function(x) slope_df$slope[match(x, slope_df$raster_value)]),
    df = slope_df
  ),
  "Distância de rodovias" = list(
    pal = roads_dist_pal,
    values = roads_range,
    title = "Distância de rodovias (m)",
    layerId = "legend_Distância de rodovias",
    labFormat = NULL # continuous, so default formatting
  ),
  "Uso do solo" = list(
    pal = land_use_pal,
    values = lu_raster_values,
    title = "Uso do Solo",
    layerId = "legend_Uso do Solo",
    labFormat = labelFormat(transform = function(x) landuse_df$land_use[match(x, landuse_df$raster_value)]),
    df = landuse_df
  ),
  "Imóveis" = list(
    pal = Property_pal,
    values = Imóveis$Tamanho,
    title = "Imóveis",
    layerId = "legend_Imóveis",
    labFormat = labelFormat()
  ),
  "Propriedades rurais (categorias: pequena, média e grande)" = list( # Only for reclassification
    df = Property_df
  ),
  "Índice Integral de Conectividade" = list(
    pal = IIC_pal,
    values = IIC_values,
    title = "IIC",
    layerId = "legend_IIC",
    labFormat = labelFormat(transform = function(x) IIC_df$IIC[match(x, IIC_df$raster_value)])
  ),
  "Áreas de Preservação Permanente" = list(
    pal = "#1D0AC4", 
    values = NULL, 
    title = "Áreas de Preservação Permanente",
    layerId = "legend_Áreas de Preservação Permanente",
    labFormat = NULL,
    df = APP_df
  ),
  "Limite Cantareira" = list(
    pal = "red",
    values = NULL,
    title = "Limite Cantareira",
    layerId = "legend_Limite Cantareira",
    labFormat = NULL
  ),
  "Estradas" = list(
    pal = "black",
    values = NULL,
    title = "Estradas",
    layerId = "legend_Estradas",
    labFormat = NULL
  ),
  "Limite de Municípios" = list(
    pal = "grey",
    values = NULL,
    title = "Limite de Municípios",
    layerId = "legend_Limite de Municípios",
    labFormat = NULL
  )
)
