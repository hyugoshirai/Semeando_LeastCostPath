# # Load Default Layers
# =================== LandUse
# Define the raster values directly
lu_raster_values <- c(1, 2, 4, 5, 31, 32, 33)

# Define land use labels and colors
land_use_colors <- c("#1f8d49", "#B5E61D", "#d4271e","#2532e4", "#E974ED", "#edde8e", "#7a5900")
land_use_labels <- c("Floresta", "Vegetação Herbácea e Arbustiva", "Área não Vegetada", "Corpo D'água", "Agricultura", "Pastagem", "Silvicultura" )

# Create the color factor palette
land_use_pal <- colorFactor(palette = land_use_colors, na.color = "transparent", domain = lu_raster_values)

# Create the data frame with static values
landuse_df <- data.frame(
  raster_value = lu_raster_values,
  land_use = factor(lu_raster_values, levels = lu_raster_values, labels = land_use_labels),
  # land_use = factor(land_use_labels),
  # new_value = lu_raster_values,
  new_value = c(100, 70, 1, 1, 40, 50, 50),
  stringsAsFactors = FALSE
)
# =================== Special Areas
# Define the raster values directly
spa_values <- c(1:5)

# Define land use labels and colors
spa_colors <- c("#345c32", "#BFFF00", "#9b7bb8", "#f5c840", "#9c3f28")
spa_labels <- c ("UC de Proteção Integral", "UC de Uso Sustentável","Área Prioritária", "Terra Indígena", "Área quilombola")

# Create the color factor palette
spa_pal <- colorFactor(palette = spa_colors,na.color = "transparent", domain = spa_labels)

# Create the data frame with static values
spa_df <- data.frame(
  raster_value = spa_values,
  spa = factor(spa_values, levels = spa_values, labels = spa_labels),
  new_value =  c(100, 50, 30, 100, 60),
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

Property_labels <- c("Grande", "Media", "Pequena")
# Create the color factor palette
Property_pal <- colorFactor(palette = Property_colors, na.color = "transparent", domain = Property_labels)

# Create the data frame with static values
Property_df <- data.frame(
  raster_value = Property_values,
  Property = factor(Property_values, levels = Property_values, labels = Property_labels),
  new_value = c(100, 80, 20),
  stringsAsFactors = FALSE
)