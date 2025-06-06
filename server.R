# Server Function ----------------------------------------------------------
server <- function(input, output, session) {
  
  ### 1. Render map
  output$map <- renderLeaflet({
    initializeMap(`Uso do solo`, `Imóveis`)
  })
  
  ### 2. Update the basemap when selection changes
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$basemap]], group = "OpenStreetMap", layerId = "basemap")
  })
  
  ### 3. Handle landuse reclassification and map update
  # Output for the dropdown UI
  output$dropdownDefaultLayers <- renderUI({
    current_layers <- names(default_layers)
    selectInput("dropdownDefaultLayers", label = "Selecione uma camada:", choices = c("Selecione uma camada" = "", current_layers))
  })
  
  DefaultRasterReclassification(id= "Reclassify_table", input, output, session, landuse_df, input$dropdownDefaultLayers)
  
  ### 4. Handle map drawing events
  DrawPoints(input, output, session, all_points)
  
  
  ### 5. Function to process the uploaded shapefile
  shapefile_data_processed <- reactiveVal(NULL)
  observeEvent(input$shapefile, {
    req(input$shapefile)
    shapefile_data_processed(processShapefile(input$shapefile, shapefile_data))
    
    
    if (is.na(st_crs(shapefile_data_processed()))) {
      showNotification("O shapefile não tem um sistema de coordenadas. O processo foi interrompido.", type = "error")
      return(NULL)
    } else {
      if (!is.null(input$shapefile)) {
        shinyjs::show("add_shapefile") # Show the add_shapefile button
      }
      showModal(modalDialog(
        title = "Insira o nome do shapefile",
        textInput("shapefile_name", "Nome do shapefile"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("new_shp_tab", "OK")
        )
      )
      )
    }
  })
  
  # ### 7. Shapefile tab
  observeEvent(input$new_shp_tab, {
    req(input$shapefile_name)
    shapefile_data_processed <- shapefile_data_processed()
    shapefile_name <- input$shapefile_name
    removeModal()
    
    # Add the new tab to the reactive value
    new_tabs <- reactive_tabs()
    new_tabs[[shapefile_name]] <- shapefile_name
    reactive_tabs(new_tabs)
    
    # Append the new tab to the UI
    appendTab(inputId = "dynamic_tabs",
              tabPanel(shapefile_name,  # Use shapefile_name as tab title
                       h3(paste("Shapefile carregado:", shapefile_name)),
                       DTOutput(paste0("shapefile_table_", shapefile_name)),  # Updated to match output ID
                       actionButton(paste0("apply_", shapefile_name), "Aplicar reclassificação"),
                       actionButton(paste0("euclidean_dist", shapefile_name), "Executar distancia euclidiana", style = "display: none;"),
                       # Hidden numeric input
                       hidden(
                         numericInput("number_of_intervals", "Número de intervalos:", value = 5, min = 1, max = 20)
                       ),
                       DTOutput(paste0("euclidean_dist_table_", shapefile_name)),  # Updated to match output ID
                       actionButton("reclass_euclid_dist", "Reclassificar distancia euclidiana", style = "display: none;")
              )
    )
    
    # Create a reactive value to store the shapefile data
    unique_names <- unique(shapefile_data_processed$name)
    # Create a data frame with 'name' and 'value' columns
    shapefile_df <- reactiveVal(data.frame(
      name = unique_names,              # Column for unique names
      value = rep(1, length(unique_names))   # Initialize 'value' column with 1s
    ))
    
    # Render the data table with editable cells
    output[[paste0("shapefile_table_", shapefile_name)]] <- renderDT({
      datatable(
        shapefile_df(),  # Use the reactive value
        editable = list(target = "cell", disable = list(columns = 0)),  # Disable editing for the 'name' column
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)  # Adjust options as needed
      )
    })
    
    # Observe cell edits
    observeEvent(input[[paste0("shapefile_table_", shapefile_name, "_cell_edit")]], {
      new_data <- input[[paste0("shapefile_table_", shapefile_name, "_cell_edit")]]
      row <- new_data$row
      value <- as.numeric(new_data$value)
      
      # Ensure the value is not less than 2
      if (value < 2) {
        showNotification("O valor deve ser maior ou igual a 2.", type = "warning")
        value <- 2
      }
      
      # Update the reactive value with the new data
      df <- shapefile_df()
      df[row, "value"] <- value
      shapefile_df(df)
    })
    
    # Observe the apply button click
    observeEvent(input[[paste0("apply_", shapefile_name)]], {
      # Get the updated data from the reactive value
      df <- shapefile_df()
      print(df)
      
      # Add a new column to the shapefile with the corresponding 'value' from the data table
      shapefile_data_processed$value <- sapply(shapefile_data_processed$name, function(feature_name) {
        match_index <- match(feature_name, df$name)
        if (!is.na(match_index)) {
          return(df$value[match_index])
        } else {
          return(NA)  # Or some default value if not found
        }
      })
      
      # Perform rasterization based on the 'values' column
      rasterized_shp <- rasterizeShapefile(shapefile_data_processed, Uso_do_Solo)
      
      # Add the rasterized shapefile to the list
      updateList (rasterized_shp, shapefile_name, all_reclassified_rasters)
      
      # Add rasterized layer to the map or further process it
      # addRasterToMap(rasterized_shp, shapefile_name)
      
      showNotification(paste("Shapefile", shapefile_name, "foi rasterizado baseado na coluna 'value'"), type = "message")
      shinyjs::show(paste0("euclidean_dist", shapefile_name)) # Show the reclassify Euclidean distance button
    })
    
    # Observe the Euclidean distance button click
    observeEvent(input[[paste0("euclidean_dist", shapefile_name)]], {
      shinyjs::show("number_of_intervals")  # Show the numeric input
      
      # Get the list of all reclassified rasters
      current_rasters <- all_reclassified_rasters()
      
      start_time <- Sys.time()
      cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      # Convert the terra raster to a raster object
      raster_layer <- raster(current_rasters[[shapefile_name]])
      # Record the end time
      end_time <- Sys.time()
      cat("Converting finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Total aggregate processing time: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
      
      # Calculate Euclidean distance
      euclidean_dist_rasters[[shapefile_name]] <- euclideanDistance(raster_layer)
      
      # Calculate the intervals for the raster data
      raster_values <- raster::values(euclidean_dist_rasters[[shapefile_name]])  # Get the raster values
      
      observeEvent (input$number_of_intervals, {
        # Calculate the equal intervals for the raster data
        breaks <- classIntervals(raster_values, n = input$number_of_intervals, style = "equal")$brks
        # Create intervals based on breaks
        min_val <- round(breaks[-length(breaks)], 2)
        max_val <- round(breaks[-1], 2)
        # Create a data frame with 'interval' and 'value' columns
        euclidean_reclass_df(data.frame(
          min_val = min_val,
          max_val = max_val,
          value = rep(1, length(breaks) - 1)  # Initialize 'value' column
        ))
        
        # Render the data table with editable cells
        output[[paste0("euclidean_dist_table_", shapefile_name)]] <- renderDT({
          datatable(
            euclidean_reclass_df(),  # Use the reactive value
            editable = list(target = "cell", disable = list(columns = 0)),  # Disable editing for the 'interval' column
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE)  # Adjust options as needed
          )
        })
      })
      shinyjs::show("reclass_euclid_dist") # Show the reclassify Euclidean distance button
      
      # Observe cell edits and update the data frame
      observeEvent(input[[paste0("euclidean_dist_table_", shapefile_name, "_cell_edit")]], {
        info <- input[[paste0("euclidean_dist_table_", shapefile_name, "_cell_edit")]]
        df <- euclidean_reclass_df()
        # Extract the edited cell info
        i <- info$row
        j <- info$col
        new_value <- as.numeric(info$value)
        
        # Update the data frame
        df[i, j + 1] <- new_value
        
        if (info$col == 1) {
          if (df[info$row, "min_val"] >= df[info$row, "max_val"]) {
            showNotification("O valor mínimo deve ser menor que o valor máximo.", type = "warning")
            df[info$row, info$col + 1] <- df[info$row + 1, "min_val"]
          }
          else
          {
            df[info$row + 1, "min_val"] <- as.numeric(info$value)  # Update the next row's min_val
          }
        }
        
        # Update the reactive data frame
        euclidean_reclass_df(df)
        print (euclidean_reclass_df())
      })
      
      # Observe the reclassify button click for Euclidean distance
      observeEvent(input$reclass_euclid_dist, {
        req(euclidean_reclass_df())  # Ensure the data frame is available
        
        # Extract the reclassification data frame
        reclass_df <- euclidean_reclass_df()
        
        # Ensure that the user has filled in all reclassification values
        if (any(is.na(reclass_df$value))) {
          showNotification("Por favor, preencha todos os valores de reclassificação antes de prosseguir.", type = "error")
          return()
        }
        
        # Perform reclassification
        euclidean_raster <- euclidean_dist_rasters[[shapefile_name]]  # Get the current Euclidean raster
        reclassified_raster <- reclassify_raster(euclidean_raster, reclass_df)
        
        # update the reclassified raster list with the new raster
        updateList (reclassified_raster, paste("Reclassified euclidean distance for", shapefile_name), all_reclassified_rasters)
        
        # Add the resulting raster to the map
        addRasterToMap(reclassified_raster, paste("Reclassified euclidean distance for", shapefile_name))
      })
    })
  })
  
  ### 6. Function to add the shapefile to the map
  observeEvent(input$add_shapefile, {
    req(input$shapefile_name)
    shapefile_name <- input$shapefile_name
    new_layer_name <- paste("Shapefile for", shapefile_name)
    shapefile_data_processed <- shapefile_data_processed()
    
    # Add the uploaded shapefile to all shapefiles list
    updateList (shapefile_data_processed, new_layer_name, all_shapefiles)
    
    addShapefileToMap(shapefile_data_processed(), new_layer_name)
  })
  
  ### 8. Execute shortest path
  # Observer to monitor result_raster and all_points and show or hide the button
  observe({
    # Ensure both result_raster and all_points return valid values
    if (# !is.null(result_raster()) && is(result_raster(), "RasterLayer") ||
      input$calculate_button > 0 &&
      !is.null(all_points()) && length(all_points()) > 1) {
      shinyjs::show("execute_shortest_path") # Show the execute_shortest_path button
    } else {
      shinyjs::hide("execute_shortest_path") # Hide the execute_shortest_path button
    }
  })
  observeEvent(input$execute_shortest_path, {
    points_list <- all_points()  # Get the list of points
    points_combined_sf <- do.call(rbind, points_list)
    st_crs(points_combined_sf) <- 4326
    
    if (nrow(points_combined_sf) < 2) {
      showNotification("Erro: Desenhe pelo menos dois pontos no mapa", type = "error")
      return()
    }
    
    new_layer_name <- paste0("corridor_", shortest_path_counter() + 1)
    new_color <- generateColor()
    
    # Use the result_raster selected from the dropdown
     
    # Fetch the selected raster name
    selected_raster <- input$result_dropdown

    # Use the selected raster key to get the corresponding value from your data
    all_reclassified_rasters <- all_reclassified_rasters()
    # result_raster_selected <- all_reclassified_rasters [selected_raster]
    result_raster_selected <- all_reclassified_rasters()[[selected_raster]]
    
    # sp_result <- executeShortestPath(result_raster(), points_combined_sf, session)
    sp_result <- executeShortestPath(result_raster_selected, points_combined_sf, session)
    
    # Store the new shortest path result into reactive values (to color different)
    shortest_path_layers$list[[new_layer_name]] <- sp_result
    shortest_path_counter(shortest_path_counter() + 1)
    layer_colors$list[[new_layer_name]] <- new_color
    
    # Add the shortest path to all shapefiles list
    updateList (sp_result, new_layer_name, all_shapefiles)
    
    # Show the buffer related inputs
    # shinyjs::toggle(id = "buffer_div")
    shinyjs::show(id = "buffer_value")
    shinyjs::show(id = "buffer_button")
    
    
    addShortestPathToMap(sp_result, "map", new_layer_name, new_color)
  })
  
  ### 9. Calculate tab
  # Reactive values
  rv <- reactiveValues(dropdowns = NULL, multipliers = NULL)
  
  # Observe the numeric input for the number of layers
  observeEvent(input$number_of_layers, {
    n <- input$number_of_layers
    rv$dropdowns <- paste("raster_dropdown_", 1:n, sep = "")
    rv$multipliers <- paste("multiplier_", 1:n, sep = "")
    
    output$dropdown_ui <- renderUI({
      dropdowns_ui <- lapply(1:n, function(i) {
        createRasterUI(i)
      })
      do.call(tagList, dropdowns_ui)
    })
  })
  
  # Initialize the UI with the default value of numeric input
  observe({
    updateNumericInput(session, "number_of_layers", value = input$number_of_layers)
  })
  
  # Function to create the UI for raster dropdowns and multipliers
  createRasterUI <- function(index, populated = FALSE) {
    # Get the current layer names
    current_layers <- names(all_reclassified_rasters())
    
    # Create a fluidRow with two columns
    fluidRow(
      column(6,
             # Dropdown for raster selection
             selectInput(rv$dropdowns[[index]],
                         label = paste("Selecione um raster", index, ":"),
                         choices = c("Selecione um raster" = "", current_layers)
             )
      ),
      column(6,
             # Numeric input for multipliers
             numericInput(rv$multipliers[[index]],
                          label = paste("Multiplicador", index, ":"),
                          value = 1
             )
      )
    )
  }
  
  # Observe the calculate button and perform raster calculations
  observeEvent(input$calculate_button, {
    # Check if any raster is selected
    if (all(sapply(rv$dropdowns, function(id) input[[id]]) == "")) {
      showNotification("Selecione pelo menos um raster para realizar a sobreposição.", type = "error")
      return()
    }
    
    # Get the list of all reclassified rasters
    current_rasters <- all_reclassified_rasters()
    
    # Get selected rasters and multipliers from inputs
    selected_rasters <- sapply(rv$dropdowns, function(id) input[[id]])
    multipliers <- sapply(rv$multipliers, function(id) input[[id]])
    
    # Filter out empty selections and ensure multipliers are numeric
    filtered_rasters <- selected_rasters[selected_rasters != ""]
    multipliers <- as.numeric(multipliers[selected_rasters != ""])
    
    if (length(filtered_rasters) == 0) {
      showNotification("Nenhum raster selecionado.", type = "error")
      return()
    }
    
    # Load rasters from the current_rasters list based on selections
    rasters <- lapply(filtered_rasters, function(name) {
      if (name != "" && name %in% names(current_rasters)) {
        current_rasters[[name]]
      } else {
        NULL
      }
    })
    
    # Validate rasters have been retrieved
    if (all(sapply(rasters, is.null))) {
      showNotification("Nenhum raster válido encontrado na seleção.", type = "error")
      return()
    }
    
    # Initialize accumulated result
    accumulated_result <- NULL
    
    for (i in seq_along(rasters)) {
      if (!is.null(rasters[[i]])) {
        # Standardize, calculate, and accumulate in one step
        accumulated_result <- standardize_and_calculate(rasters[[i]], ref_raster = Uso_do_Solo, multipliers[i], accumulated_result)
      }
    }
    # Check if the accumulated result is valid
    if (is.null(accumulated_result)) {
     showNotification("Erro: Nenhum resultado acumulado válido encontrado.", type = "error")
     return()
    }
    
    # Update the reactive value
    new_resultRst_name <- paste0("Result raster", result_raster_counter() + 1)
    
    # Store the new result raster into reactive values
    result_raster(accumulated_result)
    
    # Increment the result raster counter
    result_raster_counter(result_raster_counter() + 1)
    
    # Add the result raster to all_reclassified_rasters list
    updateList (accumulated_result, new_resultRst_name, all_reclassified_rasters)
    
    # Add the resulting raster to the map
    addRasterToMap(accumulated_result, new_resultRst_name)
  })
  
  
  observeEvent(input$calculate_button, {
    # Render the result raster list dropdown
    output$ResultRasterList <- renderUI({
      req(all_reclassified_rasters()) # Ensure all_reclassified_rasters is available
      # Get the names of all reclassified rasters
      all_reclassified_rasters <- all_reclassified_rasters()
      raster_names <- names(all_reclassified_rasters)
      
      result_items <- raster_names[startsWith(raster_names, "Result")]
      
      if (length(result_items) > 0) {
        selectInput("result_dropdown", "Selectione o raster de permeabilidade", choices = result_items)
      }
    })
  })
  
  # ### 10. Download selected layers
  # Output for the dropdown UI
  output$download_dropdown_tabDownload <- renderUI({
    current_layers <- c(getCurrentLayerNames(),names(all_reclassified_rasters()))
    createDownloadDropdown(id = "download_dropdown_tabDownload", multiple = TRUE, populated = FALSE, current_layers = current_layers)
  })
  
  # Download handler for the download button - Selected Layers
  output$download_selected <- downloadHandler(
    filename = function() {
      paste("selected_layers_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      selected_layers <- input$download_dropdown_tabDownload # Replace with actual input ID
      
      # Check if any layers were selected
      if (is.null(selected_layers) || length(selected_layers) == 0) {
        showNotification("Nenhuma camada selecionada para download.", type = "error")
        return(NULL)
      }
      
      # Temporary directory to hold files to be zipped
      temp_dir <- tempdir()
      file_paths <- c()
      
      for (layer in selected_layers) {
        if (layer == "Pontos a conectar") {
          all_points_sf_list <- all_points() # Proper extraction
          
          # Combine the sf objects
          combined_sf <- combine_sf_objects(all_points_sf_list)
          
          print("combined_sf")
          print(combined_sf)
          
          file_path <- file.path(temp_dir, layer)
          st_write(combined_sf, file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
          file_paths <- c(file_paths, file_path)          
        }
        # Fetch the file from all_reclassified_rasters or all_shapefiles
        if (layer %in% names(all_reclassified_rasters())) {
          layer_file <- all_reclassified_rasters()[[layer]]
          file_path <- file.path(temp_dir, paste0(layer, ".tif"))
          writeRaster(layer_file, file_path, overwrite = TRUE)
          file_paths <- c(file_paths, file_path)
        } else if (layer %in% names(all_shapefiles())) {
          layer_file <- all_shapefiles()[[layer]]
          file_path <- file.path(temp_dir, layer)
          st_write(layer_file, file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
          file_paths <- c(file_paths, file_path)
        }
      }
      
      # Create a zip file with the selected layers
      zip::zipr(zipfile = file, files = file_paths)
      
      # Show a notification
      showNotification("Layers saved", type = "message")
    }
  )
  
  ### 12. Buffer button
  observeEvent(input$buffer_button, {
    req(input$buffer_value)
    req(input$sp_list)
    # Get the selected layer
    selected_layer <- input$sp_list
    print (selected_layer)
    
    # Get the buffer distance
    buffer_value <- input$buffer_value
    print (buffer_value)
    
    # Check if a layer is selected
    if (is.null(selected_layer) || selected_layer == ""){
      showNotification("Please select a layer to buffer.", type = "error")
      return(NULL)
    }
    
    # Check if a buffer distance is provided
    if (is.null(buffer_value) || buffer_value == ""){
      showNotification("Please provide a buffer distance.", type = "error")
      return(NULL)
    }
    list <- shortest_path_layers$list
    print (list)
    
    # Check if the selected layer is a shapefile
    if (selected_layer %in% names(list)){
      print (list[[selected_layer]])
      # Perform the buffer operation
      buffered_layer <- st_buffer (list[[selected_layer]], buffer_value)
      print (buffered_layer)
      print ("buffer executed")
      # perfom dissolve operation
      buffered_layer <- st_union(buffered_layer)
      print ("buffer dissolve executed")
      
      # Convert directly to sf if needed
      buffered_layer <- st_sf(geometry = buffered_layer)
      print ("Convert executed")
      print (buffered_layer)
      
      # Add the buffered layer to the map
      addShapefileToMap(buffered_layer, paste ("Buffered",selected_layer))
      # Show a notification
      showNotification("Buffer operation completed.", type = "message")
    } else {
      showNotification("Buffer operation is only supported for shapefiles.", type = "error")
    }
  })
  
  observeEvent(input$raster, {
    if (!is.null(input$raster)) {
      shinyjs::show("add_raster") # Show the add_raster button
    }
    showModal(modalDialog(
      title = "Input raster Name",
      textInput("raster_name", "raster Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_rst_tab", "OK")
      )
    )
    )
  })
  
  observeEvent(input$new_rst_tab, {
    
    removeModal()
    # Process the uploaded raster
    raster_data_processed <- reactive({
      processRaster (input$raster, input$raster_name)
    })
    
    # Add the new raster to all rasters list ################### Maybe add to another list and create single legends for each one
    raster_data_processed_value <- raster_data_processed()
    raster_name <- input$raster_name
    updateList (raster_data_processed_value, raster_name, all_reclassified_rasters)
    
    # Append the new tab to the UI
    appendTab(inputId = "dynamic_tabs",
              tabPanel(raster_name,  # Use shapefile_name as tab title
                       h3(paste("Raster carregado:", raster_name)),
                       # Hidden numeric input
                       numericInput(paste0("number_of_intervals_", raster_name), "Número de intervalos:", value = 5, min = 1, max = 20),
                       DTOutput(paste0("Raster_table_", raster_name)),  # Updated to match output ID
                       actionButton(paste0("uploaded_rst_reclassification_", raster_name), "Aplicar reclassificação")
              )
    )
    # Observe number_of_intervals input
    number_of_intervals_id <- paste0("number_of_intervals_", raster_name)  # Ensure unique dynamic input ID
    
    observeEvent(input[[number_of_intervals_id]], {
      
      req(input[[number_of_intervals_id]])  # Ensure the input exists
      
      number_of_intervals <- input[[number_of_intervals_id]]
      
      raster_data_processed_value <- all_reclassified_rasters()[[raster_name]]
      # Extract raster values as numeric vector
      raster_values <- getValues(raster_data_processed_value)
      
      intervals <- calc_intervals(raster_values, input[[number_of_intervals_id]])
      
      # Update the reactive list of data frames
      # raster_df_list_temp <- raster_df_list()
      # raster_df_list_temp[[raster_name]] <- intervals
      # raster_df_list(raster_df_list_temp)
      updateList (intervals, raster_name, raster_df_list)
      
      # Render the data table with editable cells
      output[[paste0("Raster_table_", raster_name)]] <- renderDT({
        datatable(
          raster_df_list()[[raster_name]],  # Use the calculated intervals
          editable = list(target = "cell", disable = list(columns = 0)),  # Disable editing for the 'interval' column
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE)  # Adjust options as needed
        )
      })
    })
    
    # Observe cell edits and update the data frame
    observeEvent(input[[paste0("Raster_table_", raster_name, "_cell_edit")]], {
      info <- input[[paste0("Raster_table_", raster_name, "_cell_edit")]]
      raster_df_list_temp <- raster_df_list()
      df <- raster_df_list_temp[[raster_name]]
      
      # Extract the edited cell info
      i <- as.numeric(info$row)
      j <- as.numeric(info$col)
      new_value <- as.numeric(info$value)
      
      # Update the data frame
      df[i, j + 1] <- new_value
      
      if (info$col == 0) {  # Assuming `min_val` is first column, `max_val` second
        if (df[i, "min_val"] >= df[i, "max_val"]) {
          showNotification("O valor mínimo deve ser menor que o valor máximo.", type = "warning")
          df[i, j + 1] <- df[i + 1, "min_val"]
        } else {
          df[i + 1, "min_val"] <- new_value  # Update the next row's min_val
        }
      }
      
      # Update the reactive list of data frames
      raster_df_list_temp[[raster_name]] <- df
      raster_df_list(raster_df_list_temp)
      
      # Print the updated reactive data frame for debugging
      print(raster_df_list()[[raster_name]])
    })
    
    # Observe the reclassify button click
    observeEvent(input[[paste0("uploaded_rst_reclassification_", raster_name)]], {
      # Extract the reclassification data frame
      reclass_df <- raster_df_list()[[raster_name]]
      
      # Ensure that the user has filled in all reclassification values
      if (any(is.na(reclass_df$value))) {
        showNotification("Por favor, preencha todos os valores de reclassificação antes de prosseguir.", type = "error")
        return()
      }
      
      # Perform reclassification
      new_rec_raster <- all_reclassified_rasters()[[raster_name]]  # Get the current raster
      reclassified_raster <- reclassify_raster(new_rec_raster, reclass_df)
      
      reclassified_raster_name <- paste("Reclassified raster for", raster_name)
      # Add the new raster to all rasters list
      updateList (reclassified_raster, reclassified_raster_name, all_reclassified_rasters)
      
      # Add the resulting raster to the map
      # addRasterToMap(reclassified_raster, reclassified_raster_name)
    })
  })
  
  # Observing Help button click to trigger modal dialog
  observeEvent(input$help_button, {
    showModal(createHelpModal())
  })
  # Update tabset panel within modal
  observeEvent(input$reclassify_table, {
    updateTabsetPanel(session, "help_tabs", selected = "Aba de reclassificação")
  })
  
  observeEvent(input$overlay_layers, {
    updateTabsetPanel(session, "help_tabs", selected = "Sobrepor camadas")
  })
  
  observeEvent(input$download_layers, {
    updateTabsetPanel(session, "help_tabs", selected = "Download")
  })
  
  observeEvent(input$main_page, {
    updateTabsetPanel(session, "help_tabs", selected = "Navegação")
  })
  
}