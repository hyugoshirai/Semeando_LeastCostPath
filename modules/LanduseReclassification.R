DefaultRasterReclassification <- function(id, input, output, session, landuse_df, layer_name) {
  
  # Initialize reactive values to track reclassification data frames
  rv <- reactiveValues(df = NULL)
  
  observeEvent(input$dropdownDefaultLayers, {
    DefaultRaster_name <- input$dropdownDefaultLayers
    if (DefaultRaster_name != "") {
      default_raster <- default_layers[[DefaultRaster_name]]
      print(DefaultRaster_name)
      
      # Clear the previous DT tables
      output[[id]] <- renderDT({NULL})
      output[["first_condition"]] <- renderDT({NULL})
      output[["second_condition"]] <- renderDT({NULL})
      output[["third_condition"]] <- renderDT({NULL})
      
      # Reset the reclassification data frame
      rv$df <- NULL
      if (DefaultRaster_name %in% c("Uso_do_Solo", "Areas_Especiais_rst", "PUC", "propriedades_rst")){
        print("first condition - Categorical Values")
        shinyjs::hide("defaultrst_number_of_intervals")
        if (DefaultRaster_name == "Uso_do_Solo") {
          rv$df <- landuse_df}
        if (DefaultRaster_name == "Areas_Especiais_rst") {
          rv$df <- spa_df}
        if (DefaultRaster_name == "PUC") {
          rv$df <- PUC_df}
        if (DefaultRaster_name == "propriedades_rst") {
          rv$df <- Property_df}
        
        output[["first_condition"]] <- renderDT({
          datatable(rv$df, editable = list(target = "cell", disable = list(columns = c(1, 2))))
        })
        
        observeEvent(input[[paste0("first_condition", "_cell_edit")]], {
          new_data <- input[[paste0("first_condition", "_cell_edit")]]
          row <- new_data$row
          col <- new_data$col
          value <- as.numeric(new_data$value)
          
          if (col == 0 && value <= 0) {
            shinyalert(
              title = "Invalid Value",
              text = sprintf("Invalid value '%s' in row %d, column 'min_val'. min_val must be greater than 0.", value, row),
              type = "error"
            )
          } else {
            rv$df[row, col] <<- value
          }
          print(rv$df)
        })
      } else {
        raster_values <- (values(default_raster, na.rm = TRUE))
        unique_values <- unique(raster_values)
        if (length(unique_values) == 1) {
          print("second condition - Unique value")
          shinyjs::hide("defaultrst_number_of_intervals")
          
          rv$df <- data.frame(
            raster_value = unique_values,
            new_value = 1
          )
          
          output[["second_condition"]] <- renderDT({
            datatable(rv$df, editable = list(target = "cell", disable = list(columns = c(1))))
          })
          
          observeEvent(input[[paste0("second_condition", "_cell_edit")]], {
            new_data <- input[[paste0("second_condition", "_cell_edit")]]
            row <- new_data$row
            col <- new_data$col
            value <- as.numeric(new_data$value)
            
            rv$df[row, col] <<- value
            print(rv$df)
          })
          
        } else {
          print("third condition - Continuous data")
          shinyjs::show("defaultrst_number_of_intervals")
          
          observeEvent(input$defaultrst_number_of_intervals, {
            # Record the start time
            start_time <- Sys.time()
            cat("Processing started at: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
            breaks <- classIntervals(raster_values, n = input$defaultrst_number_of_intervals, style = "equal")$brks
            # , intervalClosure = c("left", "right"), dataPrecision = NULL,
            # warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
            # gr = c("[", "]"))
            # breaks <- classify_intervals(raster_values, n = input$defaultrst_number_of_intervals, style = "equal")$brks
            # Record the end time
            end_time <- Sys.time()
            cat("Processing finished at: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
            cat("Total processing time: ", round(difftime(end_time, start_time, units = "secs")), " seconds\n")
            min_val <- round(breaks[-length(breaks)], 2)
            max_val <- round(breaks[-1], 2)
            rv$df <- data.frame(
              min_val = min_val,
              max_val = max_val,
              value = rep(1, length(breaks) - 1)
            )
            
            output[["third_condition"]] <- renderDT({
              datatable(
                rv$df,
                editable = list(target = "cell", disable = list(columns = c(1))),
                rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE)
              )
            })
          })
          
          observeEvent(input[[paste0("third_condition", "_cell_edit")]], {
            info <- input[[paste0("third_condition", "_cell_edit")]]
            df <- rv$df
            i <- info$row
            j <- info$col
            new_value <- as.numeric(info$value)
            
            # Store previous values for validation
            prev_min_value <- df[i, 1]
            prev_max_value <- df[i, 2]
            prev_value <- df[i, 3]
            
            valid_edit <- TRUE
            
            if (j == 0) {  # Edit min_val
              # Update min_val
              df[i, 1] <- new_value
              if (i < nrow(df)) { # Update max_val of the current row to match min_val of the next row
                df[i-1, 2] <- new_value
              }
              
              # Validate min_val and max_val within the same row
              if (df[i, 1] >= df[i, 2]) {
                valid_edit <- FALSE
              }
              
            } else if (j == 2) {  # Edit value
              if (new_value <= 0) {
                valid_edit <- FALSE
              } else {
                df[i, 3] <- new_value
              }
            }
            
            if (valid_edit) {
              rv$df <- df
            } else {
              # Revert to previous values if the edit is invalid
              df[i, 1] <- prev_min_value
              df[i, 2] <- prev_max_value
              df[i, 3] <- prev_value
              shinyalert(
                title = "Invalid Value",
                text = "Invalid min_val. Ensure min_val is less than max_val and values are greater than 0.",
                type = "error"
              )
            }
            
            # Re-render the DataTable with the updated df
            output[["third_condition"]] <- renderDT({
              datatable(
                df,
                editable = list(target = "cell", disable = list(columns = 1)),
                rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE)
              )
            })
            
            print(rv$df)
          })
        }
      }
      }
  })
  
  observeEvent(input$apply_changes, {
    DefaultRaster_name <- input$dropdownDefaultLayers
    default_raster <- default_layers[[DefaultRaster_name]]
    reclass_df <- rv$df
    
    # Create a reclassification matrix
    if (DefaultRaster_name %in% c("Uso_do_Solo", "Areas_Especiais_rst", "PUC", "propriedades_rst")){
      reclass_matrix <- as.matrix(reclass_df[, c("raster_value", "new_value")])
    } else {
      reclass_matrix <- as.matrix(reclass_df)
    }
    
    # Perform the reclassification
    print ("reclass_matrix")
    print (reclass_matrix)
    print ("default_raster")
    print (default_raster)
    
    # Record the start time
    reclassified_raster <- classify(default_raster, reclass_matrix, include.lowest=TRUE)
    
    print ("reclassified_raster")
    print (reclassified_raster)
    
    
    # Assign a name to the reclassified raster
    name <- paste0("Reclassified for ", DefaultRaster_name)
    
    # Update the reactive list of reclassified rasters
    updateList (reclassified_raster, name, all_reclassified_rasters)

    print ("all_reclassified_rasters")
    print (all_reclassified_rasters())

    # addRasterToMap(reclassified_raster, name)
  })
}
