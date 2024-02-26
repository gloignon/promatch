server <- function(input, output, session) {
  
  ## Constants ----
  DOWNLOAD_SIZE_LIMIT <- 5  # max size in megs, above that the matched data will be downloaded as a .zip 
  
  # methods that don't support caliper
  v_no_caliper_methods <- c("quick", "optimal", "exact", "NULL")
  
  # distance that don't support caliper
  v_no_caliper_distances <- c("mahalanobis", "robust_mahalanobis")
  
  ## temp folder management ----
  session_temp_dir <- makeSessionTempDir()  # temp dir for unzipping .zip files
  
  # Function to clean up session-specific temp directory
  # This will delete the uploaded zip file(s) and if it was created the long format zipped .csv
  sessionCleanup <- function() {
    if (dir.exists(session_temp_dir)) {
      unlink(session_temp_dir, recursive = TRUE)
    }
  }
  
  # Register the cleanup function to run when the session ends
  session$onSessionEnded(function() {
    sessionCleanup()
  })
  
  ## setting up ----
  modalContent <- reactiveVal("")  # for info boxes

  ## Advanced tab ----
  # Reactive value for storing available variables
  availableVarsOnMatch <- reactiveVal()
  
  # Observe changes in data availability
  observe({
    # When data becomes available
    if (data_available()) {
      # Get all variables
      allVars <- names(df_matched_data())
      
      # Exclude selected variables in depVar, covariates, and exact_match
      selectedVars <- unique(c(input$depVar, input$covariates, input$exact_match))
      availableVars <- setdiff(allVars, selectedVars)
      
      # Update the multiInput choices
      updateMultiInput(
        session = session, 
        inputId = "advancedVars",
        choices = availableVars
      )
    }
  })
  
  # Observe changes in availableVarsOnMatch
  observe({
    updateMultiInput(
      session = session, 
      inputId = "advancedVars",
      choices = availableVarsOnMatch()
    )
  })
  
  ## Multiple comparisons ----
  observeEvent(input$run_test, {
    selected_test <- input$repeated_tests
    
    # Using try to catch errors in pivot_matched_data
    result <- try(
      pivot_matched_data(df_matched_data, input$advancedVars)
    )
    
    # Check if there was an error in pivot_matched_data
    if (inherits(result, "try-error")) {
      showModal(modalDialog(
        title = "Error",
        "An error occurred while transforming to long format. Please check your data, especially the selected measurement variables.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return() # Stop further execution if error occurred
    }
    
    # If no error, assign result to df_long_data
    df_long_data <- result  
    
    # group_var <- sym(input$depVar)
    df_long_data$.group <- df_long_data[[input$depVar]]
    unique_levels <- unique(df_long_data$.group)
    print(unique_levels)
    
    # # ensure the levels are coded as 1 and 0{{}}
    # df_long_data <- df_long_data %>%
    #   mutate(.group = recode(.group, unique_levels[1] = 1, unique_levels[2] = 0))
    
    if (selected_test == "T-test") {
      # Run t-test comparisons here
      result_t_test <- try(
        df_long_data %>% group_by(variable) %>% 
          rstatix::t_test(score ~ .group) %>% 
          mutate(p = format.pval(p, eps = .001)) %>% 
          select(-c(".y.", "group1", "group2", "n1", "n2")) %>% 
          mutate(across(where(is.numeric), round, 2)) %>% 
          as.data.frame()
      )
      # Check if there was an error
      if (inherits(result_t_test, "try-error")) {
        showModal(modalDialog(
          title = "Error",
          "An error occurred while running the t-tests. Please check if the selected variables are appropriate for a t-test.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        tests_summary("Error.") 
      } else {  # No error
        tests_summary(result_t_test) 
      }
    } else if (selected_test == "Proportions") {
      df_props <- df_long_data %>%
        filter(!is.na(score)) %>% 
        group_by(.group, variable) %>%
        summarise(
          success = mean(score, na.rm = T),
          fail = 1 - success,
          n = n(),
          n_success = n * success,
          .groups = 'drop'
        ) %>% pivot_wider(
          id_cols = variable,
          names_from = .group, 
          values_from = c(success, n, n_success)
        ) %>% 
        group_by(variable) %>%
        mutate(across(where(is.numeric), round, 2)) %>% 
        mutate(p_value = prop.test(
          x = c(!!sym(paste0("n_success_", unique_levels[1])),
                !!sym(paste0("n_success_", unique_levels[2]))),
          n = c(!!sym(paste0("n_", unique_levels[1])),
                !!sym(paste0("n_", unique_levels[1])))
        )$p.value %>% round(4) %>% format.pval(eps = .001)) %>% 
        select(variable, 
               !!sym(paste0("success_", unique_levels[1])),
               !!sym(paste0("success_", unique_levels[2])),
               p_value) %>% 
        as.data.frame()
      tests_summary(df_props)
    } else if(selected_test == "Brunner-Munzel") {
      result_BM_test <- try(
        df_long_data %>% group_by(variable) %>% 
          brunner_munzel_test(score ~ .group) %>% 
          # mutate(p = format.pval(p, eps = .001)) %>% 
          # select(-c(".y.", "group1", "group2", "n1", "n2")) %>% 
          mutate(across(where(is.numeric), round, 2)) %>% 
          as.data.frame()
      )
      # Check if there was an error
      if (inherits(result_BM_test, "try-error")) {
        showModal(modalDialog(
          title = "Error",
          "An error occurred while running the B-M tests. Please check if the selected variables are appropriate for a t-test.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        tests_summary("Error.") 
      } else {
        tests_summary(result_BM_test) 
      }
    }
    
    # Store and define the summary
    # tests_summary(summary(m.out))
    output$repeated_tests_output <- renderPrint({
      # req(tests_summary())  # Require that output_summary is not NULL
      tests_summary()       # Output the stored summary
    })
    
  })
  
  
  ## language selector ----
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    # print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)
  })
  
  # Initialize df_unmatched as a reactiveVal
  df_unmatched <- reactiveVal()
  
  ## File upload ----
  observe({
    req(input$file1)
    inFile <- input$file1
    
    if (is.null(inFile)) { # no file uploaded
      return(NULL)  # let's get out
    }
    
    # Check file type and read accordingly
    if (grepl("\\.csv$", inFile$name)) {
      df <- read.csv(inFile$datapath)
    } else if (grepl("\\.xlsx$", inFile$name)) {
      df <- read_excel(inFile$datapath)
    } else if (grepl("\\.zip$", inFile$name)) {
      # Clear the directory if it exists, or create it if it doesn't
      zip_dir <- file.path(session_temp_dir, "unzipped_files")
      if (dir.exists(zip_dir)) {
        unlink(zip_dir, recursive = TRUE)
      }
      dir.create(zip_dir)
      
      # Unzip the file
      unzip(inFile$datapath, exdir = zip_dir)
      
      # Read the file
      files <- list.files(zip_dir, pattern = "\\.csv$|\\.xlsx$")
      if (length(files) == 0) {
        stop("Error: No CSV or Excel files found in the ZIP.")
      } else if (length(files) > 1) {
        stop("Error: Multiple files found in the ZIP. Please include only one CSV or Excel file.")
      }
      file_path <- file.path(zip_dir, files[1])
      if (grepl("\\.csv$", files[1])) {
        df <- read.csv(file_path)
      } else if (grepl("\\.xlsx$", files[1])) {
        df <- read_excel(file_path)
      } else {
        stop("Error: Invalid file type in ZIP.")
      }
    } else {
      stop("Error: invalid file type.")
    }
    
    # df <- janitor::clean_names(df, case = "none")
    df <- cleanUploadedFile(df)
    df_unmatched(df)
  })
  
  ## download button disable ----
  observe({
    # If data is available, enable the download button, otherwise disable it
    if(data_available()) {
      shinyjs::enable("download")
      shinyjs::enable("downloadLongFormat")
    } else {
      shinyjs::disable("download")
      shinyjs::disable("downloadLongFormat")
    }
    
    if (length(input$advancedVars) > 0) {
      # Enable download button when at least one variable is selected
      shinyjs::enable("downloadLongFormat")
    } else {
      # Disable download button when no variable is selected
      shinyjs::disable("downloadLongFormat")
    }
  })
  
  # This will hold the summary of m.out
  output_summary <- reactiveVal()
  
  tests_summary <- reactiveVal()
  
  # this will hold m.out, the matching object
  match_object <- reactiveVal()
  
  # This reactive expression will hold the matched data for download
  df_matched_data <- reactiveVal()  # Initialize a reactive value
  
  # Define a reactive expression that checks if the matched data is available and non-empty
  data_available <- reactive({
    # Check if df_matched_data is non-null and has more than 0 rows
    !is.null(df_matched_data()) && nrow(df_matched_data()) > 0
  })
  
  # Update UI based on uploaded file
  output$varSelect <- renderUI({
    df <- df_unmatched()
    if (exists("df") && is.data.frame(df)) {
      list(
        selectInput(
          "depVar",
          label = i18n$t("Select Treatment (Group) Variable"),
          choices = filterDichotomousVariables(df)
        ),
        selectInput(
          "covariates",
          label = i18n$t("Select Covariates"),
          choices = names(df),
          multiple = TRUE,
          selectize = TRUE
        ),
        selectInput(
          "exact_match",
          label = i18n$t("Select Exact matching variables (optional)"),
          choices = names(df),
          multiple = TRUE,
          selectize = TRUE
        ),
        actionButton(inputId = "var_config", icon = icon("gear"), label = "Variable properties")
      )
    }
  })
  
  ## Caliper checkbox and slider toggles ----
  output$caliper_checkbox <- renderUI({
    # Check if the method and distance allow for caliper
    if (!input$method %in% v_no_caliper_methods && !input$distance %in% v_no_caliper_distances) {
      # Return the checkbox
      checkboxInput("showSlider", "Use caliper (matching tolerance)", value = input$showSlider)
    }

  })
  
  output$dynamicSlider <- renderUI({
    if (!is.null(input$showSlider) && input$showSlider && 
        !is.null(input$method) && !input$method %in% v_no_caliper_methods &&
        !is.null(input$distance) && !input$distance %in% v_no_caliper_distances) {
      
      # Use a default value for 'caliper' if it's NULL or uninitialized
      caliperValue <- ifelse(is.null(input$caliper), 0.2, input$caliper)
      
      sliderInput("caliper", "Caliper", min = 0.05, max = 1, value = caliperValue, step = 0.05)
    }
  })
  
  ## Matching ----
  observeEvent(input$match, {
    req(input$depVar, input$covariates)
    df <- df_unmatched()
    df[df == ""] <- NA  # blank spaces are interpreted as NAs
    
    # Ensure the dependent variable is a factor
    df[[input$depVar]] <- as.factor(df[[input$depVar]])
    
    # Check if 2 levels
    if (nlevels(df[[input$depVar]]) != 2) {
      showModal(modalDialog(
        title = i18n$t("Error"),
        i18n$t("The treatment variable must have exactly two levels to perform matching."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # if column called weights, remove
    if("weights" %in% names(df)) {
      df[["weights"]] <- NULL
    }
    
    # if column called subclass, remove
    if("subclass" %in% names(df)) {
      df[["subclass"]] <- NULL
    }
    
    # Count initial number of rows
    initial_row_count <- nrow(df)
    
    # If a column is called "distance" we need to remove it
    df <- df[, !names(df) %in% "distance", drop = FALSE]
    
    # Removing rows with NA in the dependent variable or any of the covariates
    # df_clean <- na.omit(df[, c(input$depVar, input$covariates)])
    
    # remove NAs from variables
    if (!is.null(input$exact_match)) {  # if the exact match field is NOT empty
      # print(paste0("You have ordered an exact match too: ", input$exact_match))
      complete_rows <- complete.cases(df[, c(input$depVar, input$covariates, input$exact_match)])
    } else {
      complete_rows <- complete.cases(df[, c(input$depVar, input$covariates)])
    }
    df_clean <- df[complete_rows, ]  # Remove rows with NA from the entire data frame (will keep all columns)
    
    
    # Count new number of rows after removal
    new_row_count <- nrow(df_clean)
    
    # if we removed rows, use a modal to tell the user how many rows were removed
    if (new_row_count < initial_row_count) {
      # showModal(modalDialog(
      #   title = i18n$t("Had to remove some rows..."),
      #   paste(i18n$t("Removed"), initial_row_count - new_row_count, i18n$t("rows with missing values.")),
      #   easyClose = TRUE,
      #   footer = modalButton("OK")
      # ))
      output_summary( paste(i18n$t("Had to remove"), initial_row_count - new_row_count, i18n$t("rows that had missing values.")))
    }
    
    # if the new row count is zero, display the following modal
    if (new_row_count == 0) {
      showModal(modalDialog(
        title = i18n$t("Error"),
        i18n$t("Error: Not enough data to perform matching after removing rows with 'NA' values."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    if (input$showSlider == FALSE | is.null(input$caliper) |
        input$method %in% v_no_caliper_methods | input$distance %in% v_no_caliper_distances) {
      caliper <- NULL
    } else {
      caliper <- input$caliper
    }
    
    ## the actual matching ----
    # display a progress bar while matching
    withProgress(message = i18n$t("Preparing matching..."), value = 0, {
        # Perform the matching using the matchit function
        formula <- as.formula(paste(input$depVar, "~", paste(input$covariates, collapse = " + ")))
        
        # prepare the method var
        method <- NULL
        if (input$method != "NULL") {
          method <- input$method
        }
        # Prepare the exact match variable, if needed
        exact_match_vars <- NULL
        if (!is.null(input$exact_match) && length(input$exact_match) > 0) {
          # Split the string by '+' and trim whitespaces
          exact_match_vars <- unlist(strsplit(input$exact_match, "\\s*\\+\\s*"))
          # Remove any leading or trailing whitespace
          exact_match_vars <- trimws(exact_match_vars)
          # print(paste0("About to do an exact match on ", paste(exact_match_vars, collapse = ", ")))
        }
        
        # update progress bar
        setProgress(0.2, message = i18n$t("Matching..."))
        
        # Perform the matching using the matchit function
        m.out <- matchit(
          formula, 
          method = method, 
          distance = input$distance, 
          data = df_clean, 
          exact = exact_match_vars,
          caliper = caliper,
          link = "linear.logit", # will apply the caliper to the logit
          std.caliper = TRUE,
          verbose = TRUE
        )
        
        match_object(m.out)  # copy the matched object to the reactive value; used for model assessment
        
        # update progress bar
        setProgress(.4, message = i18n$t("Matching complete. Extracting matched data..."))
        
        # Extract the matched data
        matched <- match.data(object = m.out, data = df_clean)
        #TODO: user should be able to choose if match.data will output the distance or the prop.score
        
        # Update the reactive value with the matched data
        df_matched_data(matched)  # This line updates the reactive value
        
        # update progress bar
        setProgress(.8, message = i18n$t("Preparing matched data for download..."))
        
        ### Prepare the file for download ----
        main_output_dir <- file.path(session_temp_dir, "main_output")
        file_path_name <- file.path(main_output_dir, "output.csv")
  
        if (dir.exists(main_output_dir)) {
          unlink(main_output_dir, recursive = TRUE)
        }
        dir.create(main_output_dir)
        write.csv(x = df_matched_data(), file = file_path_name,
                  row.names = TRUE)
        
        # zip if needed
        file_size <- file.info(file_path_name)$size / (1024^2) # Size in Megabytes
        if (file_size > DOWNLOAD_SIZE_LIMIT) {
          zip(zipfile = file.path(main_output_dir, "output.zip"), 
              files = file_path_name)
          unlink(file_path_name)
        }
        
        # Get the matching summary, rendered as text
        summary_text <- capture.output(summary(m.out))
      
        # add the summary at the end of output_summary
        output_summary(paste0(output_summary(), "\n", paste(summary_text, collapse = "\n")))
        
        output$matchSummary <- renderText({
          req(output_summary())  # Require that output_summary is not NULL
          output_summary()       # Output the stored summary
        })
        
        # final progress update
        setProgress(1, message = i18n$t("Matching complete. Matched data available for download."))
        
        # Remove the loading message and show the "Done!" message
        # removeModal()
        # showModal(modalDialog(
        #   title = i18n$t("Success"),
        #   i18n$t("Done! The matching is complete. You can now download the matched data."),
        #   easyClose = TRUE
        # ))
    })  # end of withProgress statement
  })  # end of observeEvent matching
  
  # info box
  observeEvent(input$info_click, {
    content <- switch(input$info_click,
                      "info_file" = i18n$t("This will upload your file. Accepted formats are .csv, .xlsx. Zipped (.zip) csv and excel are also good."),
                      # ... add more cases as needed ...
                      stop("Unknown info icon")
    )
    modalContent(content)  # Update the reactive value
    print(content)
    showModal(modalDialog(
      title = "Information",
      content,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }) 
  
  # Main download handler -----
  output$download <- downloadHandler(
    filename = function() {
      files <- list.files(file.path(session_temp_dir, "main_output"), pattern = "\\.csv$|\\.zip$")
      if (grepl("\\.csv$", files[1])) {
        return(paste("matched-", input$method, "-", input$distance, "-", Sys.Date(), ".csv", sep = ""))
      } else {
        return(paste("matched-", input$method, "-", input$distance, "-", Sys.Date(), ".zip", sep = ""))
      }
    },
    content = function(file) {
      # Confirm that df_matched_data is not empty
      if (!data_available() || is.null(df_matched_data()) || nrow(df_matched_data()) == 0) {
        showModal(modalDialog(
          title = i18n$t("Error"),
          i18n$t("No matched data available for download."),
          easyClose = TRUE
        ))
      } else {
        # file.copy(file.path(session_temp_dir, "output.csv"), file)
        files <- list.files(file.path(session_temp_dir, "main_output"), pattern = "\\.csv$|\\.zip$")
        file_path <- file.path(session_temp_dir, "main_output", files[1])
        file.copy(file_path, file)
      }
    }
  )
  # output$download <- downloadHandler(
  #   filename = function() {
  #     orig_dir <- getwd()
  #     main_output_dir <- file.path(session_temp_dir, "main_output")
  #     if (dir.exists(main_output_dir)) {
  #       unlink(main_output_dir, recursive = TRUE)
  #     }
  #     dir.create(main_output_dir)
  #     orig_dir <- getwd()
  #     setwd(main_output_dir)
  #     file_name <- paste0("matched-long_format-", Sys.Date(), ".csv")
  #     write.csv(x = df_matched_data(),
  #               file = file_name, 
  #               row.names = FALSE)
  #     file_size <- file.info(file_name)$size / (1024^2) # Size in Megabytes
  #     setwd(orig_dir)  # let's go back to the original directory
  #     
  #     # Determine the filename based on size
  #     if (file_size > 1) {
  #       zip(zipfile = file, files = paste0("matched-long_format-", Sys.Date(), ".csv"))
  #       # return(paste0("matched-", Sys.Date(), ".zip"))
  #       return(paste0("matched-", input$method, "-", input$distance, "-", Sys.Date(), ".zip", sep = ""))
  #     } else {
  #       return(paste0("matched-", input$method, "-", input$distance, "-", Sys.Date(), ".csv", sep = ""))
  #     }
  #   },
  #   content = function(file) {
  #     # Confirm that df_matched_data is not empty
  #     if (!data_available() || is.null(df_matched_data()) || nrow(df_matched_data()) == 0) {
  #       showModal(modalDialog(
  #         title = i18n$t("Error"),
  #         i18n$t("No matched data available for download."),
  #         easyClose = TRUE
  #       ))
  #     } else {
  #       # Write the data to CSV
  #       files <- list.files(file.path(session_temp_dir, "main_output"), pattern = "\\.csv$|\\.zip$")
  #       file_path <- file.path(session_temp_dir, "main_output", files[1])
  #       file.copy(file_path, file)
  #     }
  #   }
  # )

  # Long format download handler ----
  output$downloadLongFormat <- downloadHandler(
    filename = function() {
      paste0("matched-long_format-", Sys.Date(), ".zip")
    },
    content = function(file) {
      if (!data_available() || is.null(df_matched_data()) || nrow(df_matched_data()) == 0) {
        showModal(modalDialog(
          title = i18n$t("Error"),
          i18n$t("No matched data available for download."),
          easyClose = TRUE
        ))
        return()
      }

      # Get the columns to pivot
      cols_to_pivot <- input$advancedVars
      req(length(cols_to_pivot) > 0)  # Ensure there's at least one column selected

      df_long_data <- pivot_matched_data(df_matched_data, cols_to_pivot)

      # Create a temporary file for the CSV
      zip_dir <- file.path(session_temp_dir, "csv2zip")
      if (dir.exists(zip_dir)) {
        unlink(zip_dir, recursive = TRUE)
      }
      dir.create(zip_dir)
      orig_dir <- getwd()
      setwd(zip_dir)

      write.csv(x = df_long_data,
                file = paste0("matched-long_format-", Sys.Date(), ".csv"),
                row.names = FALSE)

      # Create a ZIP file containing the CSV
      zip(zipfile = file, files = paste0("matched-long_format-", Sys.Date(), ".csv"))
      setwd(orig_dir)  # let's go back to the original directory

      # # Clean up the temporary CSV file
      # unlink(temp_csv)

    }
  )

  
  # display about page in right language
  output$about_content <- renderUI({
    lang <- input$selected_language
    print(lang)
    filePath <- if (lang == "fr") "www/apropros.html" else "www/about.html"
    # print(paste("Attempting to load file:", filePath))
    # print(paste("File exists:", file.exists(filePath)))
    includeHTML(filePath)
  })
  
  # Diagnostic plots (with cobalt's love plot) ----
  # Render the UI for the plot
  output$love_plot <- renderUI({
    # Only create the plotOutput if match_obj is not NULL
    req(match_object())
    plotOutput("lovePlot")
  })
  
  
  # Generate the love plot
  output$lovePlot <- renderPlot({
    req(match_object())  # Ensure that match_obj is not NULL
    
    # if a caliper was specified, used that, otherwise use 0.2
    caliper <- ifelse(is.null(input$caliper), 0.2, input$caliper)
    
    # Generate the love plot for the match object
   love.plot(match_object(),
              threshold = caliper,  # Set your threshold for standardized mean differences
              abs = TRUE,
              thresholds = .2,
              drop.distance = TRUE,
              stats = c("mean.diffs", "variance.ratios")
    )
    
  })
  

  
#   # Diagnostic plots ----
#   # Create plot tag list 
#   output$diagnosticPlots <- renderUI({
#     req(df_matched_data(), input$covariates, input$depVar)
#     df <- df_matched_data()
# 
#     # Filter only continuous covariates
#     continuous_covariates <- Filter(function(v) is_continuous(df, v), input$covariates)
#     req(length(continuous_covariates) > 0)
# #
# #     plot_output_list <- lapply(seq_along(continuous_covariates), function(i) {
# #       plotname <- paste("plot", i, sep = "_")
# #       plotOutput(plotname, width = "300", inline = FALSE)
# #     })
# #     do.call(tagList, plot_output_list)
#     # Wrap plotOutput in a div with the .balance-plot class
#     plot_output_list <- lapply(seq_along(continuous_covariates), function(i) {
#       plotname <- paste("plot", i, sep = "_")
#       div(plotOutput(plotname, width = "400"), class = "balance-plot")
#     })
# 
#     # Wrap the list of divs in another div with .plots-container class
#     div(class = "plots-container", plot_output_list)
#   })

  # # Dynamically create renderPlot for each plot
  # observe({
  #   df <- df_matched_data()  # Ensure data is re-evaluated on change
  #   continuous_covariates <- Filter(function(v) is_continuous(df, v), input$covariates)
  # 
  #   lapply(seq_along(continuous_covariates), function(i) {
  #     local({
  #       plot_id <- paste("plot", i, sep = "_")
  #       covariate <- continuous_covariates[i]
  # 
  #       output[[plot_id]] <- renderPlot({
  #         ggplot(df,
  #                aes_string(
  #                  x = input$depVar,
  #                  y = covariate,
  #                  group = input$depVar
  #                )) +
  #           geom_boxplot() +
  #           labs(y = covariate, x = input$depVar
  #                # title = paste("Boxplot of", covariate, "by", input$depVar)
  #                ) +
  #           theme_minimal(base_size = 14) +
  #           theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  # 
  #       })
  #     })
  #   })
  # })
  
  # Variable type config ----
  # Observe event for var_config button
  observeEvent(input$var_config, {
    # Create a modal dialog when the button is clicked
    showModal(modalDialog(
      title = "Variable Configuration",
      
      # UI for selecting variable types
      h4("Select Variable Types"),
      uiOutput("var_type_ui"),
      
      # Modal footer with action buttons
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_var_config", "Save Configuration")
      )
    ))
  })
  
  # UI for variable type selection with current type display
  output$var_type_ui <- renderUI({
    req(df_unmatched())  # Ensure data is available
    df <- df_unmatched()  # Your actual dataset
    combined_vars <- c(input$covariates, input$exact_match)
    
    my_icons <- c("<i class='fa-solid fa-ruler'></i> Continuous",
                  "<i class='fa-solid fa-list'></i> Nominal",
                  "<i class='fa-solid fa-arrow-up-wide-short'></i> Ordinal"
    )

    # Create a selectInput for each variable
    var_type_inputs <- lapply(combined_vars, function(var) {
      current_type <- translate_var_type(var, df)
      shinyWidgets::pickerInput(
          inputId = paste0("var_type_", var),
          label = var,
          choices = c("Continuous", "Nominal", "Ordinal"),
          selected = current_type,
          choicesOpt = list(content = my_icons)
      )
    })
    do.call(tagList, var_type_inputs)
  })
  
  # # Observe event for save_var_config button in modal
  # observeEvent(input$save_var_config, {
  #   # Code to handle the saving of variable configurations
  #   
  #   # For demonstration, print the selected types to the console
  #   combined_vars <- c(input$covariates, input$exact_match)
  #   selected_types <- sapply(combined_vars, function(var) {
  #     input[[paste0("var_type_", var)]]
  #   })
  #   print(selected_types)
  #   
  #   # Close the modal after saving
  #   removeModal()
  # })
  # 
  observeEvent(input$save_var_config, {
    
    # Save the selections
    current_covariate_selections <- input$covariates
    current_exact_match_selections <- input$exact_match
    
    # Gather the new types selected by the user
    combined_vars <- c(input$covariates, input$exact_match)
    new_types <- sapply(combined_vars, function(var) {
      input[[paste0("var_type_", var)]]
    }, USE.NAMES = TRUE)
    
    # head(df_unmatched())
    # Call the function to update the variable types in df_matched_data
    updated_df <- change_variable_types(df = df_unmatched(), var_names = combined_vars, 
                                        new_types = new_types)

    df_unmatched(updated_df) # we update the reactiveVal
    
    # Restore the selected variables in the selectInput boxes
    updateSelectInput(session, "covariates", selected = current_covariate_selections)
    updateSelectInput(session, "exact_match", selected = current_exact_match_selections)
    
    # Close the modal after saving
    removeModal()
  })

}

# # Run the application 
# shinyApp(ui = ui, server = server)