server <- function(input, output, session) {
  
  ## Constants ----
  DOWNLOAD_SIZE_LIMIT <- 5  # max size in megs, above that the matched data will be downloaded as a .zip 
  
  # methods that don't support caliper
  v_no_caliper_methods <- c("quick", "optimal", "exact", "NULL")
  
  # distance that don't support caliper
  v_no_caliper_distances <- c("robust_mahalanobis")
  
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
  
  # Reactive values ----
  modalContent <- reactiveVal("")  # for info boxes
  output_summary <- reactiveVal()    # This will hold the summary of m.out
  tests_summary <- reactiveVal()   # This will hold the summary of the tests
  match_object <- reactiveVal()  #   # this will hold m.out, the matching object
  df_matched_data <- reactiveVal()  # will hold the matched data
  df_unmatched_data <- reactiveVal()  # hold the "raw" unmatched data after upload
  df_clean_unmatched <- reactiveVal()  # holds the cleaned unmatched data 
  availableVarsOnMatch <- reactiveVal()  #     Reactive value for storing available variables
  adv_results_viewed <- reactiveVal(FALSE)  # will toggle to TRUE if adv tab results have been shown
  matching_updated <- reactiveVal(FALSE)       # will toggle to TRUE if matching has been updated    


  # Observers ----
  
  # # Update the availableVarsOnMatch reactive value
  # # Observe changes in data availability
  # observe({
  #   # When data becomes available
  #   if (data_available()) {
  #     # Get all variables
  #     allVars <- names(df_matched_data())
  #     
  #     # Exclude selected variables in depVar, covariates, and exact_match
  #     selectedVars <- unique(c(input$depVar, input$covariates, input$exact_match))
  #     availableVars <- setdiff(allVars, selectedVars)
  #     
  #     # for now, this module only does dichotomous variables, so we'll limit the choice again
  #     dichoVars <- filterDichotomousVariables(df_matched_data())
  #     availableVars <- intersect(availableVars, dichoVars)  # available AND dichotomous
  #     
  #     # Update the multiInput choices
  #     updateMultiInput(
  #       session = session, 
  #       inputId = "advancedVars",
  #       choices = availableVars
  #     )
  #   }
  # })
  
  
  # Observe changes in availableVarsOnMatch
  observe({
    updateMultiInput(
      session = session, 
      inputId = "advancedVars",
      choices = availableVarsOnMatch()
    )
  })
  
  # language selector 
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    # print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)
  })

  
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
    df_unmatched_data(df)
  })
  
  # toggles in the advanced tab
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
    
    if (length(input$advancedVars) > 1) {
      # Enable download button when at least one variable is selected
      shinyjs::enable("check_mixed_model")
      shinyjs::removeClass(id = "check_mixed_model_container", class = "disabled-checkbox")
      # print("enabling mixed model checkbox")
    } else {
      # Disable download button when no variable is selected
      shinyjs::disable("check_mixed_model")
      shinyjs::addClass(id = "check_mixed_model_container", class = "disabled-checkbox")
      # print("enabling mixed model checkbox")
    }
      
  })

  
  # Define a reactive expression that checks if the matched data is available and non-empty
  data_available <- reactive({
    # Check if df_matched_data is non-null and has more than 0 rows
    !is.null(df_matched_data()) && nrow(df_matched_data()) > 0
  })
  
  # Update UI based on uploaded file
  output$varSelect <- renderUI({
    req(df_unmatched_data())
    df <- df_unmatched_data()
    req(is.data.frame(df))
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
      actionButton(inputId = "btn_var_config", icon = icon("gear"), label = i18n$t("Variable properties"))
    )
  })
  
  ## Caliper checkbox and slider toggles ----
  output$caliper_checkbox <- renderUI({
    # Check if the method and distance allow for caliper
    if (!input$method %in% v_no_caliper_methods && !input$distance %in% v_no_caliper_distances) {
      # Return the checkbox
      checkboxInput("showSlider", i18n$t("Use caliper (matching tolerance)"), value = input$showSlider)
    }

  })
  
  output$dynamicSlider <- renderUI({
    if (!is.null(input$showSlider) && input$showSlider && 
        !is.null(input$method) && !input$method %in% v_no_caliper_methods &&
        !is.null(input$distance) && !input$distance %in% v_no_caliper_distances) {
      
      # Use a default value for 'caliper' if it's NULL or uninitialized
      caliperValue <- ifelse(is.null(input$caliper), 0.2, input$caliper)
      
      sliderInput("caliper", i18n$t("Caliper"), min = 0.05, max = 1, value = caliperValue, step = 0.05)
    }
  })
  
  ## Matching ----
  observeEvent(input$match, {
    req(input$depVar, input$covariates)
    df <- df_unmatched_data()
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
    

    ## the actual matching ----
    # display a progress bar while matching
    withProgress(message = i18n$t("Preparing matching..."), value = 0, {
        # Perform the matching using the matchit function
        formula <- as.formula(paste(input$depVar, "~", paste(input$covariates, collapse = " + ")))
        
        # prepare the caliper var
        if (input$showSlider == FALSE | is.null(input$caliper) |
            input$method %in% v_no_caliper_methods | input$distance %in% v_no_caliper_distances) {
          caliper <- NULL
        } else {
          caliper <- input$caliper
        }
        
        # prepare the distance var
        distance <- input$distance
        
        # prepare the mahalanobis formula
        # this will allow to have both a prop score caliper and mahalanobis distance
        if (!is.null(caliper) & input$distance == "mahalanobis") {
          # format as a formula
          mah_vars <- paste(input$covariates, collapse = "+")
          mah_vars <- as.formula(paste("~", mah_vars))
          distance <- "glm"
        } else {
          mah_vars <- NULL
        }
        
        # prepare the method var
        method <- NULL
        if (input$method != "NULL") {
          method <- input$method
        }
        
        # Prepare the exact match variable, if needed
        exact_match_vars <- NULL
        if (!is.null(input$exact_match) && length(input$exact_match) > 0) {
         exact_match_vars <- paste(input$exact_match, collapse = " + ")
         exact_match_vars <- as.formula(paste("~", exact_match_vars))
        }
        
        # update progress bar
        setProgress(0.2, message = i18n$t("Matching..."))
        
        # before running the matching, print the parameters to console
        print(paste("Matching parameters:"))
        print(formula)
        print("exact match:")
        print(exact_match_vars)
        print(paste("distance: ", distance))
        print(paste("method: ", method))
        print(paste("caliper: ", caliper))
        print(paste("mahalanobis variables: ", mah_vars))

 
        # Perform the matching using the matchit function
        m.out <- try(
        matchit(
            formula, 
            method = method, 
            distance = distance, 
            data = df_clean, 
            exact = exact_match_vars,
            caliper = caliper,
            link = "linear.logit", # will apply the caliper to the logit
            std.caliper = TRUE,
            mahvars = mah_vars,
            verbose = FALSE
          )
        )
        
        if (inherits(m.out, "try-error")) {
          showModal(modalDialog(
            title = i18n$t("Error"),
            i18n$t("Error while matching.")
          ))
          return()
        }
        
        print(m.out)
        match_object(m.out)  # copy the matched object to the reactive value; used for model assessment
        
        # update progress bar
        setProgress(.4, message = i18n$t("Extracting matched data..."))
        
        # Extract the matched data
        matched <- match.data(object = m.out, data = df_clean)
        #TODO: user should be able to choose if match.data will output the distance or the prop.score
        
        # Update the reactive values 
        df_matched_data(matched)  # matched data
        df_clean_unmatched(df_clean)  # cleaned unmatched data
        
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
        list_summary <- summary(m.out)
        
        # remove call element from list_summary
        list_summary$call <- NULL
        
        # reconstruct the summary without the call object
        summary_text <- capture.output(print(list_summary))
        summary_text <- paste(summary_text, collapse = "\n")
        
        # produce a summary of the matching, each element being only added if non-NULL
        param_summary <- paste(
          "Matching parameters:",
          add_to_summary("treatment: ", input$depVar),
          add_to_summary("covariates: ", paste(input$covariates, collapse = " + ")),
          add_to_summary("exact match: ", paste(input$exact_match, collapse = " + ")),
          add_to_summary("distance: ", distance),
          add_to_summary("method: ", method),
          add_to_summary("caliper: ", caliper),
          add_to_summary("mahalanobis variables: ", paste(mah_vars, collapse = " + ")),
          sep = ""
        )
      
        # add the summary at the end of output_summary
        output_summary(paste(
          output_summary(), "\n\n",
          param_summary, "\n",
          summary_text
          ))

        output$matchSummary <- renderText({
          req(output_summary())  # Require that output_summary is not NULL
          output_summary()       # Output the stored summary
        })
        
        # final progress update
        setProgress(1, message = i18n$t("Matching complete. Matched data available for download."))
        
        matching_updated(TRUE)  # Update the flag to indicate new matching is available
        # adv_results_viewed(FALSE)  # Reset stats viewed status since it's impossible we saw advanced results for the new matching

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
    filePath <- if (lang == "fr") "www/apropros.html" else "www/about.html"
    # print(paste("Attempting to load file:", filePath))
    # print(paste("File exists:", file.exists(filePath)))
    includeHTML(filePath)
  })
  
  # Dx plots - love plots ----
  # Render the UI for the plot
  output$love_plots <- renderUI({
    # Only create the plotOutput if match_obj is not NULL
    req(match_object())
    
    # estimate required height based on number of covariates
    # and of levels in the covariates
    est_heights <- estimate_lp_height(match_object(), baseline = 200, mult = 30)
    
    tagList(
      h3(i18n$t("Balance analysis (Love plots)")),
      withSpinner(plotOutput("lovePlot_p1", height = est_heights$top)),
      withSpinner(plotOutput("lovePlot_p2", height = est_heights$bottom))
    )
  })
  
  # Generate the love plots
  output$lovePlot_p1 <- renderPlot({
    req(match_object())  # Ensure that match_obj is not NULL
    
    # if a caliper was specified, used that, otherwise use 0.2
    caliper <- ifelse(is.null(input$caliper), 0.2, input$caliper)
    
    p1 <- cobalt::love.plot(match_object(),
                      binary = "std",
                      stats = "m",
                      abs = TRUE,
                      drop.distance = TRUE, 
                      title = "", 
                      colors = c("blue", "red"), alpha = .8,
                      sample.names = c(i18n$t("Unmatched"), i18n$t("Matched")),
                      thresholds = caliper  # Set your threshold for standardized mean differences
                      ) + 
      labs(x = i18n$t("Absolute standardized mean differences")) + 
      ggpubr::theme_pubclean(base_size = 14) +
      theme(legend.title = element_blank(),
            plot.margin = unit(c(1, 1, 1, 1), "lines")
      )
    
    return(p1)
    
  })
  
  output$lovePlot_p2 <- renderPlot({
    req(match_object())  # Ensure that match_obj is not NULL
    
    # if a caliper was specified, used that, otherwise use 0.2
    caliper <- ifelse(is.null(input$caliper), 0.2, input$caliper)
    
    p2 <- cobalt::love.plot(match_object(),
                            binary = "std",
                            stats = "variance.ratio",
                            drop.distance = TRUE, 
                            sample.names = c(i18n$t("Unmatched"), i18n$t("Matched")),
                            title = "",
                            colors = c("blue", "red"), alpha = .8
    )  + 
      labs(x = i18n$t("Variance ratios")) + 
      ggpubr::theme_pubclean(base_size = 16) +
      theme(legend.title = element_blank())
  
    return(p2)
    
  })
  
  
  
#   # Dx plots (by var) ----
#   # Create plot tag list
  
  output$diagnosticPlots <- renderUI({
    df <- df_matched_data()
    # Check if the data exists and has the necessary rows
    if (is.null(df) || nrow(df) == 0) {
      # Return a message indicating no data is available
      return(p(i18n$t("Please execute matching first."), align = "center"))
    }
  
    req(df_matched_data(), df_clean_unmatched(), 
        input$covariates, input$depVar)
    valid_covariates <- c(input$covariates, input$exact_match) 
    
    # print(valid_covariates)
    
    req(length(valid_covariates) > 0)
    
    # create div with plot
    plot_output_list <- lapply(seq_along(valid_covariates), function(i) {
      plotname <- paste("plot", i, sep = "_")
      div(
        h4(valid_covariates[i]), # Include variable name in h3 tag
        withSpinner(plotOutput(plotname)), # Wrap plotOutput with withSpinner
        class = "balance-plot"
      )
    })
    
    tagList(
      h3(i18n$t("Descriptive plots")),
      div(class = "plots-container", plot_output_list)
    )
  })
  
  observe({
    req(df_matched_data(), df_clean_unmatched(), input$covariates, input$depVar)
    df <- df_matched_data()
    df_raw <- df_clean_unmatched()
    valid_covariates <- c(input$covariates, input$exact_match)
    dep_var <- input$depVar
  
    lapply(seq_along(valid_covariates), function(i) {
      local({
        plot_id <- paste("plot", i, sep = "_")
        covariate <- valid_covariates[i]
        
        output[[plot_id]] <- renderPlot({
          if (is_continuous(df, covariate)) {
            # Continuous variable plot
            pr <-
              ggplot(df_raw, aes(x = .data[[covariate]], fill = .data[[dep_var]], color = .data[[dep_var]])) +
              geom_density(alpha = .5) +
              labs(title = i18n$t("Unmatched data"), x = "", y = "") +
              theme(legend.position = "top") +
              ggpubr::theme_pubclean(base_size = 12)
            pm <-
              ggplot(df, aes(x = .data[[covariate]], fill = .data[[dep_var]], color = .data[[dep_var]])) +
              geom_density(alpha = .5) +
              labs(title = i18n$t("Matched data"), x = "", y = "") +
              theme(legend.position = "none") +
              ggpubr::theme_pubclean(base_size = 12)
            
            # combine plots, with aligned x-axis 
            # cowplot::plot_grid(pr, pm, ncol = 1, labels = c("Unmatched", "Matched"), align = "v", vjust = -1)
            ggarrange(
              pr,
              pm,
              ncol = 1,
              legend = "top",
              common.legend = TRUE
            )
          } else {
            # Categorical variable plot (bar plot)
            hm <- ggplot(df, aes(x = .data[[covariate]], fill = .data[[dep_var]])) +
              geom_bar(position = "dodge") +
              labs(title = i18n$t("Matched data"), x = "", y = "", fill = dep_var) +
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x labels for better readability
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank() 
                    )
            
           hr <- ggplot(df_raw, aes(x = .data[[covariate]], fill = .data[[dep_var]])) +
              geom_bar(position = "dodge") +
              labs(title = i18n$t("Unmatched data"), x = "", y = "", fill = dep_var) +
              theme_minimal(base_size = 12) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x labels for better readability
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank() 
              )
           ggarrange(
             hr,
             hm,
             ncol = 1,
             legend = "top",
             common.legend = TRUE
           )
           
          }
        })
      })
    })
  })
  
  # Variable type config ----
  # Observe event for var_config button
  observeEvent(input$btn_var_config, {
    # Create a modal dialog when the button is clicked
    showModal(modalDialog(
      title = i18n$t("Variable Configuration"),
      
      # UI for selecting variable types
      h4(i18n$t("Select Variable Types")),
      uiOutput("var_type_ui"),
      
      # Modal footer with action buttons
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton("btn_save_var_config", i18n$t("Save Configuration"))
      )
    ))
  })
  
  # UI for variable type selection with current type display
  output$var_type_ui <- renderUI({
    req(df_unmatched_data())  # Ensure data is available
    df <- df_unmatched_data()  # Your actual dataset
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
  observeEvent(input$btn_save_var_config, {
    
    # Save the selections
    current_covariate_selections <- input$covariates
    current_exact_match_selections <- input$exact_match
    
    # Gather the new types selected by the user
    combined_vars <- c(input$covariates, input$exact_match)
    new_types <- sapply(combined_vars, function(var) {
      input[[paste0("var_type_", var)]]
    }, USE.NAMES = TRUE)
    
    # Call the function to update the variable types in df_matched_data
    updated_df <- change_variable_types(df = df_unmatched_data(), var_names = combined_vars, 
                                        new_types = new_types)

    df_unmatched_data(updated_df) # we update the reactiveVal
    
    # Restore the selected variables in the selectInput boxes
    updateSelectInput(session, "covariates", selected = current_covariate_selections)
    updateSelectInput(session, "exact_match", selected = current_exact_match_selections)
    
    # Close the modal after saving
    removeModal()
  })
  
  # Advanced tab stuff =================
  
  ## advanced selectors ----
  output$advanced_tab_selection <- renderUI({
    # if there is no matched data, display a message and exit
    if (is.null(df_matched_data()) || nrow(df_matched_data()) == 0) {
      return(p(i18n$t("Please execute matching first.")))
    }
    allVars <- names(df_matched_data())
    
    selectedVars <- unique(c(input$depVar, input$covariates, input$exact_match))
    availableVars <- setdiff(allVars, selectedVars)
    
    # for now, this module only does dichotomous variables, so we'll limit the choice again
    dichoVars <- filterDichotomousVariables(df_matched_data())
    availableVars <- intersect(availableVars, dichoVars)  # available AND dichotomous
    
    
    # combine the following elements in a list
    list(
      p(i18n$t("Note: this tab currently works only for dichotomous (binary) measurements, e.g. success/failure.")),
      # Use multiInput for advanced variable selection
      multiInput(
        inputId = "advancedVars", 
        autocomplete = TRUE, 
        label = i18n$t("Select measurement variable(s)"),
        choices = availableVars 
      ),
      
      br(),
      # h4(i18n$t("Statistical tests")),
      # selectInput(
      #   "repeated_tests",
      #   label = i18n$t("Select a comparison method"),
      #   choices = c("Proportions test", "Sensitivity analysis"), 
      #   selectize = FALSE
      # ),
      
      # let user select the max gamma value (from 1 to 6, by increments of 1)
      # and using a dynamic slider
      
      sliderInput("max_gamma", i18n$t("Max. gamma value for sensitivity analysis"), 
                  min = 1, max = 5, value = 2, step = 1),
      
      # selectInput("gamma_inc", i18n$t("Test by increments of..."), 
      #             choices = c(0.05, 0.1, 0.2), selected = .1),
      
      # add a checkbox to activate mixed model analysis
      
      
      # offer three choices of alpha threshold: 0.05, 0.01, 0.001
      selectInput("alpha_thres", i18n$t("Alpha level"), 
                  choices = c(0.1, 0.05, 0.01, 0.001), selected = 0.05),
      
      # # this will become TRUE iff at least 2 measurements were selected in the multiInput
      # flag_multiple_measurements <- reactive({
      #   length(input$advancedVars) > 1
      # }),
      # 
      # checkboxes to run mixed model analysis, should be displayed only if there are multiple measurements
      div(id = "check_mixed_model_container", class = "disabled-checkbox", 
          checkboxInput("check_mixed_model", i18n$t("Include mixed model analyses"), value = FALSE),
          ),
      actionButton(inputId = "run_test", label = i18n$t("Run tests")),
      br(),br(),
      downloadButton("downloadLongFormat", i18n$t("Download Long Format Matched Data"))
    )
  })
  
  ## Step 1:  pivot the data to long format ----
  reac_long_data <- eventReactive(input$run_test, {
    req(input$depVar, input$advancedVars, df_clean_unmatched(), df_matched_data())
    
    
    print("pivoting data...")
    
    df_unmatched <- df_clean_unmatched()
    df_matched <- df_matched_data()
    
    df_long_data_unmatched <- try(
      pivot_data(df_unmatched, input$advancedVars)
    )
    df_long_data_matched <- try(
      pivot_data(df_matched, input$advancedVars)
    )
    
    df_long_data_unmatched$.group <- df_long_data_unmatched[[input$depVar]]
    df_long_data_matched$.group <- df_long_data_matched[[input$depVar]]
    return(list(unmatched = df_long_data_unmatched,
                matched = df_long_data_matched))
  }, ignoreNULL = TRUE)
  
  ## Step 2: proportions tests ----
  observeEvent(reac_long_data(), {
    req(reac_long_data())
    withProgress(message = i18n$t("Running proportions tests..."), value = 0, {
      print("running prop tests...")
      
      list_long_data <- reac_long_data()
      
      df_long_data_unmatched <- list_long_data$unmatched
      df_long_data_matched <- list_long_data$matched
      unique_levels <- unique(df_long_data_matched$.group)
      
      result_props_tests_unmatched <- try(run_props_tests(df_long_data_unmatched, unique_levels))
      result_props_tests_matched <- try(run_props_tests(df_long_data_matched, unique_levels))
      
      output$prop_tests_results_ui <- renderUI({
        return(list(
          h3(i18n$t("Proportions tests")),
          h4(i18n$t("Unmatched data")),
          renderTable(result_props_tests_unmatched),
          h4(i18n$t("Matched data")),
          renderTable(result_props_tests_matched)
        ))
      })
  
      ## Step 3A: mixed model fit ----
      if (input$check_mixed_model) {
        print("fitting mixed models...")
        setProgress(0.2, message = i18n$t("Running requested mixed model analysis - null model..."))
        mm_null <-
          try(mm_analysis_null_model(df_long = df_long_data_matched))
        
        # if there was an error fitting the null model, break with error message, otherwise continue
        if (inherits(mm_null, "try-error")) {
          showModal(modalDialog(
            title = i18n$t("Error"),
            i18n$t("There was an error fitting the null model. Please check your data and try again.")
          ))
          return()
        }
        
        setProgress(0.3, message = i18n$t("Running requested mixed model analysis - uniform model..."))
        mm_uniform <-
          try(mm_analysis_uniform_model(df_long = df_long_data_matched, mm_null = mm_null))
        setProgress(0.5, message = i18n$t("Running requested mixed model analysis - non-uniform model..."))
        
        # if there was an error fitting the uniform model, break with error message, otherwise continue
        if (inherits(mm_uniform, "try-error")) {
          showModal(modalDialog(
            title = i18n$t("Error"),
            i18n$t("There was an error fitting the uniform model. Please check your data and try again.")
          ))
          return()
        }
        mm_nonuniform <-
          try(mm_analysis_nonuniform_model(df_long = df_long_data_matched, mm_uniform = mm_uniform))
        # if there was an error fitting the non-uniform model, break with error message, otherwise continue
        if (inherits(mm_nonuniform, "try-error")) {
          showModal(modalDialog(
            title = i18n$t("Error"),
            i18n$t("There was an error fitting the non-uniform model. Please check your data and try again.")
          ))
          return()
        }
  
        
        setProgress(0.7, message = i18n$t("Running requested mixed model analysis - model comparisons..."))
        # TODO: move everything I can to a function in the global.R file
        result_lr <-
          mm_analysis_comparison(
            mm_null,
            mm_uniform,
            mm_nonuniform
          ) %>% data.frame
        result_lr$model <- c("Null", "Uniform", "Non-uniform")
        result_lr <- result_lr %>% select(model, everything())
        colnames(result_lr) <- c("Model", "df", "AIC", "BIC", "logLik", "deviance", "Chisq", "Chisq_df", "p")
        anova_table <- sjPlot::tab_df(as.data.frame(result_lr))
        # use sjPlot tab_model to create a summary, with the names Null, Uniform and Non-uniform
        sj_table <- tab_model(mm_null, 
                              mm_uniform, 
                              mm_nonuniform,
                              dv.labels = c("Null", "Uniform (group)", "Non-uniform<br>(group x measurement)"),
                              show.intercept = FALSE,
                              show.p = TRUE,
                              show.ci = FALSE)
        
        output$mixed_model_results_ui <- renderUI({
          list(
            h3(i18n$t("Models summary")),
            # display the HTML is sj_table$knitr
            HTML(sj_table$knitr),
            h4(i18n$t("Likelihood ratio tests")),
            HTML(anova_table$knitr)
            # renderPrint({
            #   result_lr
            # })
          )
        })
      }
      
      ## Step 3B: sensitivity tests ----
      # run predictions if mixed model analysis requested and available
      setProgress(0.8, message = i18n$t("Running sensitivity analyses..."))
      
      if (input$check_mixed_model && !is.null(mm_nonuniform)) {
        # produire les prédictions du modèle
        print("running sensitivity analysis (mixed model)")
        df_matched_pred_long <- df_long_data_matched %>% 
          filter(!is.na(score)) %>% 
          mutate(pred = predict(mm_nonuniform, type = "response")) %>% 
          mutate(score = ifelse(pred >= .5, 1, 0))  # clip predictions to [0, 1]
        result_sens_analysis <-
          sensitivity_analysis(response = df_matched_pred_long$score,
                               group = df_matched_pred_long$.group,
                               items = df_matched_pred_long$measurement,
                               subclass = df_matched_pred_long$subclass,
                               # gamma_inc =  input$gamma_inc,
                               max_gamma = input$max_gamma)
      } else {
        print("running sensitivity analysis (raw)")
        result_sens_analysis <-
          sensitivity_analysis(response = df_long_data_matched$score,
                               group = df_long_data_matched$.group,
                               items = df_long_data_matched$measurement,
                               subclass = df_long_data_matched$subclass,
                               # gamma_inc =  input$gamma_inc,
                               max_gamma = input$max_gamma)
      }
    
      annotations_sens_analysis <-
        make_sens_annotations(result_sens_analysis, alpha = input$alpha_thres) %>% 
        data.frame()
      
      # 
      # list(
      #   long_results = result_sens_analysis,
      #   annotations = annotations_sens_analysis
      # )
      
      output$sens_tests_results_ui <- renderUI({
        list(
          h3(i18n$t("Sensitivity analysis")),
          renderTable({
            annotations_sens_analysis
          }),
          div(class = "pre-overflow", 
              renderPrint({
                result_sens_analysis
              }))
          )
      })
    
    })  # fin withProgress
    adv_results_viewed(TRUE)
    matching_updated(FALSE)  

  })

  
  # Detect when user navigates to the advanced stats tab
  # Will warn the user about outdated results
  observeEvent(input$main_tabs, {
    # print("input tabsssss!")
    # print(input$main_tabs)
    # print(matching_updated())
    if (input$main_tabs == "advanced" & matching_updated() == TRUE & adv_results_viewed() == TRUE) {
      showModal(modalDialog(
        title = i18n$t("New data available"),
        i18n$t("New matched data is available. The results displayed here are not current, please update as needed.")
      ))
    }
  })
  
}