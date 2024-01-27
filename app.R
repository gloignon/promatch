# promatch shiny app
# 
# Guillaume Loignon
# 
# Dec 2023
# loignon.guillaume@uqam.ca
# 
# Mises à jour:
# - 2024-01-05  
#   ajout traduction fr / en
#   ajout exact matching parameter
# - 2023-01-07
#   ajout fichier about / aide et son affichage bilingue
#   déménagement vers github
#   largeur d'affichage fixée à max 950 px
# - 2023-01-08
#   ajout fichiers .zip
#   fix: les variables exactes étaient ignorées
# - 2023-01-22
#   ajout onglet avancé
# - 2023-01-24
#   fix onglet avancé
#   correction affichage onglet matching
# - 2023-01-25
#   Ajout traduction, documentation
#   Ajout B-M test

library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)

library(shiny.i18n)

# the main R functions are in MatchIt
library(MatchIt)

# for opening xlsx files
library(readxl)

# explicit library calls so that the server can use the more advanced methods and distance measurements
library(optmatch)
# library(randomForest)
# library(rpart)
library(mgcv)  # for gam
library(glmnet)  # for lasso

library(brunnermunzel)  # for BM tests

i18n <- Translator$new(translation_json_path = 'translation.json')
# i18n$set_translation_language('fr')

# Define a function to create a unique temporary directory for each session
makeSessionTempDir <- function() {
  temp_dir <- tempdir()
  session_dir <- file.path(temp_dir, paste0("session_", Sys.getpid()))
  if (!dir.exists(session_dir)) {
    dir.create(session_dir)
  }
  return(session_dir)
}

# Transform df_matched_data to long format
pivot_matched_data <- function(df_matched_data, cols_to_pivot) {
  df <- df_matched_data()
  validate(
    need(!is.null(cols_to_pivot) && length(cols_to_pivot) > 0, "No columns selected for pivoting."),
    # need(all(cols_to_pivot %in% colnames(df_matched_data)), "One or more selected columns do not exist in the data frame.")
  )
  
  # convert selected columns to numeric
  df <- df %>%
    mutate(across(all_of(cols_to_pivot), as.numeric))
  
  # pivot
  df_long_data <- df %>%
    pivot_longer(cols = all_of(cols_to_pivot), names_to = "variable", values_to = "score")
  
  return(df_long_data)
}

brunner_munzel_test <- function(data, formula, conf.level = .05, 
                                alternative = "two.sided", est = "original") {
  
  bm_clean <- function(data, formula, conf.level, alternative, 
                       est) {
    # Run the test once
    test_result <-
      brunnermunzel::brunnermunzel.test(
        formula = formula,
        data = data,
        alternative = alternative,
        alpha = conf.level,
        est = est
      )
    
    # Unpack the results into separate columns
    data %>%
      summarise(
        df = test_result$parameter,
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        estimate = test_result$estimate,
        ci_low = test_result$conf.int[1],
        ci_high = test_result$conf.int[2]
      )
  }
  data %>%
    group_modify( ~ bm_clean(.x, formula, conf.level, alternative, est))
}

# Define UI ----
ui <- fluidPage(title = "promatch",

  tags$head(tags$style(
    HTML(
      "
      .info-icon {
        cursor: pointer;
        color: #007bff; /* Bootstrap link color */
        text-decoration: underline; /* Make it look like a clickable link */
        margin-left: 5px; /* Space between the fileInput and the info text */
      }
      .info-icon:hover {
        color: #0056b3; /* Darker shade when hovering, like a link */
      }
      .container-fluid {  max-width: 950px; }
      #file1-label {display:none;}
      "
    )
  )),
  
  useShinyjs(),    # Use shinyjs to enable JavaScript functions
  shiny.i18n::usei18n(i18n),  # for translation
  
  tags$div(
    style = 'float: right;',
    selectInput(
      inputId = 'selected_language', 
      width = "150px",
      selectize = FALSE,
      label = i18n$t('Changer la langue'),
      choices = i18n$get_languages(),
      # selected = i18n$get_key_translation()
      selected = "fr"
    )
  ),
  
  titlePanel("promatch"),
  ## Panel Match ----
  tabsetPanel(
    tabPanel(i18n$t("Match"),
      sidebarLayout(
        sidebarPanel(
          # fluidRow(column(
            width = 6,
            tags$p(tags$b(i18n$t("Choose CSV or Excel (.xlsx) File")), 
                   tags$span(id = "info_file", class = "info-icon",
                    onclick = "Shiny.setInputValue('info_click', 'info_file', {priority: 'event'});",
                    icon("info-circle")), 
                   style = "margin-bottom: 5px;"
              ),
            fileInput(
              "file1", 
              placeholder = "",
              buttonLabel = i18n$t("Browse"),
              # label = i18n$t(""),
              label = "file1",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                ".xlsx",
                ".zip"),
            ),

          uiOutput("varSelect"),
          # Dynamic UI for selecting variables
          selectInput(
            "method",
            label = i18n$t("Matching Method"),
            choices = c("nearest", "quick", "optimal", "full", "exact"),
            selectize = FALSE
          ),
          selectInput(
            "distance",
            label = i18n$t("Distance Measure"),
            choices = c("glm", "lasso", "gam"),
            selectize = FALSE
          ),
          # Extra controls
          checkboxInput("showSlider", "Use caliper (matching tolerance, in stand. dev.)", value = FALSE),
          
          uiOutput("dynamicSlider"),
          # Match button
          actionButton("match", label = i18n$t("Match"))  # *THE* button
        ),  # fin sidebar panel
      mainPanel(width=6,
        br(),
        downloadButton("download", label = i18n$t("Download Matched CSV")),
        br(),
        verbatimTextOutput("matchSummary")
      ),  # fin main panel onglet matching
      )  # fin sidebar layout
    ),     # fin onglet matching
    ## Panel Advanced -----
    tabPanel(i18n$t("Advanced"),
             sidebarLayout(
               sidebarPanel(width = 6,
                 # Use multiInput for advanced variable selection
                 multiInput(
                   inputId = "advancedVars", 
                   autocomplete = TRUE, 
                   label = i18n$t("Select measurement variable(s)"),
                   choices = "" # Initial choices, updated via server
                   # options = list(
                   #   non_selected_header = i18n$t("Available variables:"),
                   #   selected_header = i18n$t("Selected variables:")
                   # ) # Enable live search and actions box
                 ),
                 br(),
                 downloadButton("downloadLongFormat", i18n$t("Download Long Format Matched Data")),
                 br(),
                 h4("Repeated tests"),
                 selectInput(
                   "repeated_tests",
                   label = i18n$t("Select a test"),
                   choices = c("T-test", "Proportions", "Brunner-Munzel"), 
                   selectize = FALSE
                 ),
                 actionButton(inputId = "run_test", label = i18n$t("Run comparisons"))
              ),
              mainPanel(width = 6,
                verbatimTextOutput("repeated_tests_output")
              )
             )
    ),  # fin Avancé
    ## Panel help ----
    tabPanel(i18n$t("Help / About"),
             fluidRow(
               column(
                 width = 8,
                 offset = 2,
                 uiOutput("about_content")
                 
               )
             )
    )  # fin Help
    )  # fin onglets 
)  # fin UI

# Define server logic ----
server <- function(input, output, session) {

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
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)
  })

  
  ## file upload ----
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    
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
  
  # This reactive expression will hold the matched data for download
  df_matched_data <- reactiveVal()  # Initialize a reactive value
  
  # Define a reactive expression that checks if the matched data is available and non-empty
  data_available <- reactive({
    # Check if df_matched_data is non-null and has more than 0 rows
    !is.null(df_matched_data()) && nrow(df_matched_data()) > 0
  })
  
  # Update UI based on uploaded file
  output$varSelect <- renderUI({
    df <- data()
    if (is.data.frame(df)) {
      list(
        selectInput("depVar", label = i18n$t("Select Treatment (Group) Variable"), choices = names(df)),
        selectInput("covariates", label = i18n$t("Select Covariates"), choices = names(df), multiple = TRUE, selectize = TRUE),
        selectInput("exact_match", label = i18n$t("Select Exact matching variables (optional)"), choices = names(df), multiple = TRUE, selectize = TRUE)
      )
    }
  })
  
  ## Caliper slider toggle ----
  # Dynamic UI for the slider
  output$dynamicSlider <- renderUI({
    # Check if the checkbox is selected
    if(input$showSlider) {
      # Return the slider UI
      sliderInput("caliper", "Caliper",
                  min = 0.05, max = 1, value = 0.2, step = 0.05)
    }
  })
  
  ## Matching ----
  observeEvent(input$match, {
    req(input$depVar, input$covariates)
    df <- data()
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
    
    # Show a modal dialog indicating that matching is in progress
    showModal(modalDialog(
      title = i18n$t("Matching in Progress"),
      i18n$t("Please wait, the matching process is running..."),
      easyClose = FALSE,  # Prevent the user from closing the modal, which could be confusing
      footer = NULL
    ))
    
    if (input$showSlider == FALSE | is.null(input$caliper)) {
      caliper <- NULL
    } else {
      caliper <- input$caliper
    }
    
    # Proceed with the matching if there are enough rows left
    if (new_row_count > 0) {
      tryCatch({
        # Perform the matching using the matchit function
        formula <- as.formula(paste(input$depVar, "~", paste(input$covariates, collapse = " + ")))
        
        # Prepare the exact match variable, if needed
        exact_match_vars <- NULL
        if (!is.null(input$exact_match) && length(input$exact_match) > 0) {
          # Split the string by '+' and trim whitespaces
          exact_match_vars <- unlist(strsplit(input$exact_match, "\\s*\\+\\s*"))
          # Remove any leading or trailing whitespace
          exact_match_vars <- trimws(exact_match_vars)
          print(paste0("About to do an exact match on ", paste(exact_match_vars, collapse = ", ")))
        }
        
        # Perform the matching using the matchit function
        m.out <- matchit(
          formula, 
          method = input$method, 
          distance = input$distance, 
          data = df_clean, 
          exact = exact_match_vars,
          caliper = caliper, 
          std.caliper = TRUE
        )
        
        # Extract the matched data
        matched <- match.data(object = m.out, data = df_clean)
        #TODO: user should be able to choose if match.data will output the distance or the prop.score
        
        # Update the reactive value with the matched data
        df_matched_data(matched)  # This line updates the reactive value
        
        # Store and define the summary
        output_summary(summary(m.out))
        output$matchSummary <- renderPrint({
          req(output_summary())  # Require that output_summary is not NULL
          output_summary()       # Output the stored summary
        })
        
        # Remove the loading message and show the "Done!" message
        removeModal()
        showModal(modalDialog(
          title = i18n$t("Success"),
          i18n$t("Done! The matching is complete. You can now download the matched data."),
          easyClose = TRUE
        ))
      }, error = function(e) {
        # Capture the call that caused the error
        error_call <- conditionCall(e)
        if (!is.null(error_call)) {
          # Convert the call to a character string for printing
          call_msg <- paste(i18n$t("Error in"), deparse(error_call)[1])
        } else {
          call_msg <- ""
        }
        
        # Show a more informative error message
        showModal(modalDialog(
          title = i18n$t("Error in matchit"),
          paste(i18n$t("Error: "), e$message, call_msg),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    } else {
      showModal(modalDialog(
        title = i18n$t("Error"),
        i18n$t("Error: Not enough data to perform matching after removing rows with 'NA' values."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
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
  
  ## Main download handler -----
  output$download <- downloadHandler(
    filename = function() {
      paste("matched-", input$method, "-", input$distance, "-", Sys.Date(), ".csv", sep = "")
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
        # Write the data to CSV
        write.csv(df_matched_data(), file, row.names = TRUE)
      }
    }
  )
  
  # Long format download handler ----
  # output$downloadLongFormat <- downloadHandler(
  #   filename = function() {
  #     paste0("matched-long_format-", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     if (!data_available() || is.null(df_matched_data()) || nrow(df_matched_data()) == 0) {
  #       showModal(modalDialog(
  #         title = i18n$t("Error"),
  #         i18n$t("No matched data available for download."),
  #         easyClose = TRUE
  #       ))
  #     } else {
  #       # Write the data to CSV
  #       # req(df_matched_data())  # Ensure df_matched_data is available
  #       
  #       # Get the columns to pivot
  #       cols_to_pivot <- input$advancedVars
  #       req(length(cols_to_pivot) > 0)  # Ensure there's at least one column selected
  #       
  #       df_long_data <- pivot_matched_data(df_matched_data, cols_to_pivot)
  #       
  #       # Write the long format data to the file
  #       write.csv(df_long_data, file, row.names = FALSE)      
  #       }
  # 
  #   }
  # )
  
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
}

# Run the application 
shinyApp(ui = ui, server = server)