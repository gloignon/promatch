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
      .balance-plot {
        /* Flex item style for individual plots */
        margin-right: 10px; /* Adjust the spacing between plots */
        flex: 0 0 auto; /* Do not grow or shrink */
      }
      .balance-plot {
        /* Flex item style for individual plots */
        margin-right: 10px; /* Adjust the spacing between plots */
        flex: 0 0 auto; /* Do not grow or shrink */
      }
      .plots-container {
        /* Flex container style */
        display: flex;
        flex-wrap: wrap;
        align-items: flex-start; /* Align items to the start of the cross axis */
        justify-content: flex-start; /* Align items to the start of the main axis */
      }
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
                       choices = c("nearest", "quick", "optimal", "full", "exact", "NULL"),
                       selectize = FALSE
                     ),
                     selectInput(
                       "distance",
                       label = i18n$t("Distance Measure"),
                       choices = c("glm", "lasso", "gam", "mahalanobis", "robust_mahalanobis"),
                       selectize = FALSE
                     ),
                     # Extra controls
                     uiOutput("caliper_checkbox"),
                     uiOutput("dynamicSlider"),
                     
                     # Match button
                     actionButton("match", label = i18n$t("Match"))  # *THE* button
                   ),  # fin sidebar panel
                   mainPanel(width=6,
                             br(),
                             downloadButton("download", label = i18n$t("Download Matched data")),
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
        ## Panel Diagnostic ----
        tabPanel(i18n$t("Balance check"), 
                 id = "diagnostic",
                 fluidRow(
                   column(
                     width = 12, 
                     div(class = "plots-container",  # Apply the container style
                     uiOutput("diagnosticPlots"))
                   )
                 )
        ),  # fin vérif
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