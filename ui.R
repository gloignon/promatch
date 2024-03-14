# CSS ----
ui <- fluidPage(title = "promatch",
                tags$script(src = "https://kit.fontawesome.com/<you>.js"),
                
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
      .container-fluid {  max-width: 1200px; }
      #file1-label {display:none;}
      
      /* make sure the plots occupy at most half the horizontal space, otherwise all the space */ 
      .balance-plot {
        /* Flex item style for individual plots */
        margin-right: 10px; /* Adjust the spacing between plots */
        flex: 1 1 500px; /* Flex-grow, flex-shrink, flex-basis */ 
        min-width: 400px; /* Minimum width of each plot */
        max-width: 600px;
        box-sizing: border-box; /* Make sure margins and paddings are included in the width calculation */
      
      }
      .plots-container {
        /* Flex container style */
        display: flex;
        flex-wrap: wrap;
        align-items: flex-start; /* Align items to the start of the cross axis */
        justify-content: flex-start; /* Align items to the start of the main axis */
        gap: 10px; /* Adds space between plots */
      }
      
      #test_plots img{
       min-width: 150px;
      }
      
      .pre-overflow pre{overflow-y: scroll; max-height: 300px;}

      hr {border: .5px solid #ddd; !important;}
      
      .disabled-checkbox label { color: #aaa; } /* Light grey color */
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
          selected = i18n$get_key_translation()
        )
      ),

# Panels ----
      titlePanel("promatch"),
      ## Panel Match ----
      tabsetPanel(id = "main_tabs",
        tabPanel(i18n$t("Match"), 
                 sidebarLayout(
                   sidebarPanel(
                     # fluidRow(column(
                     width = 6,
                     tags$p(tags$b(i18n$t("Choose CSV or Excel (.xlsx) File")), 
                            tags$span(id = "info_file", class = "info-icon",
                                      onclick = "Shiny.setInputValue('info_click', 'info_file', {priority: 'event'});",
                                      icon("info-circle"))
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
                     # Variable selectors
                     uiOutput("varSelect"),
                     #hr(),
                     # Method and distance selectors
                     selectInput(
                       "method",
                       label = i18n$t("Matching Method"),
                       choices = c("nearest", "quick", "optimal", "full", "exact", "NULL"),
                       selectize = FALSE
                     ),
                     selectInput(
                       "distance",
                       label = i18n$t("Distance Measure"),
                       choices = c("glm", "lasso", "mahalanobis", "robust_mahalanobis"),
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
                             br(), br(),
                             verbatimTextOutput("matchSummary")
                   ),  # fin main panel onglet matching
                 )  # fin sidebar layout
        ),     # fin onglet matching
        ## Panel Diagnostic ----
        tabPanel(i18n$t("Balance check"), 
                 value = "diagnostic",
                 fluidRow(
                   column(
                     width = 12, 
                     div(class = "plots-container",  # Apply the container style
                         uiOutput("diagnosticPlots")
                     ),
                     # uiOutput("bal_plot"),
                     uiOutput("love_plot")
                   )
                 )
        ),  # fin dx
        ## Panel Advanced -----
        tabPanel(i18n$t("Advanced"), value = "advanced",
                 sidebarLayout(
                   sidebarPanel(width = 5,
                                uiOutput("advanced_tab_selection")
                   ),
                   mainPanel(width = 7,
                       uiOutput("prop_tests_results_ui"),
                       # uiOutput("mixed_model_results_null_ui"),
                       # uiOutput("mixed_model_results_uniform_ui"),
                       # uiOutput("mixed_model_results_nonuniform_ui"),
                       uiOutput("mixed_model_results_ui"),
                       uiOutput("sens_tests_results_ui"),
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