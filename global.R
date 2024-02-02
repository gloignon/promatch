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
# - 2023-01-27
#   Fichier long converti en zip avant le téléchargement
#   Ajout caliper
#   Ajout method quick
# - 2023-01-29
#   Conversion en trois fichiers, server.R, ui.R, global.R
# - 2023-01-31
#   Ajout matching NULL
#   caliper disparait quand méthode ou distance incompatible avec caliper
#   filtrage des variables quand par 2 niveaux (ou 2 valeurs uniques)
#   nettoyage du fichier lorsque téléversé
# - 2023-02-01
#   le téléchargement principal va être un zip si le fichier est volumineux
#   (le long format est toujours un zip)
#   Ajout d'un onglet diagnostic rudimentaire

library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)

library(shiny.i18n)

# the main R functions are in MatchIt
library(MatchIt)

# for opening xlsx files
library(readxl)

library(janitor)

# explicit library calls so that the server can use the more advanced methods and distance measurements
library(optmatch)
# library(randomForest)
# library(rpart)
library(mgcv)  # for gam
library(glmnet)  # for lasso
library(quickmatch)  # for quick

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

filterDichotomousVariables <- function(df) {
  eligibleVars <- sapply(df, function(x) {
    x <- x[!is.na(x)]
    is_eligible <- length(unique(x)) == 2
    
    return(is_eligible)
  })
  
  # Return names of eligible variables
  return(names(df)[eligibleVars])
}

cleanUploadedFile <- function(df) {
  df <- janitor::remove_constant(df)
  df <- janitor::clean_names(df, case = "none")
  return(df)
}

# Function to check if a variable is continuous
is_continuous <- function(df, var) {
  is.numeric(df[[var]]) || is.double(df[[var]])
}

# Function to translate R's variable types to common types
translate_var_type <- function(var, data) {
  # print(head(data[[var]]))
  # print(typeof(data[[var]]))
  if (is.factor(data[[var]])) {
    levels <- nlevels(data[[var]])
    if (levels > 2) {
      return("Nominal")
    } else {
      return("Nominal")  # Assuming binary factors are ordinal; adjust as needed
    }
  } else if (is.numeric(data[[var]])) {
    return("Continuous")
  } else if (is.integer(data[[var]])) {
    return("Continuous")  # Assuming integers are ordinal; adjust as needed
  } else if (is.character(data[[var]])) {
    return("Nominal")
  } else {
    # print(paste("oops,", var, "is type other"))
    return("Other")
  }
}

# will change var types when the specified new type
# does not match the curren type
change_variable_types <- function(df, var_names, new_types) {
  # Loop through the variable names
  for (var in var_names) {
    new_type <- new_types[var]
    current_type <- translate_var_type(var = var, data = df)
    # print(paste("Var:", var, "Current type:", current_type, "New type:", new_type, "Head:", head(df[[var]])))
    
    # Check if the new type is different from the current type
    if (new_type != current_type) {
      # Apply the type change based on the new type
      if (new_type == "Continuous") {
        df[[var]] <- as.numeric(df[[var]])
      } else if (new_type == "Nominal") {
        # print("changing to nominal!")
        df[[var]] <- as.factor(df[[var]])
      } else if (new_type == "Ordinal") {
        # For ordinal, ensure the variable is a factor first
        df[[var]] <- factor(df[[var]])
        # Then set the class to "ordered"
        df[[var]] <- ordered(df[[var]])
      }
    }
  }
  return(df)
}
