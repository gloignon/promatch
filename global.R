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
# - 2024-01-07
#   ajout fichier about / aide et son affichage bilingue
#   déménagement vers github
#   largeur d'affichage fixée à max 950 px
# - 2024-01-08
#   ajout fichiers .zip
#   fix: les variables exactes étaient ignorées
# - 2023-01-22
#   ajout onglet avancé
# - 2024-01-24
#   fix onglet avancé
#   correction affichage onglet matching
# - 2024-01-25
#   Ajout traduction, documentation
#   Ajout B-M test
# - 2024-01-27
#   Fichier long converti en zip avant le téléchargement
#   Ajout caliper
#   Ajout method quick
# - 2024-01-29
#   Conversion en trois fichiers, server.R, ui.R, global.R
# - 2024-01-31
#   Ajout matching NULL
#   caliper disparait quand méthode ou distance incompatible avec caliper
#   filtrage des variables quand par 2 niveaux (ou 2 valeurs uniques)
#   nettoyage du fichier lorsque téléversé
# - 2024-02-01
#   le téléchargement principal va être un zip si le fichier est volumineux
#   (le long format est toujours un zip)
#   Ajout d'un onglet diagnostic rudimentaire
# - 2024-02-02
#   Ajout boite pour modifier type de variables
# - 2024-02-26
#   Amélioration des messages d'erreur lors du matching
#   Barre de progrès plutôt que modal
#   Message lorsque données retirées (NA) apparait dans le rapport (output_summary)
#   dx plot est fait avec cobalt::love.plot()
# - 2024-02-27
#   Correction de bugs dans l'affichage des graphiques descriptifs
#   Ajout de traductions
# - 2024-02-28
#   Fix graphiques descriptifs
#   Ajout de traductions
#   Ajout sens analysis

library(tidyverse)
library(ggrepel)
library(ggpubr)  # to combine plots
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)

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

library(cobalt)  # for matching assessment plot

library(rbounds)  # for sensitivity analysis

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
  df <- as.tibble(df)
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

run_props_tests <- function(df_long_data, unique_levels) {
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
  return(df_props)
}

# This function will accept: 
# a vector of response data, a vector specifying items,
# a group membership (treatment) variable (e.g. group1, group2),
# subclass: a variable that indicates pair in matching.
# Optional parameters: max_gamma
# for max gamma (default: 2 ) and gamma increase (default: .1)
# It will apply sensitivity binary analysis and return a data frame with the upper 
# and lower bounds, by item.
sensitivity_analysis <- function(response, items = "response", group, subclass, max_gamma = 2, gamma_inc = .1) {
  # Create a data frame with the response data, the items and the treatment variable
  #  response = df_match_long$SCORE_DICHO
  #  items = df_match_long$ITEM
  # treatment = df_match_long$STATUT_LING
  
  df <- data.frame(response, items, group, subclass)
  
  # Count discrepant pairs
  df_discrepant_cnt <- count_discrepant_pairs(response, group, items, subclass)
  
  # print(df_discrepant_cnt)
  
  # Apply the sensitivity analysis, item by item, using lapply
  apply_binarysens <- function(row) {
    binarysens(row$n_discrepant_x, row$n_discrepant_y, Gamma = max_gamma, GammaInc = gamma_inc)
  }
  binarysens_results <-
    lapply(1:nrow(df_discrepant_cnt), function(i)
      apply_binarysens(df_discrepant_cnt[i,]))
  
  # decode into a dataframe
  df_results_binarysens <- map_df(binarysens_results, ~ .x$bounds)
  
  # add the items column back to the df
  df_results_binarysens$item = rep(
    df_discrepant_cnt$item,
    each = nrow(df_results_binarysens) / nrow(df_discrepant_cnt)
  )
  
  names(df_results_binarysens) <- c("gamma", "lower_bound", "upper_bound", "measurement")
  
  
  return(df_results_binarysens)
}

# For each item, grab:
# max_sig_gamma : the largest gamma for which the p value is below the alpha treshold
# unconf_estimate : the upper_bound when gamma is exactly 1
make_sens_annotations <- function(df, alpha = .05) {
  df_sens_annotations <- df %>% 
    group_by(measurement) %>% 
    filter(upper_bound < alpha) %>% 
    summarise(
      max_sig_gamma = max(gamma),
      unconf_estimate = min(upper_bound)
    ) 
  
  # p_at_max_gamma : the upper_bound when gamma is at the max value in the data (e.g. 2)
  df_sens_annotations_2 <- df %>% 
    group_by(measurement) %>% 
    filter(gamma == max(gamma)) %>% 
    summarise(
      p_at_max_gamma = max(upper_bound)
    )
  # merge with df_sens_annotations
  df_sens_annotations <- df_sens_annotations %>%
    left_join(df_sens_annotations_2) %>%
    mutate(
      limit = if_else(p_at_max_gamma > alpha, alpha, p_at_max_gamma),
      label_1 = paste0("Γ = ", round(max_sig_gamma, 2), "\np = ", round(limit, 3)),
      label_2 = paste0("Γ = 1 \np = ", round(unconf_estimate, 3)),
      xpos_1 = max_sig_gamma,
      ypos_1 = alpha,
      xpos_2 = 1,
      ypos_2 = unconf_estimate
    )
  
  # pivot longer so that there is a single label column with the
  df_sens_labels <- df_sens_annotations %>%
    pivot_longer(
      cols = c(xpos_1, ypos_1, xpos_2, ypos_2, label_1, label_2),
      names_to = c(".value", "label_id"),
      names_pattern = "(.*)_(.)"
    )
  
  return(df_sens_labels)
}


# This function will accept a vector of response data, a vector specifying items,
# a group membership (treatment) variable (e.g. group1, group2), and a subclass
# variable (indicates pair in matching).
# It will return a data frame with the number of discrepant pairs, by item.
# discrepant_x = number of pairs where first group is higher than second group
# discrepant_y = number of pairs where first group is lower than second group
count_discrepant_pairs <- function(response, group, item, subclass) {
  # Create a data frame with the response data, the items and the treatment variable
  df <- data.frame(response, group, item, subclass)
  
  # keep the levels of the group variable in a vector
  levels_group <- levels(group)
  
  # Count sets of discrepant pairs
  df_compare <- df %>%
    filter(!is.na(response)) %>% 
    group_by(item, group, subclass) %>%
    summarise(
      response
    ) %>% pivot_wider(names_from = group, values_from = response) %>% 
    # calculate discrepancies, using dynamic level names
    mutate(discrepancy_x = as.numeric(get(levels_group[1]) > get(levels_group[2])),
           discrepancy_y = as.numeric(get(levels_group[1]) < get(levels_group[2]))
    ) %>% 
    summarise(
      n_discrepant_x = sum(discrepancy_x, na.rm = T),
      n_discrepant_y = sum(discrepancy_y, na.rm = T)
    )
  
  return(df_compare)
}

make_sensitivity_plot <- function(analysis_data, labels, alpha = .05) {
  print("Creating sens plot...")
  result <- analysis_data %>% 
    # filter(ITEM %in% df_sens_labels_model$ITEM) %>% 
    ggplot(aes(x = gamma, y = upper_bound)) +
    geom_line(linewidth = 1, alpha = .7) +
    geom_hline(
      yintercept = alpha,
      linetype = "dashed",
      color = "darkgrey",
      linewidth = .5
    ) +
    facet_wrap(~ measurement, nrow = 3) +
    labs(title = "",
         x = "Gamma",
         y = "p") +
    geom_label_repel(
      data = labels,
      aes(
        label = label,
        x = xpos,
        y = ypos,
        fill = label_id
      ),
      box.padding = 1, 
      nudge_y = .1,
      nudge_x = .01,
      show.legend = FALSE,
      alpha = .6
    ) + 
    theme_bw(base_size = 12)
  return(result)
}
