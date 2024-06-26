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
# - 2024-02-29
#   Fix choix mahalanobis + caliper
#   Affichage paramètres matching
#   Affichage conditionnel onglet avancé
#   Choix du max gamma et du alpha dans l'analyse de sensibilité
#   Retrait des plots sensibilité (inutiles) remplacés par tableau synthèse
# - 2024-03-01
#   Prop test sur données non-appariées et appariées
#   Meilleur affichage prop test
#   Avertissement résultats qui ne sont plus à jour
#   Bug fix prop tests
# - 2024-03-14
#   Intégration des analyses modèles mixtes
#   Bug fix, affichage des résultats des analyses avancées
#   Progress bar pour analyses avancées
#   Love plot en 2 plots (améliore un peu la hauteur d'affichage pour éviter que 
#   les labels se chevauchent)
# - 2024-03-18
#   Gestion rudimentaire des erreurs dans le fit des modèles mixtes
#   Mise à jour du about.html (anglais seulement)
# - 2024-03-20
#   Try autour de la fonction MatchIt.
#   Fix hauteur affichage des love plots (maintenant il y a une estimation de la hauteur requise)
# - 2024-03-28
#   Correction bug affichage mah vars dans le rapport de matching
#   Bug fix, analyse de sensibilité lorsqu'il y a 0 discrepant pairs
#   Diverses corrections pour s'assurer que le résultat est identifique à la
#   version "script"

library(tidyverse)
# library(ggrepel)
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

library(janitor)  # used to clean up columns, could be removed

# explicit library calls so that the server can use the more advanced methods and distance measurements
library(optmatch)
# library(randomForest)
# library(rpart)
# library(mgcv)  # for gam
library(glmnet)  # for lasso
library(quickmatch)  # for quick

# library(brunnermunzel)  # for BM tests

library(cobalt)  # for matching assessment plots
library(rbounds)  # for sensitivity analysis

library(glmmTMB)  # for mixed model analysis
library(sjPlot)  # for mixed model tables

# library(parallel)


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
pivot_data <- function(df, cols_to_pivot) {

  validate(
    need(!is.null(cols_to_pivot) && length(cols_to_pivot) > 0, "No columns selected for pivoting."),
    # need(all(cols_to_pivot %in% colnames(df_matched_data)), "One or more selected columns do not exist in the data frame.")
  )
  
  # print("Advanced vars (cols_to_pivot) :")
  # print(cols_to_pivot)
  
  # convert selected columns to numeric
  df <- df %>%
    mutate(across(all_of(cols_to_pivot), as.numeric))
  
  # add unique .id column in case user wants mixed model analysis
  # with padded zeroes to ensure proper sorting
  df <- df %>%
    mutate(.id = row_number()) %>%
    mutate(.id = str_pad(.id, width = nchar(nrow(df)), side = "left", pad = "0"))
    
  # pivot to long format
  df_long_data <- df %>%
    pivot_longer(cols = all_of(cols_to_pivot), names_to = "measurement", values_to = "score")
  
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
  df <- as_tibble(df)
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
  # make sure df_long_data is long format data with a column named .group
  if (!".group" %in% colnames(df_long_data)) {
    stop("The data frame must have a column named .group")
  }
  
  if (!"measurement" %in% colnames(df_long_data)) {
    stop("The data frame must have a column named measurement")
  }
  
  # # make sure we have at least 5 observations per group
  # if (any(df_long_data %>% group_by(.group, measurement) %>% summarise(n = n()) %>% pull(n) < 5)) {
  #   stop("At least one cell has fewer than 5 observations")
  # }
  
  # print(head(df_long_data))
  df_props <- df_long_data %>%
    filter(!is.na(score)) %>% 
    group_by(.group, measurement) %>%
    summarise(
      prop = mean(score, na.rm = T),
      fail = 1 - prop,
      n = n(),
      n_success = n * prop,
      .groups = 'drop'
    ) %>% pivot_wider(
      id_cols = measurement,
      names_from = .group, 
      values_from = c(prop, n, n_success)
    ) %>% 
    ungroup()
  
  df_props <- df_props %>% group_by(measurement) %>%
    mutate(p = prop.test(
      x = c(!!sym(paste0("n_success_", unique_levels[1])),
            !!sym(paste0("n_success_", unique_levels[2]))),
      n = c(!!sym(paste0("n_", unique_levels[1])),
            !!sym(paste0("n_", unique_levels[2])))
    )$p.value %>% round(4) %>% format.pval(eps = .001)) %>% 
    # round the columns that start with prop_ to 2 decimals
    mutate(across(starts_with("prop_"), ~ round(., 2))) %>%
    select(measurement,
           !!sym(paste0("n_", unique_levels[1])),
           !!sym(paste0("n_", unique_levels[2])),
           !!sym(paste0("prop_", unique_levels[1])),
           !!sym(paste0("prop_", unique_levels[2])),
           p) %>%
    as.data.frame()
  return(df_props)
}

sensitivity_analysis <- function(response, items = "response", group, subclass, max_gamma = 2, gamma_inc = .1) {
  # Create a data frame with the response data, the items and the treatment variable
  #  response = df_match_long$SCORE_DICHO
  #  items = df_match_long$ITEM
  # treatment = df_match_long$STATUT_LING
  
  # Count discrepant pairs
  df_discrepant_cnt <- count_discrepant_pairs(response, group, items, subclass)
  
  # print(df_discrepant_cnt)
  
  # Apply the sensitivity analysis, item by item, using lapply
  apply_binarysens <- function(row) {
    if (row$n_discrepant_x == 0 | row$n_discrepant_y == 0) {
      return(NULL)
    }
    binarysens(row$n_discrepant_x, row$n_discrepant_y, Gamma = max_gamma, GammaInc = gamma_inc)
  }
  binarysens_results <-
    lapply(1:nrow(df_discrepant_cnt), function(i)
      apply_binarysens(df_discrepant_cnt[i,]))
  
  # name the binarysens_results to match the items
  names(binarysens_results) <- df_discrepant_cnt$item
  
  # loop across the items and bind the results when they are not NULL
  df_results_binarysens <- do.call(bind_rows, lapply(names(binarysens_results), function(item_name) {
    x <- binarysens_results[[item_name]]
    if (!is.null(x$bounds)) {  # Ensure there's a bounds list to work with
      df <- as.data.frame(x$bounds)  # Convert bounds to a data frame
      df$item_name <- item_name  # Add a new column with the item name
      return(df)
    } else {
      df <- data.frame(item_name = item_name)
      return(df)
    }
  }))
  
  names(df_results_binarysens) <- c("gamma", "lower_bound", "upper_bound", "item")
  
  
  return(df_results_binarysens)
}

# For each item, grab:
# max_sig_gamma : the largest gamma for which the p value is below the alpha treshold
# unconf_estimate : the upper_bound when gamma is exactly 1
make_sens_annotations <- function(df, alpha = .05) {
  # for each item, return the bound when gamma is exactly 1
  
  # keep the NA items
  na_items <- df %>% 
    filter(is.na(upper_bound) & is.na(lower_bound)) %>% 
    select(item)
  
  # now filter the df
  df <- df %>% 
    filter(!item %in% na_items$item)
  
  # find the unconf_estimate
  df_unconf <- df %>% 
    group_by(item) %>% 
    filter(gamma == 1) %>% 
    summarise(
      unconf_estimate = upper_bound
    )
  
  # find if, when gamma > 1, lower bound is larger than when gamma == 1
  df_increase_test <- df %>% 
    group_by(item) %>% 
    filter(gamma > 1) %>% 
    summarise(
      lb_next = min(lower_bound)
    )
  
  # merge
  df_unconf <- df_unconf %>% 
    left_join(df_increase_test, by = "item") %>% 
    mutate(
      increase = lb_next > unconf_estimate
    )
  
  # if the lower bound is increasing, then find the largest gamma at which the lower bound is below alpha
  if (df_unconf[1, "increase"] == TRUE) {
    df_max_sig <- df %>% 
      group_by(item) %>% 
      filter(lower_bound < alpha) %>% 
      summarise(
        max_sig_gamma = max(gamma)
      )
    df_p_at_max_gamma <- df %>% 
      group_by(item) %>% 
      filter(gamma == max(gamma)) %>% 
      summarise(
        p_at_max_gamma = max(lower_bound)
      )
  } else {
    df_max_sig <- df %>% 
      group_by(item) %>% 
      filter(upper_bound < alpha) %>% 
      summarise(
        max_sig_gamma = max(gamma)
      )
    df_p_at_max_gamma <- df %>% 
      group_by(item) %>% 
      filter(gamma == max(gamma)) %>% 
      summarise(
        p_at_max_gamma = max(upper_bound)
      )
  }
  
  # merge 
  df_sens_annotations <- df_unconf %>% 
    left_join(df_max_sig, by = "item") %>% 
    left_join(df_p_at_max_gamma, by = "item") %>% 
    select(item, unconf_estimate, max_sig_gamma) %>% 
    mutate(unconf_estimate = format.pval(unconf_estimate, eps = .001))
  
  # let's add back the items in na_items
  df_sens_annotations <- df_sens_annotations %>% 
    bind_rows(na_items) %>% 
    arrange(item)
  
  return(df_sens_annotations)
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


add_to_summary <- function(label, value) {
  if (!is.null(value)) {
    # If value is not NULL, append to summary
    return(paste("\n", label, value, sep = ""))
  }
  return("")
}

# Mixed model analysis ----
mm_analysis_null_model <- function(df_long) {
  print("Running null model...")
  print(paste("n =", nrow(df_long)))
  # first me make sure the required columns exist
  if (!".group" %in% colnames(df_long)) {
    stop("The data frame must have a column named .group")
  }
  
  if (!".id" %in% colnames(df_long)) {
    stop("The data frame must have a column named .id")
  }
  
  if (!"measurement" %in% colnames(df_long)) {
    stop("The data frame must have a column named measurement")
  }
  
  if (!"score" %in% colnames(df_long)) {
    stop("The data frame must have a column named score")
  }
  
  # make sure .groups has only 2 unique values
  if (length(unique(df_long$.group)) != 2) {
    stop("The .group column must have exactly 2 unique values")
  }
  
  df_long <- df_long %>% filter(!is.na(score))
  # Sys.sleep(1)
  
  mm_null <- glmmTMB::glmmTMB(
    formula = score ~ 1 + (1 | .id),
    family = "binomial",
    data = df_long
    # control = glmmTMBControl(parallel = parallel::detectCores())
  )
  
}

mm_analysis_uniform_model <- function(mm_null, df_long) {

  df_long <- df_long %>% filter(!is.na(score))
  
  # Sys.sleep(1)
  
  mm_uniform <- update(mm_null, . ~ .group + . 
                       # control = glmmTMBControl(parallel = parallel::detectCores())
  )

  return(mm_uniform)
}

mm_analysis_nonuniform_model <- function(mm_uniform, df_long) {
  
  df_long <- df_long %>% filter(!is.na(score))
  
  mm_nonuniform <- update(mm_uniform, . ~ . +  measurement + .group:measurement
                          # control = glmmTMBControl(parallel = parallel::detectCores())
  )
  # Sys.sleep(1)
  
  return(mm_nonuniform)
}

mm_analysis_comparison <- function(mm_null, mm_uniform, mm_nonuniform = NULL) {
  
  # if a mm_nonuniform was provided, we compare it to mm_null and mm_uniform
  if (!is.null(mm_nonuniform)) {
    lr_test <- anova(mm_null, mm_uniform, mm_nonuniform)
  } else {
    lr_test <- anova(mm_null, mm_uniform)
  }
  # Sys.sleep(1)
  return(lr_test)
}

# Estimates the required height for the Love plot
estimate_lp_height <-
  function(match_object,
           baseline = 200,
           mult = 30) {
    cur_summary <- summary(match_object)
    n_levels_top <- nrow(cur_summary[["sum.all"]])
    n_levels_bottom <- length(colnames(match_object$X))
    
    height_top <- baseline + (n_levels_top * mult)
    height_bottom <- baseline + (n_levels_bottom * mult)
    
    return(
      data.frame(
        top = height_top,
        bottom = height_bottom
    ))
  }
