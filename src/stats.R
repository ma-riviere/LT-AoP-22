#==============================#
#### Stats helper functions ####
#==============================#

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

should_exp <- function(mod) {
  return(mod$modelInfo$family$link %in% c("log", "logit"))
}

distribution_summary <- function(data, dvs, between = "Condition") {
  data |> select(all_of(between), all_of(dvs)) |> 
    pivot_longer(all_of(dvs), names_to = "DV", values_to = "Value") |> 
    group_by(across(any_of(between)))  |> 
    group_map( 
      \(.x, .y) parameters::describe_distribution(.x |> group_by(DV)) |>
        mutate(
          Variance = SD^2,
          CoV = SD / Mean,
          Variable = str_remove(.group, fixed("DV="))
        ) |> 
        add_column(.y, .after = 1) |> 
        select("Variable", all_of(between), "Mean", "SD", "Variance", "CoV", "IQR", "Min", "Max", "Skewness", "Kurtosis", "n", "n_Missing")
    ) |> 
    purrr::reduce(full_join, by = c("Variable", all_of(between), "Mean", "SD", "Variance", "CoV", "IQR", "Min", "Max", "Skewness", "Kurtosis", "n", "n_Missing")) |> 
    arrange(Variable, across(any_of(between)))
}

#--------------------------#
#### Variable selection ####
#--------------------------#

find_formula_formatted <- function(mod) {
  return(
    c(
      insight::find_formula(mod)$conditional,
      purrr::map(
        insight::find_formula(mod)$random,
        \(.x) paste0("(", purrr::reduce(deparse(.x), \(.y) paste(.y)) |> stringr::str_replace_all("~", ""), ")")
      )
    ) |> purrr::keep(\(.z) .z != "()") |> paste(collapse = " + ")
  )
}

find_coefficient_count_mod <- function(mod) {
  return(find_coefficient_count(
    insight::get_data(mod),
    insight::find_predictors(mod)$conditional,
    insight::find_interactions(mod)$conditional
  ))
}

find_coefficient_count <- function(data, predictors, interactions = NULL) {
  
  coef_count <- 0
  coef_count <- purrr::map(predictors, \(pred) length(unique(data[[pred]])) - 1) |> purrr::reduce(\(x, y) sum(x, y, na.rm = TRUE), .init = 0)
  
  if (!is.null(interactions)) {
    coef_count <- coef_count + purrr::map(
      stringr::str_split(interactions, ":"), 
      \(int_preds) {
        purrr::map(int_preds, \(int_pred) length(unique(data[[int_pred]])) - 1) |> purrr::reduce(\(x, y) prod(x, y, na.rm = TRUE), .init = 1)
      }
    ) |> purrr::reduce(\(x, y) sum(x, y, na.rm = TRUE), .init = 0)
  }
  
  return(coef_count + 1) # Add one for the intercept
}

LRT <- function(mod, pred = "Condition") {
  data <- insight::get_data(mod)
  resp <- insight::find_response(mod)
  link <- insight::link_function(mod)
  preds <- insight::find_predictors(mod)$conditional
  inters <- insight::find_interactions(mod)$conditional
  
  preds_reduced <- stringr::str_subset(preds, pred, negate = T)
  inters_reduced <- stringr::str_subset(inters, pred, negate = T)
  
  reduced_formula <- glue("{resp} ~ . -{paste0(stringr::str_subset(preds, pred))}")
  if (!is.null(inters)) reduced_formula <- str_c(reduced_formula, glue("-{paste0(stringr::str_subset(inters, pred))}"))
  
  if (toupper(insight::find_algorithm(mod)$algorithm) == "REML") 
    cat(crayon::yellow(glue::glue("\n{crayon::bold('\n[LRT]')} Full model was fit with REML --> Refitting with ML.\n")))
  
  mod_full <- tryCatch(
    update(mod, REML = FALSE),
    
    warning = \(w) {
      cat(crayon::red(glue::glue("{crayon::bold('\n[LRT]')} Model convergence warning (full model):\n\n")))
      print(w)
      cat(crayon::yellow(glue::glue("{crayon::bold('[LRT]')} Retrying with better starting values ...\n\n")))
      
      beta_start_full <- c(link(mean(insight::get_response(mod))), rep(0, find_coefficient_count(data, preds, inters) - 1))
      
      return(
        tryCatch(
          update(mod, REML = FALSE, start = list(beta = beta_start_full)),
          warning = \(w) {
            update(mod, REML = FALSE, start = list(beta = beta_start_full), control = glmmTMBControl())
          },
          error = \(e) {
            update(mod, REML = FALSE, start = list(beta = beta_start_full), control = glmmTMBControl())
          }
        )
      )
    },
    error = \(e) {
      cat(crayon::red(glue::glue("{crayon::bold('\n[LRT]')} Model convergence error (full model):\n\n")))
      print(e)
      cat(crayon::yellow(glue::glue("{crayon::bold('[LRT]')} Retrying with better starting values ...\n\n")))
      
      beta_start_full <- c(link(mean(insight::get_response(mod))), rep(0, find_coefficient_count(data, preds, inters) - 1))
      
      return(
        tryCatch(
          update(mod, REML = FALSE, start = list(beta = beta_start_full)),
          warning = \(w) {
            update(mod, REML = FALSE, start = list(beta = beta_start_full), control = glmmTMBControl())
          },
          error = \(e) {
            update(mod, REML = FALSE, start = list(beta = beta_start_full), control = glmmTMBControl())
          }
        )
      )
    }
  )
  
  mod_reduced <- tryCatch(
    update(mod, formula. = reduced_formula, REML = FALSE, start = NULL), 
    
    warning = \(w) {
      cat(crayon::red(glue::glue("{crayon::bold('\n[LRT]')} Model convergence warning (reduced model):\n\n")))
      print(w)
      cat(crayon::yellow(glue::glue("{crayon::bold('[LRT]')} Retrying with better starting values ...\n\n")))
      
      beta_start_reduced <- c(link(mean(insight::get_response(mod))), rep(0, find_coefficient_count(data, preds_reduced, inters_reduced) - 1))
      
      return(
        tryCatch(
          update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced)),
          warning = \(w) {
            update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced), control = glmmTMBControl())
          },
          error = \(e) {
            update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced), control = glmmTMBControl())
          }
        )
      )
    },
    error = \(e) {
      cat(crayon::red(glue::glue("{crayon::bold('\n[LRT]')} Model convergence error (reduced model):\n\n")))
      print(e)
      cat(crayon::yellow(glue::glue("{crayon::bold('[LRT]')} Retrying with better starting values ...\n\n")))
      
      beta_start_reduced <- c(link(mean(insight::get_response(mod))), rep(0, find_coefficient_count(data, preds_reduced, inters_reduced) - 1))
      
      return(
        tryCatch(
          update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced)),
          warning = \(w) {
            update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced), control = glmmTMBControl())
          },
          error = \(e) {
            update(mod, formula. = reduced_formula, REML = FALSE, start = list(beta = beta_start_reduced), control = glmmTMBControl())
          }
        )
      )
    }
  )

  formula_full <- find_formula_formatted(mod_full)
  formula_reduced <- find_formula_formatted(mod_reduced)
  
  cat(crayon::blue(glue::glue("{crayon::bold('\n[LRT]')} Full formula: {formula_full}\n")))
  cat(crayon::blue(glue::glue("{crayon::bold('\n[LRT]')} Reduced formula: {formula_reduced}\n")))
  
  res <- stats::anova(mod_full, mod_reduced, test = "LRT") |> as.data.frame() |> rownames_to_column("Model") |> janitor::clean_names()
  # res <- performance::test_lrt(mod_full, mod_reduced, estimator = "OLS")
  
  cat(paste0(
    "\n\n$\\mathcal{X}_", glue("{res$chi_df[2]}"), "^2", glue(" = {round(res$chisq[2], 3)}; "),
    scales::pvalue(res$pr_chisq[2], add_p = T, prefix = c("p < ", "p = ", "p > ")), "$"
  ))
  
  return(res)
}


#---------------------#
#### Bootstrapping ####
#---------------------#

get_boot_ci <- function(.mod, .f, .nsim = 100, .method = "perc", .width = .95) {
  boot <- lme4::bootMer(.mod, FUN = .f, nsim = .nsim)
  bootci <- confint(boot, level = .width, method = .method)
  tibble(Wald = boot$t0, BS.mean = mean(boot$t), BS.LCL = bootci[1], BS.UCL = bootci[2])
}