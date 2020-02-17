#' Exploratory analysis of association of demographics and Norwegians' outcomes.
#' 
#' This function is used to perform exploratory regression analyses of the
#' mean score, and of passing or mastery, for Norwegians. Note that a subset
#' of the Norwegian respondents are used (the same as those used in the 
#' comparison with Ugandans). This subset is used because the repondents were
#' asked all the questions necessary to assess passing and mastery.
#'
#' @param outcome The outcome of interest (e.g., passing or mastery).
#' @param outcome_name The name of the outcome (for display in tables etc.)
#' @param family The error famility (and link function) to use for the outcome.
#' @return an obejct of class \code{expl_reg} containg the results of the 
#'  estimation procedure (itself an instance of the class \code{MIresult}).
#' @importFrom magrittr "%>%"
#' @export
exploratory_regression_norwegians <- function(outcome, outcome_name, family) {
  data <- norwegian_num_correct() %>%
    # Add a mean score column for use in imputation.
    dplyr::mutate(mean_score_mi = num_correct / num_total) %>%
    # To allow multiple imputation to be used to address missing values of the
    # poststratification variables, set explicit missing levels to NA.
    dplyr::mutate_if(is.factor, forcats::fct_recode, NULL = "Missing") %>%
    # ... and rename some variables.
    dplyr::rename(Research_training = `Research training`,
                  Research_participant = `Research participant`,
                  Medical_education = `Medical education`)

  # Initialize MICE for each of the incomplete data sets.
  init <- suppressWarnings(mice::mice(data, maxit = 0))
  
  # Make a predictor matrix that excludes variables that should not be use in MICE.
  cols_to_remove <- c("NR", "num_correct", "num_total", "passing", "mastery")
  predictorMatrix <- init$predictorMatrix
  cols_to_remove <- intersect(cols_to_remove, colnames(predictorMatrix))
  predictorMatrix[, cols_to_remove] <- 0
  imputed <- mice::mice(data, seed = 1234, method = init$method,
                        predictorMatrix = predictorMatrix,
                        printFlag = FALSE)
  
  analyze <- function(i) {
    completed <- complete(imputed, i)
    completed$ISCED_Level <- factor(completed$ISCED_Level, ordered = FALSE)
    design <- weighted_design(completed)

    # Perform regression to estimate the effect of the covariates.
    model <- paste(outcome, " ~ Sex + Research_training + ",
                                "Research_participant + ISCED_Level +",
                                "Medical_education")
    survey::svyglm(formula = model, design = design,
                   family = family,
                   maxit = 100) # To achieve convergence.
  }

  # Use multiple imputation (and poststratification) to estimate. Note that we
  # use the mice package for the imputation, and can then just use mitools'
  # MIcombine to combine the list of imputed estimates.
  result <- 1:imputed$m %>% purrr::map(analyze) %>% mitools::MIcombine()

  # Attach attributes and set the class.
  attr(result, "family") <- family
  attr(result, "outcome_name") <- outcome_name
  class(result) <- c("expl_reg", class(result))

  # Return the result.
  result
}

