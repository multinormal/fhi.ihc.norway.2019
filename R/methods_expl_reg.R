#' Summarize an instance of \code{expl_reg}
#'
#' @param object an instance of \code{expl_reg}.
#' @return a 1-row \code{tibble} that summarizes the result in a readable way.
#' @importFrom magrittr "%>%"
#' @export
summary.expl_reg <- function(object) {
  # Obtain the appropriate transform given the error family.
  family <- attr(object, "family")
  stopifnot(family$family %in% c("gaussian", "quasibinomial"))
  tx <- if (family$family == "gaussian") {
    function(x) sprintf("%.1f%%", (100 * x) %>% signif(2))
  } else {
    function(x) x %>% exp() %>% signif(2)
  }

  point_estimate_ci <- function(x) {
    estimate       <- coef(object)[x]               %>% tx()
    estimate_lower <- confint(object)[x, "2.5 %"]   %>% tx()
    estimate_upper <- confint(object)[x, "97.5 %"]  %>% tx()
    paste0(estimate, " (", estimate_lower, " to ", estimate_upper, ")")
  }

  outcome_name <- attr(object, "outcome_name")
  tibble::tibble(
    ` `                    = outcome_name,
    Intercept              = point_estimate_ci("(Intercept)"),
    Male                   = point_estimate_ci("SexMale"),
    `Research participant` = point_estimate_ci("Research_participantYes"),
    `ISCED Levels 3-4`     = point_estimate_ci("ISCED_LevelLevels 3-4"),
    `ISCED Levels 5-8`     = point_estimate_ci("ISCED_LevelLevels 5-8"),
    `Medical education`     = point_estimate_ci("Medical_educationYes")
  )
}