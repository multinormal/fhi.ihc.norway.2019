#' Summarize a \code{nca_gaussian} object as a table
#'
#' @param object an object of class \code{nca_gaussian}.
#'
#' @return A \code{tibble} (table) of results.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
summary.nca_gaussian <- function(object) {
	object <- object$analysis_result

	# Function to return an estimate for a given group, where the intercept is added
	# where appropriate.
	estimate <- function(group) {
		result <- coef(object)[group]
		if (group != "(Intercept)") result <- result + coef(object)["(Intercept)"]
		result
	}
	
	# Function to return a CI bound for a given group, taking into account the uncertainty
	# on the intercept, too.
	ci <- function(group, level) {
		result <- confint(object)[group, level]
		if (group != "(Intercept)") result <- result + confint(object)["(Intercept)", level]
		result
	}
	
	# The svyglm function does not appear to provide P-values, so define a function to
	# approximate a P-value using Z-statistics using the SE and effect size.
	p_value <- function(x, se) 2 * (1 - pnorm(abs(x), mean = 0, sd = se))
	
	# Iterate over each group ...
	names(coef(object)) %>%
		purrr::map( # ... make a 1-row tibble with the groups, mean score, and CIs ...
			function(group) {
				tibble::tibble(
					group = group,
					mean_score = estimate(group),
					ci_lower = ci(group, "2.5 %"),
					ci_upper = ci(group, "97.5 %")
					)
			}) %>%
		dplyr::bind_rows() %>% # ... bind the tibbles ...
		dplyr::mutate(
			group = dplyr::if_else(group == "(Intercept)", # ... rename the intercept level ...
														 groups$norwegians,
														 substring(group, nchar("Uganda"))),
			diff = mean_score - estimate("(Intercept)"), # ... add differences and their CIs ...
			diff_lower = ci_lower - ci("(Intercept)", "2.5 %"),
			diff_upper = ci_upper - ci("(Intercept)", "97.5 %"),
			se = object %>% vcov() %>% diag() %>% sqrt() %>% unname()
			) %>%
		dplyr::transmute( # ... Format the table nicely for display purposes ...
			` ` = group,
			`Mean score (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
																			100 * mean_score %>% signif(2),
																			100 * ci_lower %>% signif(2),
																			100 * ci_upper %>% signif(2)),
			`Difference from Norwegians (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
																			100 * diff %>% signif(2),
																			100 * diff_lower %>% signif(2),
																			100 * diff_upper %>% signif(2)),
			`P-value` = p_value(diff, se) %>% format.pval(eps = 0.0001, scientific = FALSE)
		) %>%
		(function(x) { # ... blank out elements we do not want ...
			x[1, 3] <- ""
			x[1, 4] <- ""
			x
		})
}