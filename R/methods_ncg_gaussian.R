#' Helper for \code{summary} and \code{plot} methods.
#'
#' @param object an object of class \code{ncg_gaussian}.
#'
#' @return A \code{tibble} (table) of intermediate results.
#' @importFrom magrittr "%>%"
helper_ncg_gaussian <- function(object) {
	# Get the reference group.
	reference_group <- attr(object, "reference_group")

	# Function to return an estimate for a given group, where the intercept is added
	# where appropriate.
	estimate <- function(group) {
		result <- lme4::fixef(object$analysis_result)[group]
		if (group != "(Intercept)") result <- result + lme4::fixef(object$analysis_result)["(Intercept)"]
		result
	}
	
	# Function to return a CI bound for a given group, taking into account the uncertainty
	# on the intercept, too.
	ci <- function(group, level) {
		result <- confint(object$analysis_result, method = "Wald")[group, level]
		if (group != "(Intercept)")
			result <- result + confint(object$analysis_result, method = "Wald")["(Intercept)", level]
		result
	}
	
	# The standard errors.
	standard_errors <- object$analysis_result %>% summary() %>% coefficients() %>% {.[, "Std. Error"]} %>% unname()
	
	# Iterate over each fixed effect group ...
	analysis_tibble <- names(lme4::fixef(object$analysis_result)) %>%
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
														 reference_group,
														 substring(group, nchar("Uganda"))),
			diff = mean_score - estimate("(Intercept)"), # ... add differences and their CIs ...
			diff_lower = ci_lower - ci("(Intercept)", "2.5 %"),
			diff_upper = ci_upper - ci("(Intercept)", "97.5 %"),
			se = standard_errors)

	# Join the analytical and sample tables.
	dplyr::inner_join(analysis_tibble, object$sample_result, by = "group")
}

#' Summarize a \code{ncg_gaussian} object as a table
#'
#' @param object an object of class \code{ncg_gaussian}.
#'
#' @return A \code{tibble} (table) of results.
#' @importFrom magrittr "%>%"
#' @export
summary.ncg_gaussian <- function(object) {
	# Define a function to approximate a P-value using Z-stats using the SE and effect size.
	p_value <- function(x, se) 2 * (1 - pnorm(abs(x), mean = 0, sd = se))

	object %>% helper_ncg_gaussian() %>%
		dplyr::transmute( # ... Format the table nicely for display purposes ...
			` ` = group,
			n = n,
			`Mean score (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
																			100 * mean_score %>% signif(2),
																			100 * ci_lower %>% signif(2),
																			100 * ci_upper %>% signif(2)),
			`Difference (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
																			100 * diff %>% signif(2),
																			100 * diff_lower %>% signif(2),
																			100 * diff_upper %>% signif(2)),
			`P-value` = p_value(diff, se) %>% format.pval(eps = 0.0001, digits = 2, scientific = FALSE)
		) %>%
		(function(x) { # ... blank out elements we do not want ...
			x[1, 4] <- "0"
			x[1, 5] <- ""
			x
		})
}

#' Plot a \code{ncg_gaussian} object
#'
#' @param object an object of class \code{ncg_gaussian}.
#'
#' @return A plot showing the results.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
plot.ncg_gaussian <- function(object) {
	# Get the reference group.
	reference_group <- attr(object, "reference_group")

	results_tibble <- object %>%
		helper_ncg_gaussian() %>%
		dplyr::mutate(group = as.factor(group),
									group = forcats::fct_relevel(group, reference_group),
									group = forcats::fct_relevel(group, groups$norwegians, after = Inf),
									group = forcats::fct_rev(group))
	
	# Make the mean score plot.
	forest_plot <- results_tibble %>% ggplot2::ggplot(ggplot2::aes(x = group)) +
    ggplot2::geom_point(ggplot2::aes(x = group, y = mean_score)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
    													color = "steelblue", alpha = 0.25, size = 4) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 11, face = "bold")) +
    ggplot2::xlab(NULL) +
  	ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  	ggplot2::ylab("Mean Score")

		# Define a function to make a "table" plot.
  table_plot <- function(label_f, subtitle) {
    results_tibble %>%
      label_f() %>%
      ggplot2::ggplot(ggplot2::aes(x = group)) +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(x = group, y = 0, label = label), hjust = "inward") +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expand_scale(mult = c(0, 0))) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::labs(subtitle = subtitle) +
      ggplot2::theme(# Blank out the grid and axis tick labels.
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                   	 plot.subtitle = ggplot2::element_text(hjust = 0))
  }
  
  # Define labelling functions to create text labels for means and CIs.
  n_label_f <- function(x) {
  	dplyr::mutate(x, label = sprintf("%s", n))
  }
  mean_label_f <- function(x) {
		dplyr::mutate(x, label = sprintf("%s%% (%s%% to %s%%)", 100 * mean_score %>% signif(2),
																														100 * ci_lower %>% signif(2),
																														100 * ci_upper %>% signif(2)))
  }
  
  # Make the table plots.
  n_table <- table_plot(label_f = n_label_f, subtitle = "n")
  means_table <- table_plot(label_f = mean_label_f, subtitle = "Mean Score (95% CI)")

	# Join up and return the plots.
	patchwork::wrap_plots(forest_plot, n_table, means_table, widths = c(0.4, 0.03, 0.2), ncol = 3)
}
