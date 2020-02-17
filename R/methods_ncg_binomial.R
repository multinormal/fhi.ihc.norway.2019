#' Helper for \code{summary} and \code{plot} methods.
#'
#' @param object an object of class \code{ncg_gaussian}.
#'
#' @return A \code{tibble} (table) of intermediate results.
#' @importFrom magrittr "%>%"
helper_ncg_binomial <- function(object) {
	analysis_result <- object$analysis_result
	sample_result <- object$sample_result

	# Get the reference group.
	reference_group <- attr(object, "reference_group")

	# Function to return an odds ratio for a given group relative to the reference group
	# (intercept). The result is returned on the OR scale. If the group is the
	# intercept group, return NA.
	odds_ratio <- function(group) lme4::fixef(analysis_result)[group] %>% exp()

	# Function to return a CI bound for a given group. The result is returned on the OR
	# scale. If the group is the intercept group, return NA.
	odds_ratio_ci <- function(group, level) {
		confint(analysis_result, method = "Wald")[group, level] %>% exp()
	}

	# Function to get a P-value for a given group.
	p_value <- function(group) {
		summary(analysis_result) %>% coefficients() %>% {.[group, "Pr(>|z|)"]}
	}

	# Iterate over each fixed effect group ...
	analysis_tibble <- names(lme4::fixef(object$analysis_result)) %>%
		purrr::map( # ... make a 1-row tibble with the groups, mean score, and CIs ...
			function(group) {
				tibble::tibble(
					group = group,
					or = odds_ratio(group),
					ci_lower = odds_ratio_ci(group, "2.5 %"),
					ci_upper = odds_ratio_ci(group, "97.5 %"),
					p_value = p_value(group))
			}) %>%
		dplyr::bind_rows() %>% # ... bind the tibbles ...
		dplyr::mutate(
			group = dplyr::if_else(group == "(Intercept)", # ... rename the intercept level ...
														 reference_group,
														 substring(group, nchar("Uganda"))))
	
	# Create a column with the odds for the reference group for all groups, except for the
	# reference group itself, which should be 1, so that multiplication by the OR for that
	# group (which is the odds in the reference group) gives the estimated odds for all
	# groups.
	analysis_tibble <- analysis_tibble %>%
		dplyr::mutate(ref_odds = odds_ratio("(Intercept)")) %>%
		(function(x) {
			x[x$group == reference_group,]$ref_odds <- 1
			x
		})

	# Define a function to convert odds to a probability.
	o_to_p <- function(x) x / (1 + x)

	# Join the analytical and sample tables, and use them to prepare the summary.
	dplyr::inner_join(analysis_tibble, sample_result, by = "group") %>%
		# Calculate quantities that are used for forming the final summary table,
		# most of which are also useful for checking calculations.
		dplyr::mutate(corresponding_odds = ref_odds * or,
									corresponding_odds_lower = ref_odds * ci_lower,
									corresponding_odds_upper = ref_odds * ci_upper,
									corresponding_prob = o_to_p(corresponding_odds),
									corresponding_prob_lower = o_to_p(corresponding_odds_lower),
									corresponding_prob_upper = o_to_p(corresponding_odds_upper),
									prob_diff = corresponding_prob - corresponding_prob[1],
									prob_diff_lower = corresponding_prob_lower - corresponding_prob[1],
									prob_diff_upper = corresponding_prob_upper - corresponding_prob[1]) %>%
		# Check that the sample odds are contained in the confidence intervals for the odds.
		(function(x) {
			stopifnot(all(x$corresponding_odds_lower[-1] <= x$sample_odds[-1]))
			stopifnot(all(x$sample_odds[-1] <= x$corresponding_odds_upper[-1]))
			x
		})
}

#' Summarize a \code{ncg_binomial} object as a table
#'
#' @param object an object of class \code{nca_quasibinomial}.
#' @return A \code{tibble} (table) of results.
#' @importFrom magrittr "%>%"
#' @export
summary.ncg_binomial <- function(object) {
 object %>%
	 helper_ncg_binomial() %>%
	 # Now convert those into a readable table.
	 dplyr::transmute(` ` = group,
	 								 `Sample` = sprintf("%i / %i", sample_n, sample_n + sample_k),
	 								 `  ` =  sprintf("(%.1f%%)", 100 * sample_prob),
	 								 `Probability (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
	 																								 100 * corresponding_prob %>% signif(2),
	 																								 100 * corresponding_prob_lower %>% signif(2),
	 																								 100 * corresponding_prob_upper %>% signif(2)),
	 								 `Difference (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
	 																								 100 * prob_diff %>% signif(2),
	 																								 100 * prob_diff_lower %>% signif(2),
	 																								 100 * prob_diff_upper %>% signif(2)),
	 								 `Odds ratio (95% CI)` = sprintf("%s (%s to %s)",
	 																								 or %>% signif(2),
	 																								 ci_lower %>% signif(2),
	 																								 ci_upper %>% signif(2)),
	 								 `P value` = p_value %>% format.pval(eps = 0.0001, digits = 2, scientific = FALSE)
	 ) %>%
	 (function(x) { # ... deal with the first row (the reference group) ...
	 	x[1, 5:7] <- c("0", "1", "")
	 	x
	 })
}

#' Plot a \code{ncg_binomial} object
#'
#' @param object an object of class \code{ncg_gaussian}.
#'
#' @return A plot showing the results.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
plot.ncg_binomial <- function(object) {
	# Get the reference group.
	reference_group <- attr(object, "reference_group")

	# Get the name of the dependent variable.
	dep_var <- object$analysis_result@call$formula %>% all.vars() %>% {.[1]}

	results_tibble <- object %>%
		helper_ncg_binomial() %>%
		dplyr::mutate(group = as.factor(group),
									group = forcats::fct_relevel(group, reference_group),
									group = forcats::fct_relevel(group, groups$norwegians, after = Inf),
									group = forcats::fct_rev(group))

	# Set up logarithmic breaks, removing any that lie outside the range to be displayed.
	breaks <- c(0.001, 0.1, 0.3, 1.0, 3, 10, 30, 100)
	breaks[breaks < min(results_tibble$ci_lower)] <- NA
	breaks[breaks > max(results_tibble$ci_upper)] <- NA
	break_labels <- as.character(breaks)

	# Make the OR plot.
	or_forest_plot <- results_tibble %>%
		(function(x) {
			x[x$group == reference_group, "or"] <- 1
			x[x$group == reference_group, "ci_lower"] <- 1
			x[x$group == reference_group, "ci_upper"] <- 1
			x
		}) %>%
		ggplot2::ggplot(ggplot2::aes(x = group)) +
    ggplot2::geom_point(ggplot2::aes(x = group, y = or)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
    													color = "steelblue", alpha = 0.25, size = 4) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::xlab(NULL) +
  	ggplot2::scale_y_continuous(trans = "log10", breaks = breaks, labels = break_labels, limits = c(min(results_tibble$ci_lower), max(results_tibble$ci_upper))) +
  	ggplot2::ylab("Odds Ratio") +
  	ggplot2::theme(# Blank out the minor grid ticks
									 panel.grid.minor = ggplot2::element_blank(),
									 axis.text.y  = ggplot2::element_blank()) +
  	ggplot2::geom_hline(yintercept = 1)

	# Make a plot of probability of "success".
	prob_plot <- results_tibble %>%
		ggplot2::ggplot(ggplot2::aes(x = group)) +
    ggplot2::geom_point(ggplot2::aes(x = group, y = corresponding_prob)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = corresponding_prob_lower, ymax = corresponding_prob_upper),
    													color = "steelblue", alpha = 0.25, size = 4) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::xlab(NULL) +
  	ggplot2::scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  	ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
							     axis.text.y = ggplot2::element_blank()) +
  	ggplot2::ylab(paste("Probability of", dep_var %>% tools::toTitleCase()))

	 # Define a function to make a "table" plot.
   table_plot <- function(label_f, subtitle, inculude_group_labels = FALSE) {
     result <- results_tibble %>%
       label_f() %>%
       ggplot2::ggplot(ggplot2::aes(x = group)) +
       ggplot2::coord_flip() +
       ggplot2::geom_text(ggplot2::aes(x = group, y = 0, label = label), hjust = "inward") +
       ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expand_scale(mult = c(0, 0))) +
       ggplot2::theme_minimal() +
       ggplot2::ylab(NULL) +
       ggplot2::xlab(NULL) +
       ggplot2::labs(subtitle = subtitle) +
       ggplot2::theme(# Blank out the grid.
										  axis.text.x = ggplot2::element_blank(),
										  axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                    	plot.subtitle = ggplot2::element_text(hjust = 0))
			if (!inculude_group_labels) {
				result <- result + ggplot2::theme(# Blank out the axis tick labels.
                      										axis.text.y = ggplot2::element_blank())
			}
			result
   }

   # Define labelling functions to create text labels for ORs and CIs.
   n_label_f <- function(x) {
		 dplyr::mutate(x, label = sprintf("%s / %s", sample_n, sample_n + sample_k))
   }
   or_label_f <- function(x) {
   	 result <- dplyr::mutate(x, label = sprintf("%s (%s to %s)", or %>% signif(2),
   	 																														 ci_lower %>% signif(2),
   	 																														 ci_upper %>% signif(2)))
	 	 result[result$group == reference_group, "label"] <- "1"
	 	 result
   }
   p_label_f <- function(x) {
   	 dplyr::mutate(x, label = sprintf("%s%% (%s%% to %s%%)", 100 * corresponding_prob %>% signif(2),
																														 100 * corresponding_prob_lower %>% signif(2),
																														 100 * corresponding_prob_upper %>% signif(2)))
   }

   # Make the table plots.
   n_table <- table_plot(label_f = n_label_f, subtitle = "Sample", inculude_group_labels = TRUE)
   or_table <- table_plot(label_f = or_label_f, subtitle = "Odds Ratio (95% CI)")
   p_table <- table_plot(label_f = p_label_f, subtitle = "Probability (95% CI)")

	 # Join up and return the plots.
	 patchwork::wrap_plots(n_table, or_forest_plot, or_table, prob_plot, p_table,
	 											 widths = c(0.47, 1, 1, 1, 1), ncol = 5)
}
