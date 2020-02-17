#' Summarize a \code{nca_quasibinomial} object as a table
#'
#' @param object an object of class \code{nca_quasibinomial}.
#' @param scale_to_sample_odds if \code{TRUE}, scale the estimated odds ratios to match
#'   the sample odds. This is useful to address what appears to be a numerical issue (see
#'   comments in the source for this function for details).
#' @return A \code{tibble} (table) of results.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
summary.nca_quasibinomial <- function(object,
																			reference_group = groups$norwegians,
																			scale_to_sample_odds = TRUE) {
	analysis_result <- object$analysis_result
	sample_result <- object$sample_result

	# Function to return an odds ratio for a given group relative to the reference group
	# (intercept). The result is returned on the OR scale. If the group is the
	# intercept group, return NA.
	odds_ratio <- function(group) coef(analysis_result)[group] %>% exp()

	# Function to return a CI bound for a given group. The result is returned on the OR
	# scale. If the group is the intercept group, return NA.
	odds_ratio_ci <- function(group, level) confint(analysis_result)[group, level] %>% exp()

	# The svyglm function does not appear to provide P-values, so define a function to
	# approximate a P-value using Z-statistics using the SE and effect size.
	p_value <- function(x, se) 2 * (1 - pnorm(abs(x), mean = 0, sd = se))

	# Iterate over each group ...
	analysis_tibble <- names(coef(analysis_result)) %>%
		purrr::map( # ... make a 1-row tibble with the groups, mean score, and CIs ...
			function(group) {
				tibble::tibble(
					group = group,
					or = odds_ratio(group),
					ci_lower = odds_ratio_ci(group, "2.5 %"),
					ci_upper = odds_ratio_ci(group, "97.5 %")
					)
			}) %>%
		dplyr::bind_rows() %>% # ... bind the tibbles ...
		dplyr::mutate(
			group = dplyr::if_else(group == "(Intercept)", # ... rename the intercept level ...
														 reference_group,
														 substring(group, nchar("Uganda"))),
			# Manually calculate the standard errors (going via summry() prints the summary!).
			se = object$analysis_result %>% vcov() %>% diag() %>% sqrt() %>% unname())

	# Obtain the (sample) odds and probability for the reference group.
	ref_prob <- object$sample_result[object$sample_result$group == reference_group,]["sample_prob"] %>% as.numeric()
	ref_odds <- odds_ratio("(Intercept)")

	# In (at least) one of the analyses, the estimate of the odds for the reference group
	# (Norwegians) was overestimated relative to the sample odds, and the ORs for the
	# comparisons to the other groups were correspondingly low (i.e., the product of the
	# estimated odds for the reference group and an OR gives a good estimate of the odds
	# for the comparator group, but the odds for the reference is wrong). A brief search
	# suggests that this can occur for various reasons, one of which is that all or almost
	# all of a particular group are "successes". To address what looks like a numerical issue,
	# I am simply going to manually scale the estimated odds for the reference group to match
	# the sample odds, and scale the ORs similarly. Basically, this just shunts all the
	# (unexponentiated) estimated coefficients down the number line, preserving the distances
	# between them, so that the intercept term agrees with the sample log odds. I assume that
	# the standard errors are correct.
	ref_odds_sample <- object$sample_result[object$sample_result$group == reference_group,]["sample_odds"] %>% as.numeric()
	scalar <- ref_odds / ref_odds_sample
	if (!scale_to_sample_odds) scalar <- 1

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
									prob_diff = corresponding_prob - ref_prob,
									prob_diff_lower = corresponding_prob_lower - ref_prob,
									prob_diff_upper = corresponding_prob_upper - ref_prob) %>%
		# Check that the sample odds are contained in the confidence intervals for the odds.
		(function(x) {
			stopifnot(all(x$corresponding_odds_lower[-1] <= x$sample_odds[-1]))
			stopifnot(all(x$sample_odds[-1] <= x$corresponding_odds_upper[-1]))
			x
		}) %>%
		# Now convert those into a readable table.
		dplyr::transmute(` ` = group,
										 `Sample` = sprintf("%i/%i", sample_n, sample_n + sample_k),
										 `  ` =  sprintf("(%.1f%%)", 100 * sample_prob),
										 `Odds ratio (95% CI)` = sprintf("%s (%s to %s)",
										 																 scalar * or %>% signif(2),
										 																 scalar * ci_lower %>% signif(2),
										 																 scalar * ci_upper %>% signif(2)),
										 `Difference (95% CI)` = sprintf("%s%% (%s%% to %s%%)",
										 																 100 * prob_diff %>% signif(2),
										 																 100 * prob_diff_lower %>% signif(2),
										 																 100 * prob_diff_upper %>% signif(2)),
										 `P value` = p_value(x = log(or), se = se) %>% format.pval(eps = 0.0001, scientific = FALSE)
		) %>%
		(function(x) { # ... blank out elements we do not want ...
			x[1, 4:6] <- ""
			x
		})
}
