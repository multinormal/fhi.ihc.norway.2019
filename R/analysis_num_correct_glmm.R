#' Perform a GLMM analysis of the number of correct answers of Ugandans and Norwegians.
#'
#' @param formula the formula that specifies the GML analysis.
#' @param family the error family to use for the GLM.
#' @param reference_group the name of the reference group.
#' @return a named list of results, of class \code{ncg_gaussian} or \code{ncg_binomial}.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
num_correct_glmm_analysis <- function(formula, family,
									reference_group = groups$intervention_children) {
	# Ensure a supported error family has been specified.
	stopifnot(family$family %in% c("gaussian", "binomial"))

	# Make a single data set containing data for the Norwegians and the Ugandans.
	combined_data <- suppressWarnings(dplyr::bind_rows(
		uganda_teachers_data,
		uganda_children_data,
		uganda_parents_data,
		norwegian_num_correct())) %>%
		# Convert any missing values coded with a dedicated factor level as NA.
    dplyr::mutate_if(is.factor, forcats::fct_recode, NULL = "Missing") %>%
		dplyr::mutate(# Add a column for proportion of questions correct.
									proportion_correct = num_correct / num_total,
									# Make the Ugandan children the reference level, and place the
									# Norwegians last.
									group = forcats::fct_relevel(group, reference_group),
									group = forcats::fct_relevel(group, groups$norwegians, after = Inf))
	
	# Check that when we look at the columns named in the formula, the data are complete.
	stopifnot(combined_data[, all.vars(formula)] %>% complete.cases() %>% all())
	
	# Fit the model and obtain the sample result.
	f <- function(x) if(family$family == "gaussian")
		lme4::lmer(formula = x, data = combined_data)
	else
		lme4::glmer(formula = x, data = combined_data, family = family,
								control = lme4::glmerControl(optimizer="bobyqa"))
	analysis_result <- f(formula)
	sample_result <- num_correct_sample_analysis(formula, family)
	
	# Make and return a list whose class identifies the type of analysis performed.
	result <- list(analysis_result = analysis_result, sample_result = sample_result)
	class(result) <- c(paste0("ncg_", family$family), class(result))
	attr(result, "reference_group") <- reference_group
	result
}