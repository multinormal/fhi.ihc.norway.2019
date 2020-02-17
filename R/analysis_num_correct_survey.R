#' Fit a GLM to a combination of the Uganda trial and Norwegian survey data
#'
#' The cluster structure is modelled using the method provided by the \code{survey}
#' package, not using random effects (which does not appear to be possible at this
#' time in R if post-stratified weights are to be used).
#'
#' @param data an imputed data set with columns as named in the formula, plus
#'   \code{country} and \code{school} columns identifying the cluster each individual
#'   belongs to.
#' @param formula a formula specifying the model structure.
#' @param family an object that specifies the error family (and link function).
#' @return an object of class \code{svyglm}.
fit_survey_glm <- function(data, formula, family) {
	design <- survey::svydesign(ids = ~country + school, probs = ~probs, data = data)
	survey::svyglm(formula = formula, design = design, family = family)
}

#' Return the *i*-th completed (imputed) data set
#'
#' The function ensures that any factor variable is converted to a character variable,
#' permitting the various data sets to be joined without errors about differing types. We
#' add a probs column which by default is all set to 1; for the Norway data, this should
#' be rewritten manually.
#'
#' @param data a value as returned by \code{mice}.
#' @param i the index to the completed data set required.
#' @return a completed data set.
#' @importFrom magrittr "%>%"
complete <- function(data, i) {
	mice::complete(data = data, i) %>%
		dplyr::mutate_if(is.factor, as.character) %>%
		dplyr::mutate(probs = 1)
}

#' Set up an imputation for a given data set
#'
#' The function ensures that any factor variable is converted to a character variable,
#' permitting the various data sets to be joined without errors about differing types. We
#' add a probs column which by default is all set to 1; for the Norway data, this should
#' be rewritten manually.
#'
#' @param x a data frame with missing data.
#' @param exclude_cluster if \code{TRUE}, exclude cluster variables from imputation.
#' @return an S3 object of class \code{mids}.
#' @importFrom magrittr "%>%"
num_correct_impute <- function(x, exclude_cluster = TRUE) {
	x <- x %>%
		# To allow multiple imputation to be used to address missing values of the
		# poststratification variables, set explicit missing levels to NA.
		dplyr::mutate_if(is.factor, forcats::fct_recode, NULL = "Missing")

	# Initialize MICE for each of the incomplete data sets.
	init <- suppressWarnings(mice::mice(x, maxit = 0))

	# Make a predictor matrix that excludes variables that should not be use in MICE.
	cols_to_remove <- c("NR")
	if (exclude_cluster) cols_to_remove <- c(cols_to_remove, "country", "school")
	predictorMatrix <- init$predictorMatrix
	cols_to_remove <- intersect(cols_to_remove, colnames(predictorMatrix))
	predictorMatrix[, cols_to_remove] <- 0
	mice::mice(x, seed = 1234, method = init$method,
						predictorMatrix = predictorMatrix,
						printFlag = FALSE)
}

#' Perform an analysis for an i-th completed data set.
#'
#' @param i an index into one of the completed (imputed) data sets.
#' @param analysis a function of a completed data set.
#' @param uganda_teachers_data data for Ugandan teachers (note that this data is complete).
#' @param uganda_children_imputed the result of calling \code{num_correct_impute} on
#'   \code{uganda_children_data}.
#' @param uganda_parents_imputed the result of calling \code{num_correct_impute} on
#'   \code{uganda_parents_data}.
#' @param norway_imputed the result of calling \code{num_correct_impute} on
#'   \code{norwegian_num_correct()}.
#' @return this function returns whatever the \code{analysis} function returns.
#' @importFrom magrittr "%>%"
num_correct_impute_and_estimate <- function(i, analysis,
																						uganda_teachers_data,
																						uganda_children_imputed,
																						uganda_parents_imputed,
																						norway_imputed) {
	# Obtain the i-th completed data sets.
	uganda_children_completed <- complete(uganda_children_imputed, i)
	uganda_parents_completed  <- complete(uganda_parents_imputed, i)
	norway_completed          <- complete(norway_imputed, i)

	# Obtain post-stratified probabilities (weights) for the completed Norwegian data.
	norway_completed$probs <- weighted_design(norway_completed)$prob

	# Only look at the data for the quiz with the same concepts as in Uganda ...
	norway_completed <- norway_completed %>%
		dplyr::filter(quiz == 1) %>%
		# ... and drop the quiz column.
		dplyr::select(-quiz)

	# Make the completed data set.
	completed <- dplyr::bind_rows(
		uganda_teachers_data,
		uganda_children_completed,
		uganda_parents_completed,
		norway_completed) %>%
		# Add a column for proportion of questions correct.
		dplyr::mutate(proportion_correct = num_correct / num_total)
	stopifnot(completed[,-which(names(completed) == "NUTS2")] %>% complete.cases() %>% all())

	# Perform the analyses.
	analysis(completed)
}

#' Compute quantities to support the reporting of a GLM.
#'
#' @param family the error family to use for the GLM.
#' @param formula the formula that specifies the GML analysis.
#' @return a data frame with columns to describe the sample. Note that for analyses using
#'   the quasibinomial error family, the columns \code{sample_n} and \code{sample_k} gives
#'   the number of "successes" and "failures", such that their ration gives the same odds.
#' @importFrom magrittr "%>%"
#' @export
num_correct_sample_analysis <- function(formula, family) {
	sample_data <- suppressWarnings(dplyr::bind_rows(
		uganda_teachers_data,
		uganda_children_data,
		uganda_parents_data,
		norwegian_num_correct())) %>%
		# Add a column for proportion of questions correct.
		dplyr::mutate(proportion_correct = num_correct / num_total)

	# Get the sames of the LHS and RHS variables.
	dep_var <- all.vars(formula)[1]
	ind_var <- all.vars(formula)[2]

	if (family$family == "gaussian") {
		result <- sample_data %>%
			dplyr::group_by(!!rlang::sym(ind_var)) %>%
			dplyr::summarize(n = n()) %>%
			dplyr::ungroup()
	} else {
		stopifnot(family$family %in% c("quasibinomial", "binomial"))

		result <- sample_data %>%
			dplyr::group_by(!!rlang::sym(ind_var)) %>%
			dplyr::summarize(sample_prob = mean(!!rlang::sym(dep_var)),
											 sample_n = sum(!!rlang::sym(dep_var)),
											 sample_k = length(!!rlang::sym(dep_var)) - sum(!!rlang::sym(dep_var))) %>%
			dplyr::ungroup() %>%
			dplyr::mutate(sample_odds = sample_n / sample_k)
	}

	result
}

#' Perform an analysis of the number of correct answers of Ugandans and Norwegians.
#'
#' Multiple imputation is used to complete missing values of poststratification variables.
#' Post-stratification is used to address possible nonrandom nonresponse of Norwegians.
#' Clustering is modelled using the method provided by the \code{survey} package rather
#' than using random effects (see \code{link{fit_survey_glm}}).
#'
#' @param formula the formula that specifies the GML analysis.
#' @param family the error family to use for the GLM.
#' @return a named list of results, which are each also given the class
#'   \code{num_correct_analysis}.
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
num_correct_survey_analysis <- function(formula, family) {
	stop("This function has been superceded by num_correct_glmm_analysis")
	
  # Verify that the Uganda teachers data is complete, and convert any factor variables
  # to character variables, permitting the various data sets to be joined without errors
  # about differing types.
	stopifnot(uganda_teachers_data %>% complete.cases() %>% all())
	uganda_teachers_data %<>%
		dplyr::mutate_if(is.factor, as.character) %>%
		dplyr::mutate(probs = 1)

	# Perform MICE on each data set.
	uganda_children_imputed <- num_correct_impute(uganda_children_data, exclude_cluster = FALSE)
	uganda_parents_imputed  <- num_correct_impute(uganda_parents_data)
	norway_imputed          <- num_correct_impute(norwegian_num_correct())

	# Define a function to impute and estimate.
	impute_and_estimate <- function(i, analysis)
		num_correct_impute_and_estimate(i, analysis,
																		uganda_teachers_data = uganda_teachers_data,
																		uganda_children_imputed = uganda_children_imputed,
																		uganda_parents_imputed = uganda_parents_imputed,
																		norway_imputed = norway_imputed)

	# Use multiple imputation to perform the specified analysis. Note that we use the mice
	# package for the imputation, and can then just use mitools' MIcombine to combine the
	# list of imputed estimates.
	n_imputations <- uganda_children_imputed$m # Should be the same for all "imputed".
	analysis_result <- 1:n_imputations %>%
		purrr::map(function(i) {
								impute_and_estimate(i = i,
																		analysis = function(x)
																							   fit_survey_glm(x, formula, family))}) %>%
		mitools::MIcombine()

	sample_result <- num_correct_sample_analysis(formula, family)
	result <- list(analysis_result = analysis_result, sample_result = sample_result)

	# Set a class for the result, where the class name derives from the family used.
	class(result) <- c(paste0("nca_", family$family), class(result))

	# Return the result.
	result
}
