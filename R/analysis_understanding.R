#' Test if a participant understands a concept
#'
#' @param data a data frame of class \code{quiz_long} containing data for a single
#'   participant. Specifically, it is expected that this has been generated within a
#'   \code{group_by}-\code{ungroup} construct, and the function called using
#'   \code{dplyr::summarize}.
#'
#' @return 1 if the participant understands a concept or 0 otherwise.
#' @importFrom magrittr "%>%"
understands_concept <- function(data) {
	stopifnot((data$NR %>% unique() %>% length()) == 1)
	stopifnot((data$key_concept %>% unique() %>% length()) == 1)

	# Some concepts need special-casing (e.g., they are not supported by two questions).
	special_cases <- c(concepts$more_not_better, attitudes)
	key_concept <- data$key_concept %>% unique()
	if (key_concept %in% special_cases) {
		dplyr::if_else(data$correct == 1, 1, 0)
	} else {
		floor(sum(data$correct) / 2)
	}
}

#' Obtain data on a single concept
#'
#' @param concept the concept of interest (e.g., "Newer is not necessarily better").
#' @param data a data frame of class \code{quiz_long} containing the data (by default,
#'   \code{quiz_data_long} is used).
#'
#' @return a data frame with one row per participant on the specified concept, and in
#'   particular a factor \code{concept_correct} that encodes whether participants
#'   understand the concept. Defined columns will be renamed to remove spaces from their
#'   names (it is assumed that objects of class \code{quiz_long} have these variables).
#' @importFrom magrittr "%>%"
#' @export
data_for_concept <- function(concept, data = quiz_data_long()) {
	stopifnot("quiz_long" %in% class(data))
	# Define the variables from data that should be included in the data set that we will
	# analyze, and which define a unique participant.
	weighting_variables <- c(Sex = "Sex", ISCED_Level = "ISCED_Level", NUTS2 = "NUTS2")
	variables <- c(NR = "NR",
								 Research_training = "Research training",
								 Research_participant = "Research participant",
								 Medical_education = "Medical education",
								 weighting_variables)

	# Determine the number of rows we expect in the data set that we will analyze.
	n_expected_rows <- data %>%
		dplyr::filter(key_concept == concept) %>% {.$NR} %>% unique() %>% length()

	# Define a function to ensure that the concept_correct column of a data frame has both
	# levels (even if there are no instances of one of the levels).
	ensure_levels <- function(x) {
		levels(x$concept_correct) <- c("Misunderstand", "Understand")
		x
	}

	# Modify the data such that it assesses whether each individual understands the concept
	# (i.e., answers both questions for the concept correctly).
	data %>%
		# Obtain data for the concept of interest ...
		dplyr::filter(key_concept == concept) %>%
		# ... look at each individual ...
		dplyr::group_by(NR) %>%
		# ... determine if they understand the concept ...
		dplyr::summarize(concept_correct = understands_concept(.data)) %>%
		dplyr::ungroup() %>%
		# ... and merge with the unique rows of the original data frame for selected columns.
		merge(data[variables %>% unname()] %>% unique(), by = "NR") %>%
		dplyr::rename(!!variables) %>%
		# Perform some sanity-checking.
		{
			stopifnot(nrow(.) == length(unique(.$NR))) # Should have one participant per row.
			stopifnot(nrow(.) == n_expected_rows) # Should have the expected number of participants.
			stopifnot(!any(is.na(.$concept_correct))) # There should be no NAs in this column.
			stopifnot(all(.$concept_correct %in% c(0, 1))) # Only 0s & 1s allowed in this column.
			.
		} %>%
		# Recode the concept_correct column as a factor.
		dplyr::mutate(concept_correct = forcats::fct_recode(concept_correct %>% as.factor(),
																												Understand = "1",
																												Misunderstand = "0")) %>%
		dplyr::mutate(key_concept = concept) %>%
		# Ensure that the concept_correct factor has both levels.
		ensure_levels() %>%
		# To allow multiple imputation to be used to address missing values of the
		# poststratification variables, set explicit missing levels to NA.
		dplyr::mutate_at(colnames(.), forcats::fct_recode, NULL = "Missing")
}

#' Compute the probability that a participant would understand a concept by guessing
#'
#' All possible responses to questions are considered, including "Don't know" etc.
#'
#' @param concept the concept of interest (e.g., "Newer is not necessarily better").
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#'
#' @return an estimate of the proportion of Norwegians who would be expected to
#'   "understand" a concept if they made random guesses.
#' @importFrom magrittr "%>%"
#' @export
expected_probability <- function(concept, data = quiz_data_long()) {
	data <- data %>% dplyr::filter(Answer != "Missing")

	probabilities <- c()
	data_for_concept <- data %>% dplyr::filter(key_concept == concept)
	unique_quizzes <- data_for_concept$quiz %>% unique()
	for (this_quiz in unique_quizzes) {
		probability <- 1
		data_for_quiz <- data_for_concept %>% dplyr::filter(quiz == this_quiz)
		unique_questions <- data_for_quiz$Question %>% unique()
		for (this_question in unique_questions) {
			data_for_question <- data_for_quiz %>% dplyr::filter(Question == this_question)
			number_options <- data_for_question$Answer %>% unique() %>% length()
			probability <- probability / number_options
		}
		probabilities <- c(probabilities, probability)
	}

	mean(probabilities)
}

#' Estimate (with/without adjustment) the proportion of Norwegians who understand a concept
#'
#' This function estimates a design-weighted mean (proportion) of respondents who
#' understand a given concept, or performs logistic regression to adjust for
#' covariates of interest and hence estimate their role in understanding the concept.
#'
#' Multiple imputation is used to complete missing values of poststratification variables.
#' It is correct to perform multiple imputation for each call of this function, because
#' the full data set contains multiple instances of the same participant (and hence
#' the same pattern of missingness).
#'
#' @param concept the concept of interest (e.g., "Newer is not necessarily better").
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#' @param regress if \code{FALSE} (the default), estimate the proportion of
#'   Norwegians who understand the concept. If \code{TRUE}, use logistic
#'   regression to model understanding the concept in terms of covariates sex,
#'   research training, research participation, education, and medical education.
#'
#' @return an estimate of the proportion of Norwegians who understand the concept. The
#'   returned object will have an attribute, \code{raw_estimate}, with the raw estimate
#'   (i.e., without poststratification).
#' @importFrom magrittr "%>%"
#' @export
proportion_understand <- function(concept, data = quiz_data_long(),
                                  regress = FALSE) {
  # Estimate the proportion we would expect from random guesses.
  expected_probability <- expected_probability(concept = concept, data = data)

  # Obtain the data for the concept.
	data <- data_for_concept(concept = concept, data = data)
	stopifnot((!is.na(data$concept_correct)) %>% all())

	# Compute the raw number, total, and a raw estimate of the proportion.
	number_correct <- (data$concept_correct == "Understand") %>% sum()
	total <- nrow(data)
	raw_estimate <- (data$concept_correct == "Understand") %>% mean()

	# Now use multiple imputation and poststratification.
	init <- suppressWarnings(mice::mice(data, maxit = 0))
	method <- init$method
	predictorMatrix = init$predictorMatrix
	predictorMatrix[, c("NR")] = 0 # These should not be used in MI.
	imputed <- mice::mice(data, seed = 1234, method = method, predictorMatrix = predictorMatrix, printFlag = FALSE)

	# Define a function that, given an index into the multiple imputations, returns
	# an object that estimates the proportion of interest.
	estimate_proportion <- function(i) {
		# Make the completed data for the i-th imputation
		completed <- mice::complete(imputed, i)
		
		# Perform estimation.
		if (regress) {
			# Obtain a weighted survey design for the completed data, dropping the
			# ordering of the ISCED_Level variable because I'm not convinced that
			# proportional odds is a sensible assumption, and that treating this as
			# a simple factor makes the reporting easier.
			completed$ISCED_Level <- factor(completed$ISCED_Level, ordered = FALSE)
  		design <- weighted_design(completed)
		
		  # Perform regression to estimate the effect of the covariates.
		  model <- concept_correct ~ Sex + Research_training + 
		                             Research_participant + ISCED_Level +
		                             Medical_education
		
		  survey::svyglm(formula = model, design = design,
		                 family = stats::quasibinomial(link = "logit"))
		} else {
  		# Obtain a weighted survey design for the completed data.
  		design <- weighted_design(completed)
		
		  # Compute the mean for this completed data.
		  survey::svymean(~concept_correct, design = design)
		}
	}

	# Use multiple imputation (and poststratification) to estimate the proportion who
	# understand the concept. Note that we use the mice package for the imputation, and
	# can then just use mitools' MIcombine to combine the list of imputed estimates.
	result <- 1:imputed$m %>% purrr::map(estimate_proportion) %>% mitools::MIcombine()

	# Attach number correct, the total, the raw percent, and the expected probability as attributes.
	attr(result, "number_correct") <- number_correct
	attr(result, "total") <- total
	attr(result, "raw_estimate") <- raw_estimate
	attr(result, "expected_probability") <- expected_probability
	attr(result, "regress") <- regress

	# Return the result.
	result
}

#' Make a one-row tibble for the proportion of Norwegians who understand a given concept
#'
#' If the concept is one of the four included in all questionnaires, the confidence
#' interval will be Bonferroni-corrected.
#'
#' @param concept the concept (e.g., a member of \code{concepts})
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#'
#' @return a one-row \code{tibble} that summarizes the proportion of Norwegians who
#'   understand the concept. The \code{tibble} will contain a column that gives the
#'   half-width of the confidence interval, which may be useful for comparing with the
#'   original power analysis.
#' @importFrom magrittr "%>%"
#' @export
proportion_concept_row <- function(concept, data = quiz_data_long()) {
	# Determine the appropriate level to use for the confidence interval.
	if (concept %in% common_concepts) {
		level <- (1 - (0.05 / length(common_concepts)))
		double_dagger <- "\U2021"
		marker <- double_dagger
	} else {
		level <- 0.95
		marker <- ""
	}

	# Make the tibble with the result.
	estimate <- proportion_understand(concept = concept, data = data)
	stopifnot(attr(estimate, "regress") == FALSE)
	tibble::tibble(
		concept = concept %>% paste0(marker),
		number_correct = attr(estimate, "number_correct"),
		total = attr(estimate, "total"),
		raw = attr(estimate, "raw_estimate"),
		proportion = estimate$coefficients[["concept_correctUnderstand"]],
		ci_lower = confint(estimate, level = level)["concept_correctUnderstand",][1] %>% unname(),
		ci_upper = confint(estimate, level = level)["concept_correctUnderstand",][2] %>% unname(),
		expected_probability = attr(estimate, "expected_probability"),
		better_than_random = expected_probability < proportion,
		no_better_than_random = ci_lower < expected_probability) %>%
	dplyr::mutate(half_width = (ci_upper - ci_lower) / 2)
}

#' Make a one-row tibble for a regression analysis of Norwegians who understand a given concept
#'
#' @param concept the concept (e.g., a member of \code{concepts})
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#'
#' @return a one-row \code{tibble} that summarizes the odds ration estimated by the regression.
#' @importFrom magrittr "%>%"
#' @export
regression_concept_row <- function(concept, data = quiz_data_long()) {
	# Make the tibble with the result.
	estimate <- proportion_understand(concept = concept, data = data, regress = TRUE)
	stopifnot(attr(estimate, "regress") == TRUE)
	row <- tibble::tibble(
		concept = concept,
		number_correct = attr(estimate, "number_correct"),
		total = attr(estimate, "total"),
		raw = attr(estimate, "raw_estimate"))
	
	# Iterate over the coefficients in the model and add columns for the point
	# estimate and the confidence interval limits.
	coef_names <- estimate$coefficients %>% names()
	for (name in coef_names) {
	  row[1, name] <- estimate$coefficients[[name]]
	  row[1, paste0(name, "_lower")] <- confint(estimate)[name,][1] %>% unname()
	  row[1, paste0(name, "_upper")] <- confint(estimate)[name,][2] %>% unname()
	  stopifnot()
	}
	row
}

#' Estimate the proportion of Norwegians who understand each of the concepts
#'
#' Multiple imputation is used to complete missing values of poststratification variables.
#'
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#' @param attitudes_intentions if \code{FALSE} (the default), only questions that probe
#'   understanding the key concepts will be included, otherwise only the questions that
#'   probe attitudes will be included.
#'
#' @return a \code{tibble} of class \code{prop_df} that summarizes the proportion of
#'   Norwegians who understand the concepts. The \code{tibble} will contain a column that
#'   gives the half-width of the confidence interval, which may be useful for comparing
#'   with the original power analysis.
#' @importFrom magrittr "%>%"
#' @export
proportion_understand_all_concepts <- function(data = quiz_data_long(),
																							 attitudes_intentions = FALSE) {
	stopifnot(is.logical(attitudes_intentions))
	kc_filter <- if (attitudes_intentions) {
		function(x) x %>% dplyr::filter(key_concept %in% attitudes)
	} else {
		function(x) x %>% dplyr::filter(key_concept %in% concepts)
	}

	concept_row <- function(concept) proportion_concept_row(concept = concept, data = data)
	result <- data %>%
		# Exclude the questions on attitudes or key concepts, as appropriate.
		kc_filter() %>%
		# Apply an ordering to the concepts.
		dplyr::mutate(key_concept = forcats::fct_relevel(key_concept, concepts %>% unlist() %>% unname())) %>%
		# Estimate the proportions for the remaining concepts.
		{.$key_concept} %>% unique() %>%
		purrr::map_df(concept_row)
	class(result) <- c("prop_df", class(result))
	attr(result, "attitudes_intentions") <- attitudes_intentions
	result
}

#' Exploratory regression analysis of Norwegians understanding of each of the concepts
#'
#' This function estimates log-ORs for an intercept term and each covariate
#' included in the model.
#'
#' Multiple imputation is used to complete missing values of poststratification variables.
#'
#' @param data a data frame in long format containing the data (by default,
#'   \code{quiz_data_long}) is used.
#' @param attitudes_intentions if \code{FALSE} (the default), only questions that probe
#'   understanding the key concepts will be included, otherwise only the questions that
#'   probe attitudes will be included.
#'
#' @return a \code{tibble} of class \code{logit_df} that summarizes the results of
#'   the analysis.
#' @importFrom magrittr "%>%"
#' @export
regression_all_concepts <- function(data = quiz_data_long(),
																		attitudes_intentions = FALSE) {
	stopifnot(is.logical(attitudes_intentions))
	kc_filter <- if (attitudes_intentions) {
		function(x) x %>% dplyr::filter(key_concept %in% attitudes)
	} else {
		function(x) x %>% dplyr::filter(key_concept %in% concepts)
	}

	concept_row <- function(concept) regression_concept_row(concept = concept, data = data)
	result <- data %>%
		# Exclude the questions on attitudes or key concepts, as appropriate.
		kc_filter() %>%
		# Apply an ordering to the concepts.
		dplyr::mutate(key_concept = forcats::fct_relevel(key_concept, concepts %>% unlist() %>% unname())) %>%
		# Perform regression for the remaining concepts.
		{.$key_concept} %>% unique() %>%
		purrr::map_df(concept_row)
	class(result) <- c("logit_df", class(result))
	attr(result, "attitudes_intentions") <- attitudes_intentions
	result
}
