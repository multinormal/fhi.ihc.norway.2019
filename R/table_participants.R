#' Helper function for making rows of table of participants
#' @importFrom magrittr "%>%"
participant_table_row <- function(data, filter_f) {
	# Define a function to get the number of participants matching the filter
	# for a given quiz level x.
	n_for <- function(x) data %>% dplyr::filter(quiz == x) %>% filter_f %>% nrow()
	
	# Define a function to get the number of participants for a given quiz level x.
	total_for <- function(x) data %>% dplyr::filter(quiz == x) %>% nrow()

	# Compute the counts, totals and proportions across the quizzes.
	quiz_levels <- data$quiz %>% levels()
	counts <- quiz_levels %>% purrr::map_int(n_for)
	totals <- quiz_levels %>% purrr::map_int(total_for)
	props <- counts / nrow(data) # Proportions of the entire sample, not each quiz.
	
	# Test the null of equal proportions across the quizzes.
	# We use Fisher's exact test because the chi-square approximation falls down for some
	# levels of some variables due to small event counts.
	test_result <- matrix(c(counts, totals), ncol = 2) %>% t() %>% fisher.test()
	p_value <- test_result$p.value %>% format.pval(eps = 0.0001, scientific = FALSE)
	
	strings <- sprintf("%d (%.1f%%)", counts, 100 * props)
	overall <- sprintf("%d (%.1f%%)", sum(counts), 100 * (sum(counts) / nrow(data)))
	tibble::tibble(quiz = quiz_levels, strings = strings, `P-value` = p_value,
								 Overall = overall) %>%
		tidyr::spread(quiz, strings)
}

#' Helper function for making a section of a table of participants
#' @importFrom magrittr "%>%"
participant_table_section <- function(data, column) {
	row_for_section <- function(this_level) {
		this_filter <- function(x) x[which(x[, column] == this_level),]
		data %>%
			participant_table_row(this_filter) %>%
			dplyr::mutate(variable = column, level = this_level) %>%
			dplyr::select(variable, level, dplyr::everything()) %>%
			dplyr::select(-Overall, -`P-value`, dplyr::everything())
	}

	data[, column] %>% dplyr::pull(column) %>% levels() %>% purrr::map_dfr(row_for_section)
}

#' Helper that changes a value to a space if the preceding value is the same
repeated_to_blank <- function(x) {
	prev_value <- x[1]
	for (i in 2:length(x)) if (x[i] == prev_value) x[i] <- "" else prev_value <- x[i]
	x
}

#' Make a table of participant characteristics
#'
#' @return A \code{tibble} with the table of participant characteristics.
#' @importFrom magrittr "%>%"
#' @export
table_participants <- function() {
	make_section <- function(x) quiz_data_wide() %>% participant_table_section(x)
	# First make a row for all participants.
	result <- participant_table_row(quiz_data_wide(), function(x) x) %>%
		dplyr::mutate(variable = "All", level = "") %>%
		dplyr::select(variable, level, dplyr::everything()) %>%
		dplyr::select(-Overall, -`P-value`, dplyr::everything()) %>%
		# Now add sections for each variable.
		rbind(participant_characteristics_variables %>% names() %>% purrr::map_dfr(make_section))
	result$variable <- repeated_to_blank(result$variable)
	result %>% dplyr::rename(` ` = variable, `  ` = level)
}
