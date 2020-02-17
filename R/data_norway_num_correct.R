#' Data on number of questions answered correctly for Norwegian adults
#'
#' The Norwegians were randomly sampled and there is no hierarchical grouping;
#' \code{school} is set to a unique value for each individual.
#'
#' Because only quiz 1 was the same as in the Uganda trials, only data for that quiz is
#' returned. Responses on the questions that probe understanding of association and
#' causation are dropped from the returned data set.
#'
#' The columns \code{passing} and \code{mastery} use the previously-published definitions,
#' but are only meaningful with respect to quiz 1.
#'
#' @return a \code{tibble} where each row is a Norwegian adult, and the columns are the
#'   same as for the data sets \code{uganda_children_data}, \code{uganda_parents_data},
#'   and \code{uganda_teachers_data}.
#'
#' @importFrom magrittr "%>%"
#' @include names_groups.R
#' @export
norwegian_num_correct <- function() {
	quiz_data_long() %>%
    # Remove questions for a concept excluded in previous published analysis ...
    dplyr::filter(key_concept != concepts$association_causation) %>%
    # ... and include data only for quiz 1.
    dplyr::filter(quiz == 1) %>%
    # Assess the performance of each individual.
		dplyr::group_by(NR, Sex, ISCED_Level, NUTS2, `Research training`, `Research participant`, `Medical education`) %>%
		dplyr::summarize(num_correct = sum(correct),
									   num_total = n(),
										 passing = num_correct >= 13,
										 mastery = num_correct >= 20) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(NR = paste0("NA-", NR),
									country = "Norway", # See function documentation.
									school = NR, # Unique "school" for each person.
									group = groups$norwegians)
}
