#' Correct answers for Ugandan children
uganda_children_correct_answers <- list(
  qn4 = "A",
  qn5 = "B",
  qn6 = "A",
  qn7 = "A",
  qn8 = "C",
  qn9 = "B",
  qn10 = "B",
  qn11 = "B",
  qn12 = "A",
  qn14 = "A",
  qn15 = "B",
  qn16 = "A",
  qn17 = "A",
  qn18 = "A",
  qn20 = "A",
  qn21part1 = "2",  # Disagree
  qn21part2 = "2",  # Disagree
  qn21part3 = "1",  # Agree
  qn21part4 = "1",  # Agree
  qn22part1 = "1",  # More sure
  qn22part2 = "1",  # More sure
  qn22part3 = "2",  # Less sure
  qn23part1 = "2",  # Disagree
  qn23part2 = "1"   # Agree
)


#' Data for Ugandan children (one-year follow-up)
#' 
#' The \code{ISCED_Level} column codes educational attainment using the same ranges of ISCED 2011
#' levels used for the Norwegian population. In this study, schools were the unit of
#' randomization, and a \code{school} column is provided that records which school each
#' student attended. Columns \code{passing} and \code{mastery} are use the
#' previously-published definitions.
#' @include names_groups.R
#' @export
uganda_children_data <- {
	correct_answers <- uganda_children_correct_answers
	filename <- "uganda-children-one-year-follow-up.csv"
	
	# Read and process the data.
	system.file("extdata", filename, package = "fhi.ihc.norway.2019") %>%
		readr::read_delim(delim = ";") %>%
		# Rename columns to be more readable/consistent with the survey data set.
		dplyr::rename(NR = record,
		              school = qn0part2,
									group = qn0part5,
									Sex = qn1part2) %>%
		# Rename values to be more readable/consistent with the survey data set.
		dplyr::mutate(
			# Make the NR column unique to this data set.
			NR = paste0("UC-", NR),
			# Specify the country.
			country = "Uganda",
			# Treatment assignment.
			group = replace(group, group == 1, groups$intervention_children),
			group = replace(group, group == 2, groups$control_children),
			# Sex.
			Sex = replace(Sex, Sex == 0, NA),
			Sex = replace(Sex, Sex == 1, "Female"),
			Sex = replace(Sex, Sex == 2, "Male"),
			# Education: all participants are primary school children.
			ISCED_Level = "Levels 0-2",
			# NUTS2 region: use a simple identifier for Uganda as a whole.
			NUTS2 = "Uganda"
			) %>%
		# Include only individuals in the podcast group.
		dplyr::filter(group == groups$intervention_children) %>%
		# Add rows that specify the number of questions that were answered correctly,
		# total number of questions asked, and whether each individual achieved passing or
		# mastery scores.
		dplyr::mutate(
			num_correct = {
				(dplyr::select(., correct_answers %>% names) == correct_answers) %>%
					rowSums(na.rm = TRUE)},
			num_total = length(correct_answers),
			passing = num_correct >= 13,
			mastery = num_correct >= 20) %>%
		# Select only the columns of further interest.
		dplyr::select(NR, country, school, group, Sex, ISCED_Level, NUTS2, num_correct,
									num_total, passing, mastery) %>%
		# Ensure all character variables are converted to factors.
  	dplyr::mutate_if(is.character, as.factor)
}