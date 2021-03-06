#' Correct answers for Ugandan teachers
uganda_teachers_immediate_correct_answers <- list(
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

#' Data for Ugandan teachers (one-year follow-up)
#' 
#' The \code{ISCED_Level} column codes educational attainment using the same ranges of
#' ISCED 2011 levels used for the Norwegian population. Columns \code{passing} and
#' \code{mastery} are use the previously-published definitions.
#' @include names_groups.R
#' @export
uganda_teachers_data <- {
	correct_answers <- uganda_teachers_immediate_correct_answers
	filename <- "uganda-teachers-one-year-follow-up.csv"
	
	# Read and process the data.
	system.file("extdata", filename, package = "fhi.ihc.norway.2019") %>%
		readr::read_csv() %>%
		# Rename columns to be more readable/consistent with the survey data set.
		dplyr::rename(NR = record,
									school = qn1part1, # School one year ago (i.e., at randomization).
									group = qn0part2,
									Sex = qn1part3,
									ISCED_Level = qn1part4) %>%
		# Rename values to be more readable/consistent with the survey data set.
		dplyr::mutate(
			# Make the NR column unique to this data set.
			NR = paste0("UT-", NR),
			# Specify the country.
			country = "Uganda",
			# Treatment assignment.
			group = replace(group, group == 1, groups$podcast_teachers),
			group = replace(group, group == 2, groups$control_teachers),
			# Sex.
			Sex = replace(Sex, Sex == 1, "Female"),
			Sex = replace(Sex, Sex == 2, "Male"),
			# Education.
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 1, "Levels 0-2"),
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 2, "Levels 3-4"),
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 3, "Levels 5-8"),
			# NUTS2 region: use a simple identifier for Uganda as a whole.
			NUTS2 = "Uganda"
			) %>%
		# Add rows that specify the number of questions that were answered correctly,
		# and the total number of questions asked.
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