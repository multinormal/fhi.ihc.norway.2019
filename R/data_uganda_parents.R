#' Correct answers for Ugandan parents
uganda_parents_immediate_correct_answers <- list(
  qn5 = 'B',
  qn8 = 'C',
  qn9 = 'B',
  qn10 = 'B',
  qn11 = 'B',
  qn12 = 'A',
  qn13 = 'D',
  qn16 = 'A',
  qn17 = 'A',
  qn18 = 'A',
  qn19 = 'B',
  qn20 = 'A',
  qn21part1 = '2',  # Disagree
  qn21part2 = '2',  # Disagree
  qn21part3 = '1',  # Agree
  qn22part1 = '1',  # More sure
  qn23part1 = '2',  # Disagree
  qn23part2 = '1'   # Agree
)

#' Function to get data for Ugandan parents
#' 
#' The \code{ISCED_Level} column codes educational attainment using the same ranges of
#' ISCED 2011 levels used for the Norwegian population. Unlike the children and teachers,
#' parents are not clustered within schools, and so parents are randomized units
#' \code{school} is set to a unique values for each parent. Columns \code{passing} and
#' \code{mastery} are use the previously-published definitions.
#' 
#' @param followup if \code{FALSE}, return data on parents tested immediately after the
#'   intervention in 2016, otherwise return data on the parents tested one year later.
#' @importFrom magrittr "%>%"
#' @include names_groups.R
read_uganda_parents_data <- function(followup = FALSE) {
	correct_answers <- uganda_parents_immediate_correct_answers
	
	# Determine the file to read, and other dataset-specific values.
	if(followup) {
		filename <- "uganda-parents-one-year-follow-up.csv"
		control_group_name <- groups$control_parents
		sex_column <- "qn1part3"
		education_column <- "qn1part5"
	} else {
		filename <- "uganda-parents-immediate.csv"
		control_group_name <- groups$control_parents
		sex_column <- "qn1part2"
		education_column <- "qn1part6"
	}
	
	# Read and process the data.
	system.file("extdata", filename, package = "fhi.ihc.norway.2019") %>%
		readr::read_csv() %>%
		# Rename columns to be more readable/consistent with the survey data set.
		dplyr::rename(NR = recordid,
									group = qn0part2,
									Sex := !!sex_column,
									ISCED_Level := !!education_column) %>%
		# Rename values to be more readable/consistent with the survey data set.
		dplyr::mutate(
			# Make the NR column unique to this data set.
			NR = paste0("UP-", NR),
			# Specify the country.
			country = "Uganda",
			# Cluster (see function documentation).
			school = NR,
			# Treatment assignment.
			group = replace(group, group == 1, groups$podcast_parents),
			group = replace(group, group == 2, control_group_name),
			# Sex.
			Sex = replace(Sex, Sex == 0, NA),
			Sex = replace(Sex, Sex == 1, "Female"),
			Sex = replace(Sex, Sex == 2, "Male"),
			# Education.
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 0, NA),
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 1, "Levels 0-2"),
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 2, "Levels 3-4"),
			ISCED_Level = replace(ISCED_Level, ISCED_Level == 3, "Levels 5-8"),
			# NUTS2 region: use a simple identifier for Uganda as a whole.
			NUTS2 = "Uganda"
			) %>%
		# Include only individuals in the control group.
		dplyr::filter(group == control_group_name) %>%
		# Add rows that specify the number of questions that were answered correctly,
		# and the total number of questions asked.
		dplyr::mutate(
			num_correct = {
				(dplyr::select(., correct_answers %>% names) == correct_answers) %>%
					rowSums(na.rm = TRUE)},
			num_total = length(correct_answers),
			passing = num_correct >= 11,
			mastery = num_correct >= 15) %>%
		# Select only the columns of further interest.
		dplyr::select(NR, country, school, group, Sex, ISCED_Level, NUTS2, num_correct,
									num_total, passing, mastery) %>%
		# Ensure all character variables are converted to factors.
  	dplyr::mutate_if(is.character, as.factor)
}

#' Data on Ugandan control parents tested one year after the intervention
#' 
#' The Education column codes educational attainment using the same ranges of ISCED 2011
#' levels used for the Norwegian population.
#' @export
uganda_parents_data <- read_uganda_parents_data(followup = TRUE)
	# If data immediately after the intervention is interesting, simply rbind it with the
	# above.
	