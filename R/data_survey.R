#' Read data for one of the four questionnaires
#'
#' Do not use this function directly, but prefer the (memoized) \code{get_quiz_data}.
#' 
#' @param n The quiz number (1 to 4, inclusive).
#' 
#' @return a \code{tibble} containing the quiz data.
#' @importFrom magrittr "%>%"
do_get_quiz_data <- function(n) {
  stopifnot(n %in% c(1, 2, 3, 4))
  readxl::read_xlsx(system.file("extdata",
                                paste("Quiz-", n, "-data.xlsx", sep = ""),
                                package = "fhi.ihc.norway.2019"), progress = FALSE) %>%
    tibble::as_tibble() %>%
    # Rename some of the columns using the mapping defined in the variables list.
    {do.call(function(...) dplyr::rename(., ...), participant_characteristics_variables)} %>%
    # Recode columns.
    translate_all() %>%
    # Add a column that indicates which quiz the data comes from.
    dplyr::mutate(quiz = sprintf("Quiz %d", n)) %>%
    # Remove unwanted columns.
		dplyr::select(-dplyr::ends_with("_poeng"))
}

#' Read data for one of the four questionnaires
#' 
#' @param n The quiz number (1 to 4, inclusive).
#' 
#' @return a \code{tibble} containing the quiz data.
#' @export
get_quiz_data <- {
	if (exists("get_quiz_data")) {memoise::forget(get_quiz_data)}
	memoise::memoise(do_get_quiz_data)
}


#' Participants responses for all quizzes, in "wide" format
#' 
#' Make a single data frame containing all the quiz data.
#'
#' @return a "wide" \code{tibble} where each row is an individual and there is a column
#'   for each question. 
#' 
#' @importFrom magrittr "%>%"
#' @export
quiz_data_wide <- function() {
	1:4 %>%
		purrr::map_dfr(get_quiz_data) %>%
		# Ensure all character variables are converted to factors.
    dplyr::mutate_if(is.character, as.factor) %>%
    # Ensure NAs are factor levels.
    dplyr::mutate_if(is.factor,
    								 function(x) forcats::fct_explicit_na(x, na_level = "Missing"))
}

#' Function to make data for participant responses for all quizzes, in "long" format
#' 
#' Do not call this function directly, use \code{quiz_data_long}.
#'
#' @return a "long" \code{tibble} where individuals are identified by a \code{NR} column,
#'   and questions and participants' responses to them are provided in columns
#'   \code{Question} and \code{Answer}.
#' 
#' @importFrom magrittr "%>%"
make_quiz_data_long <- function() {
	# Define the variables that should not be "gathered".
	other_vars <- names(participant_characteristics_variables)
	other_vars <- c("NR", other_vars, "Sum", "Svartid", "quiz")
	
	# Define a function that recodes strings like "A) Ikke veldig sikker..." as "A",
	# or returns the original string if it does not match.
	recode_answer <- function(x) {
		if (substr(x, 1, 2) %in% paste0(LETTERS[1:4], ")")) substr(x, 1, 1) else x
	}
	
	# Define a function that extracts the question number from the question text.
	question_number <- function(x) {
		result <- strsplit(x, " ")[[1]][1]
		if (substr(result, nchar(result), nchar(result)) == ".")
			substr(result, 1, nchar(result) - 1)
		else
			result
	}
	
	# Define a function that takes a quiz number and converts it to long format. It is
	# important to do this once per quiz data set, rather than once for a single wide data
	# set with all quizzes, because we use the question numbers rather than the full
	# question text, to determine the correct answer and the key; question numbers can refer
	# to different questions across the quizzes.
	long_data_for <- function(n) {
		get_quiz_data(n) %>%
			# Convert to long format.
			tidyr::gather(key = "Question", value = "Answer", -other_vars) %>%
			# Change the values of the answers to single letters where possible.
			dplyr::mutate(Answer = Answer %>% purrr::map_chr(recode_answer)) %>%
			# Create a column that contains the question number.
			dplyr::mutate(Question_ID = Question %>% purrr::map_chr(question_number)) %>%
			# Remove a question for a concept that was probed three times (for validation), but
			# which we only need two of the questions.
			dplyr::filter(paste(quiz, "Question", Question_ID) != "Quiz 1 Question 18.2") %>%
			# Remove questions on key concept 1.1a
			dplyr::filter(paste(quiz, "Question", Question_ID) != "Quiz 1 Question 2") %>%
			dplyr::filter(paste(quiz, "Question", Question_ID) != "Quiz 2 Question 9") %>%
			dplyr::filter(paste(quiz, "Question", Question_ID) != "Quiz 2 Question 16.1") %>%
			# Dichotomize responses for the attitudes and behaviors questions.
			dplyr::mutate(Answer = replace(Answer, Answer == "Svært usannsynlig", "Unlikely")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Usannsynlig",       "Unlikely")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Svært sannsynlig",  "Likely")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Sannsynlig",        "Likely")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Jeg vet ikke",      "Don't know")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Svært vanskelig",   "Difficult")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Vanskelig",      		"Difficult")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Svært lett",        "Easy")) %>%
			dplyr::mutate(Answer = replace(Answer, Answer == "Lett",      		    "Easy")) %>%
			# Create a column with the correct answers.
			dplyr::mutate(correct_answer = paste(quiz, "Question", Question_ID) %>% purrr::map_chr(correct_answer)) %>%
			# Create a column that indicates if the question was answered correctly.
			dplyr::mutate(correct = dplyr::if_else(Answer == correct_answer, 1, 0)) %>%
			# Modify the correct column, so that missing responses are assessed to be incorrect.
			# Responses that do not have correct answers will remain as NA.
			{
				x <- .
				x$correct[is.na(x$Answer)] <- 0
				x
			} %>%
			# Create a column with the key concepts.
			dplyr::mutate(key_concept = paste(quiz, "Question", Question_ID) %>% purrr::map_chr(key_concept)) %>%
			# Add a column that indicates which quiz the data come from.
			dplyr::mutate(quiz = n)
	}
	
	# Return a single long data frame containing data for all the participants.
	1:4 %>%
		purrr::map_dfr(long_data_for) %>%
		# Add the columns that will be used for weighting.
		add_weight_columns() %>%
		# Ensure that all character columns are converted to factors.
		dplyr::mutate_if(is.character, as.factor) %>%
		# Ensure NAs are factor levels.
    dplyr::mutate_if(is.factor,
    								 function(x) forcats::fct_explicit_na(x, na_level = "Missing")) %>%
		# Add quiz_long to the class of the result.
		{
			class(.) <- c(class(.), "quiz_long")
			.
		}
}

#' Participants responses for all quizzes, in "long" format
#' 
#' Make a single data frame containing all the quiz data.
#'
#' @return a "long" \code{tibble} where individuals are identified by a \code{NR} column,
#'   and questions and participants' responses to them are provided in columns
#'   \code{Question} and \code{Answer}.
#' 
#' @importFrom magrittr "%>%"
#' @export
quiz_data_long <- {
  if (exists("quiz_data_long")) {memoise::forget(quiz_data_long)}
  memoise::memoise(make_quiz_data_long)
}
