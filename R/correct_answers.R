#' Define the correct answers and key concepts in terms of quiz and question number pairs
correct_answer_data <- function() list(
	# Correct answers and the key concepts for quiz 1.
	"Quiz 1 Question 1" = list(answer = "A", kc = concepts$newer_not_better),
	"Quiz 1 Question 3" = list(answer = "A", kc = concepts$newer_not_better),
	"Quiz 1 Question 4" = list(answer = "A", kc = concepts$conflicting_interests),
	"Quiz 1 Question 5" = list(answer = "C", kc = concepts$experts_not_correct),
	"Quiz 1 Question 6" = list(answer = "B", kc = concepts$anecdotes),
	"Quiz 1 Question 7" = list(answer = "B", kc = concepts$advantages_disadvantages),
	"Quiz 1 Question 8" = list(answer = "B", kc = concepts$experts_not_correct),
	"Quiz 1 Question 9" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 1 Question 10" = list(answer = "A", kc = concepts$association_causation),
	"Quiz 1 Question 11" = list(answer = "A", kc = concepts$sampling_error),
	"Quiz 1 Question 12" = list(answer = "B", kc = concepts$patient_blinding),
	"Quiz 1 Question 13" = list(answer = "A", kc = concepts$consider_all_comparisons),
	"Quiz 1 Question 14" = list(answer = "A", kc = concepts$comparisons_required),
	"Quiz 1 Question 15" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 1 Question 16" = list(answer = "B", kc = concepts$association_causation),
	"Quiz 1 Question 17" = list(answer = "A", kc = concepts$comparisons_required),
	"Quiz 1 Question 18.1" = list(answer = "Jeg er uenig", kc = concepts$anecdotes),
	"Quiz 1 Question 18.3" = list(answer = "Jeg er enig", kc = concepts$common_practice),
	"Quiz 1 Question 18.4" = list(answer = "Jeg er enig", kc = concepts$conflicting_interests),
	"Quiz 1 Question 19.1" = list(answer = "Mer sikker", kc = concepts$patient_blinding),
	"Quiz 1 Question 19.2" = list(answer = "Mindre sikker", kc = concepts$sampling_error),
	"Quiz 1 Question 20.1" = list(answer = "Jeg er enig", kc = concepts$groups_similar_at_start),
	"Quiz 1 Question 20.2" = list(answer = "Jeg er uenig", kc = concepts$consider_all_comparisons),
	"Quiz 1 Question 20.3" = list(answer = "Jeg er enig", kc = concepts$advantages_disadvantages),
		# Attitudes for quiz 1. ("Correct" answer is the "desired" one.)
	"Quiz 1 Question 21" = list(answer = "Likely", kc = attitudes$att_research_willingness),

	# Correct answers and the key concepts for quiz 2.
	"Quiz 2 Question 1" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 2 Question 2" = list(answer = "A", kc = concepts$association_causation),
	"Quiz 2 Question 3" = list(answer = "A", kc = concepts$consider_all_comparisons),
	"Quiz 2 Question 4" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 2 Question 5" = list(answer = "B", kc = concepts$association_causation),
	"Quiz 2 Question 6" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 2 Question 7.1" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 2 Question 7.2" = list(answer = "B", kc = concepts$consider_all_comparisons),
	"Quiz 2 Question 8" = list(answer = "A", kc = concepts$more_not_better),
	"Quiz 2 Question 10" = list(answer = "B", kc = concepts$explanations_wrong),
	"Quiz 2 Question 11" = list(answer = "A", kc = concepts$dramatic_effects),
	"Quiz 2 Question 12" = list(answer = "C", kc = concepts$treat_comparators_equally),
	"Quiz 2 Question 13" = list(answer = "B", kc = concepts$assess_outcomes_equally),
	"Quiz 2 Question 14" = list(answer = "B", kc = concepts$all_should_be_followed_up),
	"Quiz 2 Question 15" = list(answer = "C", kc = concepts$systematic_reviews),
	"Quiz 2 Question 16.2" = list(answer = "Dårlig grunn", kc = concepts$explanations_wrong),
	"Quiz 2 Question 16.3" = list(answer = "Dårlig grunn", kc = concepts$dramatic_effects),
	"Quiz 2 Question 17" = list(answer = "D", kc = concepts$systematic_reviews),
	"Quiz 2 Question 18.1" = list(answer = "No", kc = concepts$treat_comparators_equally),
	"Quiz 2 Question 18.2" = list(answer = "Yes", kc = concepts$assess_outcomes_equally),
	"Quiz 2 Question 18.3" = list(answer = "Yes", kc = concepts$all_should_be_followed_up),
		# Attitudes for quiz 2.
	"Quiz 2 Question 19" = list(answer = "Likely", kc = attitudes$att_research_willingness),

	# Correct answers and the key concepts for quiz 3.
	"Quiz 3 Question 1" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 3 Question 2" = list(answer = "A", kc = concepts$association_causation),
	"Quiz 3 Question 3" = list(answer = "A", kc = concepts$consider_all_comparisons),
	"Quiz 3 Question 4" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 3 Question 5" = list(answer = "B", kc = concepts$association_causation),
	"Quiz 3 Question 6" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 3 Question 7.1" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 3 Question 7.2" = list(answer = "B", kc = concepts$consider_all_comparisons),
	"Quiz 3 Question 8" = list(answer = "C", kc = concepts$are_you_different),
	"Quiz 3 Question 9" = list(answer = "D", kc = concepts$practical_your_setting),
	"Quiz 3 Question 10" = list(answer = "B", kc = concepts$certainty_of_evidence),
	"Quiz 3 Question 11" = list(answer = "A", kc = concepts$practical_your_setting),
	"Quiz 3 Question 12" = list(answer = "D", kc = concepts$certainty_of_evidence),
	"Quiz 3 Question 13" = list(answer = "B", kc = concepts$are_you_different),
		# Attitudes for quiz 3.
	"Quiz 3 Question 14.1" = list(answer = "Likely", kc = attitudes$att_challenge_claim),
	"Quiz 3 Question 14.2" = list(answer = "Likely", kc = attitudes$att_se_14_2),
	"Quiz 3 Question 15.1" = list(answer = "Easy", kc = attitudes$att_se_15_1),
	"Quiz 3 Question 15.2" = list(answer = "Easy", kc = attitudes$att_se_15_2),
	"Quiz 3 Question 15.3" = list(answer = "Easy", kc = attitudes$att_se_15_3),
	"Quiz 3 Question 15.4" = list(answer = "Easy", kc = attitudes$att_se_15_4),
	"Quiz 3 Question 16" = list(answer = "Likely", kc = attitudes$att_research_willingness),

	# Correct answers and the key concepts for quiz 4.
	"Quiz 4 Question 1" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 4 Question 2" = list(answer = "A", kc = concepts$association_causation),
	"Quiz 4 Question 3" = list(answer = "A", kc = concepts$consider_all_comparisons),
	"Quiz 4 Question 4" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 4 Question 5" = list(answer = "B", kc = concepts$association_causation),
	"Quiz 4 Question 6" = list(answer = "A", kc = concepts$common_practice),
	"Quiz 4 Question 7.1" = list(answer = "A", kc = concepts$groups_similar_at_start),
	"Quiz 4 Question 7.2" = list(answer = "B", kc = concepts$consider_all_comparisons),
	"Quiz 4 Question 8.1" = list(answer = "Right", kc = concepts$confidence_intervals),
	"Quiz 4 Question 8.2" = list(answer = "Wrong", kc = concepts$p_values_misleading),
	"Quiz 4 Question 8.3" = list(answer = "Right", kc = concepts$relative_effects_misleading),
	"Quiz 4 Question 8.4" = list(answer = "Wrong", kc = concepts$averages_misleading),
	"Quiz 4 Question 8.5" = list(answer = "Wrong", kc = concepts$subgroups_misleading),
	"Quiz 4 Question 9" = list(answer = "D", kc = concepts$earlier_not_better),
	"Quiz 4 Question 10" = list(answer = "A", kc = concepts$itt_principle),
	"Quiz 4 Question 11" = list(answer = "C", kc = concepts$subgroups_misleading),
	"Quiz 4 Question 12" = list(answer = "B", kc = concepts$relative_effects_misleading),
	"Quiz 4 Question 13" = list(answer = "A", kc = concepts$averages_misleading),
	"Quiz 4 Question 14" = list(answer = "D", kc = concepts$confidence_intervals),
	"Quiz 4 Question 15" = list(answer = "C", kc = concepts$p_values_misleading),
	"Quiz 4 Question 16" = list(answer = "C", kc = concepts$outcomes_matter_to_you),
	"Quiz 4 Question 17" = list(answer = "A", kc = concepts$earlier_not_better),
	"Quiz 4 Question 18" = list(answer = "B", kc = concepts$itt_principle),
	"Quiz 4 Question 19" = list(answer = "A", kc = concepts$outcomes_matter_to_you),
		# Attitudes for quiz 4.
	"Quiz 4 Question 20" = list(answer = "Likely", kc = attitudes$att_research_willingness)
)

#' The correct answer a given quiz and question number pair
#'
#' @param quiz_question a string like "Quiz 1 Question 2" or "Quiz 1 Question 18.1".
#'
#' @return a character (e.g., "A"), indicating the correct answer for the quiz and
#'   question. If there is no correct answer defined for the quiz and question number
#'   pair, the function will return "REMOVE_ME", under the assumption that the pair
#'   corresponds to a "false" survey participant who did not take the quiz, but who was
#'   created as a result of converting the original wide data frames to long format.
correct_answer <- function(quiz_question) {
	answer_kc <- correct_answer_data()[[quiz_question]]
	if (is.null(answer_kc)) {
	  stop(answer_kc$kc, "is NULL")
	}
	answer_kc$answer
}

#' The concept that a given quiz and question number pair probes.
#'
#' @param quiz_question a string like "Quiz 1 Question 2" or "Quiz 1 Question 18.1".
#'
#' @return the key concept probed. If there is no correct answer defined for the quiz and
#'   question number pair, the function will return "REMOVE_ME", under the assumption that
#'   the pair corresponds to a "false" survey participant who did not take the quiz, but
#'   who was created as a result of converting the original wide data frames to long
#'   format.
key_concept <- function(quiz_question) {
	answer_kc <- correct_answer_data()[[quiz_question]]
	stopifnot(!is.null(answer_kc))
	answer_kc$kc
}
