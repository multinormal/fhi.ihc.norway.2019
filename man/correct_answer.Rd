% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_answers.R
\name{correct_answer}
\alias{correct_answer}
\title{The correct answer a given quiz and question number pair}
\usage{
correct_answer(quiz_question)
}
\arguments{
\item{quiz_question}{a string like "Quiz 1 Question 2" or "Quiz 1 Question 18.1".}
}
\value{
a character (e.g., "A"), indicating the correct answer for the quiz and
  question. If there is no correct answer defined for the quiz and question number
  pair, the function will return "REMOVE_ME", under the assumption that the pair
  corresponds to a "false" survey participant who did not take the quiz, but who was
  created as a result of converting the original wide data frames to long format.
}
\description{
The correct answer a given quiz and question number pair
}
