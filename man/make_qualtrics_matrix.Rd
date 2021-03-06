% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qualtrics_function.R
\name{make_qualtrics_matrix}
\alias{make_qualtrics_matrix}
\title{Title Make a Matrix Question in Qualtrics.}
\usage{
make_qualtrics_matrix(
  questions,
  answer_scale,
  instructions = "Please answer each statement using the presented scale",
  .items_per_page = 1L
)
}
\arguments{
\item{questions}{Required. The questions parameter is the stem, or the questions, that you would like to present. This should be a vector, list, or column in a dataset that contains character strings.}

\item{answer_scale}{Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented. This should be a vector or a list of length one that contains all the answer choices you would like.}

\item{instructions}{Optional. The instructions parameter is the instructions for the participant that you would like displayed at the top of the matrix. The default is "Please answer each statement using the presented scale".}

\item{.items_per_page}{Not required, will default to 1. Option allowing number of items to be shown per page. This should be a whole number.}
}
\value{
This function will return a vector that is in the required format to import the question as a matrix into qualtrics.
}
\description{
Title Make a Matrix Question in Qualtrics.
}
\examples{
\dontrun{ 
a <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
b <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
c <- "Please read each question and answer each statement honestly using the following scale."
make_qualtrics_matrix(a, b, c, .items_per_page = 1L)
}
}
