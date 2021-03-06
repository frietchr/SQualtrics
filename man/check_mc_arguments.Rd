% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qualtrics_function.R
\name{check_mc_arguments}
\alias{check_mc_arguments}
\title{Title Check Qualtrics Multiple Choice Arguments.}
\usage{
check_mc_arguments(questions, answer_scale)
}
\arguments{
\item{questions}{Required. The questions parameter is the stem, or the questions, that you would like to present in the matrix. This should be a vector, list, or column in a dataset that contains character strings.}

\item{answer_scale}{Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the matrix. This should be a vector or a list of length one that contains all the answer choices you would like.}
}
\value{
The function will return any error messages relevant to making a multiple choice question that can be imported into Qualtrics.
}
\description{
Title Check Qualtrics Multiple Choice Arguments.
}
\examples{
\dontrun{
stem <- c("I am sad", "I am mad", "I am happy")
options <- c("Yes", "No")
check_mc_arguments(stem, options)
question <- c("I am sad", "I am mad", "I am happy")
options <- rep(list(c("Yes", "No")), 2)
}
}
