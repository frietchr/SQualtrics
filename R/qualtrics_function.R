#' Title Check Qualtrics Multiple Choice Arguments.
#'
#' @param questions Required. The questions parameter is the stem, or the questions, that you would like to present in the matrix. This should be a vector, list, or column in a dataset that contains character strings.
#' @param answer_scale Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the matrix. This should be a vector or a list of length one that contains all the answer choices you would like.
#'
#' @return The function will return any error messages relevant to making a multiple choice question that can be imported into Qualtrics.
#' @export
#'
#' @examples
#' stem <- c("I am sad", "I am mad", "I am happy")
#' options <- c("Yes", "No")
#' check_MC_arguments(stem, options)
#' question <- c("I am sad", "I am mad", "I am happy")
#' options <- rep(list(c("Yes", "No")), 2)
check_MC_arguments <- function(questions, answer_scale) {
  # checking to make sure that answer_scale is a list
  if (!is.list(answer_scale)) {
    stop("Error: answer_scale must be a list")
  }
  # if the questions are in a list, we will unlist questions
  if (is.list(questions)) {
    questions <- unlist(questions)
  }
  # length of answer_scale must be equal to the length of questions
  if (length(answer_scale) != length(questions)) {
    stop("Error: length of answer_scale mush equal length of questions")
  }
}
#' Title Make a Multiple Choice (including Likert Scale) in Qualtrics
#'
#' @inheritParams make_qualtrics_matrix
#' @return This function will return a vector that is in the required format to import the question as a dropdown into qualtrics.
#' @export
#'
#' @examples
#' x <- c("I feel calm", "I feel sad", "I feel mad", "I feel happy")
#' y <- rep(list(c("Yes", "No"), c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")), 2)
#' make_qualtrics_MC(x, y, .items_per_page = 3L)
make_qualtrics_MC <- function(questions, answer_scale, .items_per_page = 1L) {
  check_MC_arguments(questions, answer_scale)
  # paste [[Question:MC]] above each question and separate with a new line
  questions <- paste0("[[Question:MC]]", "\n", questions, "\n")
  # combine the values in each element of answer scales and separate them by a new line
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  # add [[Choices]] above each element in the list of answer_scale
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  # paste questions and answer scale together and separate by a new line
  questions <- paste0(questions, "\n", answer_scale)
  # page break
  questions <- page_break(questions, .items_per_page)
  quesions <- paste0(questions, collapse = "\n")
  return(questions)
}
#' Title Make a Matrix Question in Qualtrics.
#'
#' @param questions Required. The questions parameter is the stem, or the questions, that you would like to present in the matrix. This should be a vector, list, or column in a dataset that contains character strings.
#' @param answer_scale Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the matrix. This should be a vector or a list of length one that contains all the answer choices you would like.
#' @param instructions Optional. The instructions parameter is the instructions for the participant that you would like displayed at the top of the matrix. The default is "Please answer each statement using the presented scale".
#' @param .items_per_page Not required, will default to 1. Option allowing number of items to be shown per page. This should be a whole number.
#' @return This function will return a vector that is in the required format to import the question as a matrix into qualtrics.
#' @export
#'
#' @examples
#' a <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
#' b <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
#' c <- "Please read each question and answer each statement honestly using the following scale."
#' make_qualtrics_matrix(a, b, c, .items_per_page = 1L)
make_qualtrics_matrix <- function(questions, answer_scale, instructions = "Please answer each statement using the presented scale", .items_per_page = 1L) {
  # because in a matrix, the questions will all have the same answer choices, answer_scale should either be a vector or a list with length one
  if (is.list(answer_scale) == TRUE & length(answer_scale) != 1) {
    stop("Answer Choices in a list should have length of 1.")
  }
  # if answer_scale is a list, unlist answer_scale
  if (is.list(answer_scale) == TRUE) {
    answer_scale <- unlist(answer_scale)
  }
  # paste the instructions and separate with a new line
  instructions <- paste0(instructions, collapse = "\n")
  # paste [[Questions:Matrix]] above instructions with new lines separating them
  instructions <- paste0("[[Question:Matrix]]", "\n", instructions, "\n")
  # separate the questions with a new line
  questions <- paste0(questions, collapse = "\n")
  # paste [[Choices]] above all of the questions
  questions <- paste0("[[Choices]]", "\n", questions, "\n")
  # paste [[Answer]] above each of the answer options
  options <- paste0("[[Answer]]", "\n", answer_scale)
  # seperate each answer with a new line
  options <- paste0(options, collapse = "\n")
  # paste [[AdvancedAnswers]] in front of the first [[Answers]]
  options <- paste0("[[AdvancedAnswers]]", "\n", options, "\n")
  # paste instructions, questions, and options together
  question <- paste0(instructions, questions, options, collapse = "\n")
  return(question)
}
#' Title Multiple Choice with multiple answers
#'
#' @inheritParams make_qualtrics_matrix
#' @return This function will return a vector that is in the required format to import the question stems and options as a multiple choice questions that participants can select more than one option into qualtrics.
#' @export
#'
#' @examples
#' x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#' y <- list(c("Describes me very well", "Does not Describe me at all"), c("Agree", "Disagree"), c("Very True", "Somewhat True", "Neither True nor False", "Somewhat False", "Very False"))
#' make_qualtrics_MC_MultiSelect(x, y, .item_per_page = 2L)
make_qualtrics_MC_MultiSelect <- function(question, answer_scale, .items_per_page = 1L) {
  check_MC_arguments(question, answer_scale)
  question <- paste0("[[Question:MC:MultiSelect]]", "\n", question, "\n")
  answer_scale <- lapply(answer_scale, function(answers) {
    paste0(answers, collapse = "\n")
  })
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  question <- paste0(question, "\n", answer_scale)
  # page break
  question <- page_break(question, .items_per_page)
  question <- paste0(question, collapse = "\n")
  return(question)
}
#' Title Make a Dropdown Question in Qualtrics.
#'
#' @inheritParams make_qualtrics_matrix
#' @return This function will return a vector that is in the required format to import the question stems and options as a dropdown into qualtrics.
#' @export
#'
#' @examples
#' x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#' y <- list(c("Describes me very well", "Does not Describe me at all"), c("Agree", "Disagree"), c("Very True", "Somewhat True", "Neither True nor False", "Somewhat False", "Very False"))
#' make_qualtrics_dropdown(x, y, .items_per_page = 2L)
make_qualtrics_dropdown <- function(questions, answer_scale, .items_per_page = 1L) {
  check_MC_arguments(questions, answer_scale)
  # paste [[Question:MC:Dropdown]] above each question and separate with a new line
  questions <- paste0("[[Question:MC:Dropdown]]", "\n", questions, "\n")
  # combine the values in each element of answer scales and separate them by a new line
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  # add [[Choices]] above each element in the list of answer_scale
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  # paste questions and answer scale together and separate by a new line
  questions <- paste0(questions, answer_scale, collapse = "\n")
  # pagebreak
  questions <- page_break(questions, .items_per_page)
  return(questions)
}
#' Title Creating Qualtrics Blocks
#'
#' @param blocks Required. Blocks must be a list with each element of the list being the contents for one block. Each element of blocks should be one of the make_qualtrics functions (e.g., make_qualtrics_MC, make_qualtrics_dropdown, make_qualtrics_matrix)
#'
#' @return make_blocks outputs a vector that separates each element of the list blocks into a separate block for Qualtrics to import.
#' @export
#'
#' @examples
#' {
#'   x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#'   y <- list(c("Describes me very well", "Does not Describe me at all"), c("Agree", "Disagree"), c("Very True", "Somewhat True", "Neither True nor False", "Somewhat False", "Very False"))
#'   a <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
#'   b <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
#'   c <- "Please read each question and answer each statement honestly using the following scale."
#'   qualtrics <- list(make_qualtrics_dropdown(x, y), make_qualtrics_matrix(a, b, c))
#'   qualtrics <- make_blocks(qualtrics)
#' }
make_blocks <- function(blocks) {
  if (is.list(blocks) == FALSE) {
    stop("Error: blocks must be a list with each element of the list being the contents for one block")
  }
  invalid_blocks <- sapply(blocks, function(.blocks) {
    !grepl("^\\[\\[Question", .blocks)
  })
  if (any(invalid_blocks)) {
    stop(paste(
      "Error: invalid block number",
      which(invalid_blocks)
    ))
  }
  blocks <- lapply(blocks, function(blocks) {
    paste0("[[Block]]", "\n", blocks)
  })
  blocks <- paste0("[[AdvancedFormat]]", "\n", paste0(unlist(blocks), collapse = "\n"))
  return(blocks)
}

#' Title Outputting Qualtrics Survey to txt File
#'
#' @param blocks Required. The blocks argument is the saved variable from the make_blocks function that includes all the desired blocks for the survey
#' @param set_dir Optional. set_dir is the file path that you want to file to save in. Default is current working directory
#'
#' @return output_file will save blocks to a .txt file in a set directory so that user can upload the .txt file to Qualtrics
#' @export
#'
#' @examples
#' x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#' y <- list(c("Describes me very well", "Does not Describe me at all"), c("Agree", "Disagree"), c("Very True", "Somewhat True", "Neither True nor False", "Somewhat False", "Very False"))
#' a <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
#' b <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
#' c <- "Please read each question and answer each statement honestly using the following scale."
#' qualtrics <- list(make_qualtrics_dropdown(x, y), make_qualtrics_matrix(a, b, c))
#' qualtrics <- make_blocks(qualtrics)
#' output_file(qualtrics)
output_file <- function(blocks, set_dir = getwd(), out_file = "qualtrics_import.txt") {
  setwd(set_dir)
  sink(out_file)
  cat(blocks)
  sink()
  message(sprintf("Qualtrics Survey Import file was saved as qualtrics_import.txt to: %s", getwd()))
}
