#' Title Make a Multiple Choice (including Likert Scale) in Qualtrics
#'
#' @param questions Required. The questions parameter is the stem, or the questions, that you would like to present in the multiple choice question. This should be a vector, list, or column in a dataset that contains character strings.
#' @param answer_scale Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the multiple choice questions. This should be a list of length equal to the number of questions that contains the answer choices you would like. Each element of the list for answer_scale should represent the answer choices for the corresponding question.
#'
#' @return This function will return a vector that is in the required format to import the question as a dropdown into qualtrics.
#' @export
#'
#' @examples
#' x <- c("I feel calm", "I feel sad", "I feel mad", "I feel happy")
#' y <- rep(list(c("Yes","No"),c("Strongly Agree","Agree","Neutral","Disagree","Strongly Disagree")),2)
#' make_qualtrics_MC(x,y)
make_qualtrics_MC <- function(questions, answer_scale) {
  if (is.list(answer_scale) == FALSE) {
    stop("Error: answer_scale must be a list")
  }
  if (length(answer_scale) != length(questions)) {
    stop("Error: length of answer_scale mush equal length of questions")
  }
  questions <- paste0("[[Question:MC]]", "\n", questions, "\n")
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  questions <- paste0(questions, answer_scale, collapse = "\n")
  return(questions)
}

make_qualtrics_MC_sprint <- function(x, options = c("Strongly Agree", "Agree", "Disgree" ,"Strongly Disgree")) {
  if (is.list(options)) {
    options <- sapply(options, function(x) {
      x <- paste0(x, collapse = "\n")
    })
  } else {
    options <- paste0(options, collapse = "\n")
  }
  x <- sprintf("[[Question:MC]]\n%s\n[[Choices]]\n%s\n[[pagebreak]]\n", x, options)
  x <- add_newline(x, 1, 2, 1)
  return(x)
}

add_newline <- function(x, btw, end, be) {
  
}

#' Title Make a Matrix Question in Qualtrics.
#'
#' @param questions Required. The questions parameter is the stem, or the questions, that you would like to present in the matrix. This should be a vector, list, or column in a dataset that contains character strings.
#' @param answer_scale Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the matrix. This should be a vector or a list of length one that contains all the answer choices you would like.
#' @param instructions Optional. The instructions parameter is the instructions for the participant that you would like displayed at the top of the matrix. The default is "Please answer each statement using the presented scale".
#'
#' @return This function will return a vector that is in the required format to import the question as a matrix into qualtrics.
#' @export
#'
#' @examples
#' x <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
#' y <- c("Strongly Disagree", "Disagree", "Neutral", "Agree","Strongly Agree")
#' z <- "Please read each question and answer each statement honestly using the following scale."
#' make_qualtrics_matrix(x, y, z)
make_qualtrics_matrix <- function(questions, answer_scale, instructions = "Please answer each statement using the presented scale") {
  if (is.list(answer_scale) == TRUE & length(answer_scale) != 1) {
    stop("Answer Choices in a list should have length of 1.")
  }
  if (is.list(answer_scale) == TRUE) {
    answer_scale <- unlist(answer_scale)
  }
  instructions <- paste0(instructions, collapse = "\n")
  instructions <- paste0("[[Questions:Matrix]]", "\n", instructions, "\n")
  questions <- paste0(questions, collapse = "\n")
  questions <- paste0("[[Choices]]", "\n", questions, "\n")
  options <- paste0("[[Answer]]", "\n", answer_scale)
  options <- paste0(options, collapse = "\n")
  options <- paste0("[[AdvancedAnswers]]", "\n", options, "\n")
  question <- paste0(instructions, questions, options, collapse = "\n")
  return(question)
}

make_qualtrics_textbox <- function(question) {
  
}

#' Title Make a Dropdown Question in Qualtrics.
#'
#' @param questions Required. The questions parameter is the stem, or the questions, that you would like to present in the matrix. This should be a vector, list, or column in a dataset that contains character strings.
#' @param answer_scale Required. The answer_scale parameter is the options that you want to give your participants for each of the questions presented in the dropdown. This should be a list of length equal to the number of questions that contains the answer choices you would like. Each element of the list for answer_scale should represent the answer choices for the corresponding question.
#'
#' @return This function will return a vector that is in the required format to import the question as a dropdown into qualtrics.
#' @export
#'
#' @examples
#' x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#' y <- list(c("Describes me very well","Does not Describe me at all"), c("Agree","Disagree"),c("Very True","Somewhat True","Neither True nor False", "Somewhat False", "Very False"))
#' make_qualtrics_dropdown(x,y)
make_qualtrics_dropdown <- function(questions, answer_scale) {
  if (is.list(answer_scale) == FALSE) {
    stop("Error: answer_scale must be a list")
  }
  if (length(answer_scale) != length(questions)) {
    stop("Error: length of answer_scale mush equal length of questions")
  }
  questions <- paste0("[[Question:MC:Dropdown]]", "\n", questions, "\n")
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  questions <- paste0(questions, answer_scale, collapse = "\n")
  return(questions)
}
#note for self: right now, this function works if the answer_scale is a vector that only contains one value whereas the function needs to work for a list that equals the number of questions

pagebreak <- function(question, number) {
  
}