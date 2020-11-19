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
  #if answer scale is not a list, let user know it needs to be a list
  if (is.list(answer_scale) == FALSE) {
    stop("Error: answer_scale must be a list")
  }
  #if the length of the list for answer scale does not equal to length of questions, let user know these values have to be equal
  if (length(answer_scale) != length(questions)) {
    stop("Error: length of answer_scale mush equal length of questions")
  }
  #paste [[Question:MC]] above each question and separate with a new line
  questions <- paste0("[[Question:MC]]", "\n", questions, "\n")
  #combine the values in each element of answer scales and separate them by a new line
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  #add [[Choices]] above each element in the list of answer_scale
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  #paste questions and answer scale together and separate by a new line
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
#' a <- c("I feel calm", "I feel happy", "I feel tired", "I feel sad")
#' b <- c("Strongly Disagree", "Disagree", "Neutral", "Agree","Strongly Agree")
#' c <- "Please read each question and answer each statement honestly using the following scale."
#' make_qualtrics_matrix(a, b, c)
make_qualtrics_matrix <- function(questions, answer_scale, instructions = "Please answer each statement using the presented scale") {
  #because in a matrix, the questions will all have the same answer choices, answer_scale should either be a vector or a list with length one
  if (is.list(answer_scale) == TRUE & length(answer_scale) != 1) {
    stop("Answer Choices in a list should have length of 1.")
  }
  #if answer_scale is a list, unlist answer_scale
  if (is.list(answer_scale) == TRUE) {
    answer_scale <- unlist(answer_scale)
  }
  #paste the instructions and separate with a new line
  instructions <- paste0(instructions, collapse = "\n")
  #paste [[Questions:Matrix]] above instructions with new lines separating them
  instructions <- paste0("[[Questions:Matrix]]", "\n", instructions, "\n")
  #separate the questions with a new line
  questions <- paste0(questions, collapse = "\n")
  #paste [[Choices]] above all of the questions 
  questions <- paste0("[[Choices]]", "\n", questions, "\n")
  #paste [[Answer]] above each of the answer options
  options <- paste0("[[Answer]]", "\n", answer_scale)
  #seperate each answer with a new line
  options <- paste0(options, collapse = "\n")
  #paste [[AdvancedAnswers]] in front of the first [[Answers]]
  options <- paste0("[[AdvancedAnswers]]", "\n", options, "\n")
  #paste instructions, questions, and options together
  question <- paste0(instructions, questions, options, collapse = "\n")
  return(question)
}

make_qualtrics_multiselect <- function(questions, answer_scale) {
  #some options for this function include multiselect, single answer (horizontal), or multiselect(horizontal)
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
  #if answer scale is not a list, let user know it needs to be a list
  if (is.list(answer_scale) == FALSE) {
    stop("Error: answer_scale must be a list")
  }
  #if the length of the list for answer scale does not equal to length of questions, let user know these values have to be equal
  if (length(answer_scale) != length(questions)) {
    stop("Error: length of answer_scale mush equal length of questions")
  }
  #paste [[Question:MC:Dropdown]] above each question and separate with a new line
  questions <- paste0("[[Question:MC:Dropdown]]", "\n", questions, "\n")
  #combine the values in each element of answer scales and separate them by a new line
  answer_scale <- lapply(answer_scale, function(answer_scale) {
    paste0(answer_scale, collapse = "\n")
  })
  #add [[Choices]] above each element in the list of answer_scale
  answer_scale <- paste0("[[Choices]]", "\n", answer_scale, "\n")
  #paste questions and answer scale together and separate by a new line
  questions <- paste0(questions, answer_scale, collapse = "\n")
  return(questions)
}

pagebreak <- function(question, number) {
  
}
