make_qualtrics_MC <- function(x, options = c("Strongly Agree", "Agree", "Disgree" ,"Strongly Disgree")) {
  x <- paste0("[[Question:MC]]", "\n", x, "\n")
  options <- paste0(options, collapse = "\n")
  options <- paste0("[[Choices]]", "\n", options, "\n")
  x <- paste0(x, options, collapse = "\n")
  return(x)
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
  options <- paste0("[[Answer]]", "\n", answer_scale, "\n")
  options <- paste0(options, collapse = "\n")
  options <- paste0("[[AdvancedAnswers]]", "\n", options, "\n")
  question <- paste0(instructions, questions, options, collapse = "\n")
  return(question)
}

make_qualtrics_textbox <- function(question) {
  
}

make_qualtrics_dropdown <- function(question, answers) {
  
} 

pagebreak <- function(question, number) {
  
}