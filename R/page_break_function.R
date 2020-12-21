#' Page Break helper function
#'
#' @param .x Not Required. Will be assigned within function
#' @param .items_per_page Not required. Will default as 1 item per page but can 
#' changed if specified 
#'
#' @return This is a helper function that will add \code{[[PageBreak]]} at a specified
#' interval between items 
#' @export
#'@examples
#'\donotrun{ 
#' x <- list("I like school", "Doing well in school is not important", "I don't need to do well in school to succeed in life")
#' y <- list(c("Describes me very well", "Does not Describe me at all"), c("Agree", "Disagree"), c("Very True", "Somewhat True", "Neither True nor False", "Somewhat False", "Very False"))
#' make_qualtrics_mc_dropdown(x, y, .items_per_page = 3L)
#' }
page_break <- function(.x, .items_per_page = 1L) {
  if(!is.numeric(.items_per_page)) {
    stop("items per page must be a whole number")
  }
  if (length(.x) == 0L) {
    stop("questions missing or empty")
  } else if (length(.x) == 1L) {
    warning("no page break added as length of .x is 1")
    return(.x)
  } else {
    .indx <- seq_along(.x) %% .items_per_page == 0L
    # if everything is false
    if (all(!.indx)) {
      warning("too many items per page, lower argument")
      return(.x)
    } else {
      .x <- paste0(.x, "\n")
      .x[.indx] <- paste0(.x[.indx], "[[PageBreak]]\n")
    }
  }
  return(.x)
}
