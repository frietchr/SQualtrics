#' Page Break helper function
#'
#' @param .x Not Required. Will be assigned within function
#' @param .items_per_page Not required. Will default as 1 item per page but can 
#' changed if specified 
#'
#' @return
#' @export
#'
#' @examples
page_break <- function(.x, .items_per_page = 1L) {
  if(!is.numeric(.items_per_page)) {
    stop("items per page must be a whole number")
  }
  if (length(.x) == 0L) {
    stop("questions missing or empty")
  } else if (length(.x) == 1L) {
    #todo
    warning("no page breaks added")
    return(.x)
  } else {
    .indx <- seq_along(.x) %% .items_per_page == 0L
    # if everything is false
    if (all(!.indx)) {
      warning("too many items per page, lower argument")
      return(.x)
    } else {
      .x <- paste0(.x, "\n")
      .x[.indx] <- paste0(.x[.indx], "[[pagebreak]]\n")
    }
  }
  return(.x)
}