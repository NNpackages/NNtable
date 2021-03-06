

#' Replace an NNTable object with an empty one if no data is present
#'
#' @param .NNTable An \code{NNTable} generated by \code{\link{NNTable}}
#'
#' @return An object of class \code{NNTable} without any data
#' @keywords internal
checkEmpty <- function(.NNTable){

  old_NNTable <- .NNTable

  if (nrow(.NNTable$data) == 0) {
    empty_output <- as_tibble("There is no data for this output")
    .NNTable <- NNTable(empty_output, " " = "value")
    if ("wrapping" %in% names(old_NNTable))
    .NNTable$wrapping <- old_NNTable$wrapping

    if (isTRUE(.NNTable$wrapping$remove_empty_footnote))
      .NNTable$wrapping$footer_orig <- character(0)
  }

  return(.NNTable)
}


