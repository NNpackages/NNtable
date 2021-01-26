#' Split cells into multiple cells in accordance with a splitter
#'
#' @param .NNTable An \code{NNTable}
#' @param split \code{character} specifying the character used to split
#' @param align \code{character} specifying alignment of splits
#'
#' @return Return the NNTable with the the truncation specified
#' @export
addCellSplit <- function(.NNTable, split = "\\|", align = c("top", "bottom", "centre")) {

  if (!is.null(.NNTable$truncation) && .NNTable$truncation$split != split)
    stop("The split variable needs to be the same as that used in addTruncation")

  .NNTable$cell_split <- list(split = split, align = match.arg(align))

  return(.NNTable)
}


#' @importFrom data.table is.data.table
#' @importFrom stringr str_split
#' @importFrom stringi stri_wrap
#' @importFrom dplyr all_of pull pull mutate_all
#' @importFrom tidyr unite
apply_split <- function(.NNTable) {

  split <- .NNTable$truncation$split

  if (split == "\\|") split = "|"

  trunc_cols <- .NNTable$truncation$columns

  exdent <- .NNTable$truncation$exdent


  data_str <- .NNTable$data_str
  if (!data.table::is.data.table(data_str))
    data_str <- data.table::as.data.table(data_str)


  add_split <- function(x, width = 50, collapse = split, split_first = !is.null(.NNTable$cell_split), cell_split = .NNTable$cell_split$split) {

    if (split_first) {
      wrapped <- tibble(x = stringr::str_split(x, cell_split)) %>%
        tidyr::unnest_wider(x, names_sep = "_") %>%
        dplyr::mutate_all(~stringi::stri_wrap(.x, width = width, simplify = FALSE, exdent = exdent))

      wrapped2 <- purrr::map_dfc(names(wrapped), ~
                                   wrapped %>%
                                   dplyr::select(all_of(.x)) %>%
                                   tidyr::unnest_wider(c(!!.x), names_sep="_"))

      y <- tidyr::unite(wrapped2, "z", all_of(colnames(wrapped2)), na.rm = TRUE, sep = collapse) %>% pull(all_of("z"))
    } else {
      letter_trunc <- stringi::stri_wrap(x, width = width, simplify = FALSE, exdent = exdent)
      y <- vapply(letter_trunc, paste, collapse = collapse, character(1))
    }

    y[is.na(x)] <- x[is.na(x)]
    y
  }

  data_str <- data_str[, (names(trunc_cols)) :=
                         mapply(add_split, .SD, width = trunc_cols, SIMPLIFY = FALSE),
                       .SDcols = names(trunc_cols)]


  .NNTable$data_str <- as.data.frame(data_str)


  return(.NNTable)
}
