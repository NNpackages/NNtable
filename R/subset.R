#' @importFrom rlang is_empty
`[.NNTable` <- function(x, i, j) {

  if (!missingArg(i))
    x$data <-  dplyr::filter(x$data, !! rlang::enquo(i))

  if (!missingArg(j)) {
    x$data <-  dplyr::select(x$data, !! rlang::enquo(j))


    # After removing a column the NNTable needs to be reset
    columns_list <- rapply(x$columns_attr$columns_list, intersect,
                           y =  colnames(x$data),  how = "replace")

    columns_list <- columns_list[!sapply(columns_list, rlang::is_empty)]


    columns <- get_column_names(columns  = columns_list)
    columns_tree <- get_column_tree(columns  = columns_list)

    x$data_str        <- x$data
    x$columns         <- columns
    x$columns_attr    <- list(columns_list = columns_list,
                              columns_tree = columns_tree)

    x <- get_columns(x)
  }

  return(x)
}
