

addPrinter <- function(.NNTable, type = c("txt", "flextable"), ...) {

  .NNTable$print_method <- list(type = type,
                                add_arguments = list(...))

  return(.NNTable)
}


