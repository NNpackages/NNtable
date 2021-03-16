

addPrinter <- function(.NNTable, type = c("txt", "flextable", "docx"), ...) {

  type <- match.arg(type)

  .NNTable$print_method <- list(type = type,
                                add_arguments = list(...))

  return(.NNTable)
}


