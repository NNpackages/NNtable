
#' Truncate columns at specified width
#'
#' @param .NNTable An \code{NNTable}
#' @param ... Named \code{integers} where the name is the column to wrap and the
#'   integer tis he number of characters on each line
#' @param exdent \code{integer} indicating number of spaces on the following
#'   line
#' @param split \code{character} specifying the character used to split
#'
#' @return Return the NNTable with the the truncation specified
#' @export
#'
#' @examples
#'
#' ## *****************************************************************************
#' ## # Example of transpose from wide to long                                 ----
#' ##
#' ## Calculate the statistics for an AE table
#' ## *****************************************************************************
#'
#' library(dplyr)
#' # library(NNR)
#'
#' # Filter data according Safety analysis set
#' adsl_f <- adsl            %>%
#'   filter(SAFFL == "Y")    %>%
#'     rename(TRTA = TRT01P)
#'
#' # Calculate total exposure
#' totals <- adsl_f        %>%
#'   group_by(TRTA)        %>%
#'   summarize("Number of subjects"     = n_distinct(USUBJID),
#'             "Total exposure (years)" = sum(TR01DURY))
#'
#' # Calculate adverse event statistics
#' output <- adae %>% group_by(AEBODSYS, AEDECOD, TRTA) %>%
#'   dplyr::summarise(N = n_distinct(USUBJID),
#'                    P = N / nrow(adsl),
#'                    E = n())
#'
#' # Generate the NNTable ------------------------------------------------------
#'
#' # Initiate by defining the columns based on ADAE
#' .NNTable <- NNTable(output, "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E")
#'
#' # Add exposure calculated from ADSL
#' .NNTable <- .NNTable %>%
#'   addExposure(exposure = totals,
#'               format_alone = FALSE,
#'               format_data  = c("Number of subjects"     = "%.0f",
#'                                "Total exposure (years)" = "%.2f"))
#'
#' # Transpose to wide
#' .NNTable <- .NNTable %>% addTransWide(TRTA = c("N", "(%)", "E"))
#'
#' # Add Filling such that non-observed events is assigned 0
#' .NNTable <- .NNTable %>% addFilling(N = 0)
#'
#' # Add formatting information for the table
#' .NNTable <- .NNTable %>% addFormat(format_data = c(N = "%.0f", P = "%.2f", E = "%.0f"))
#'
#' # Group the columns AEBODSYS and AEDECOD
#' .NNTable <- .NNTable %>% addGroupedColumns("AEBODSYS", "AEDECOD" , name = "some name")
#'
#' # Truncate columns at a certain width such that long names appear on multiple lines
#' .NNTable <- .NNTable %>% addTruncation(AEBODSYS = 50, AEDECOD = 48)
#'
#' # View the final result
#' .NNTable
addTruncation <- function(.NNTable, ..., exdent = 1, split = "\\|") {

  if (!is.null(.NNTable$cell_split) && .NNTable$cell_split$split != split)
    stop("The split variable needs to be the same as that used in addCellSplit")


  .NNTable$truncation <- list(columns = c(...), split = split, exdent = exdent)

  return(.NNTable)
}



apply_truncation <- function(.NNTable) {

  if (!is.null(.NNTable$truncation)) {
    split <- .NNTable$truncation$split
  } else {
    split <- .NNTable$cell_split$split
  }
  #trunc_cols <- .NNTable$truncation$columns
  #split_cols <- names(trunc_cols)

  data_str <- .NNTable$data_str

  table_cols <- setdiff(colnames(data_str), .NNTable$remove$columns)

  if (!is.null(.NNTable$grouped_columns))
    table_cols <- unique(c(table_cols, setdiff(.NNTable$grouped_columns$columns, .NNTable$grouped_columns$invisible)))

  table_cols_trunc <- paste0(table_cols, "_trunc")

  if (!data.table::is.data.table(data_str))
    data_str <- data.table::as.data.table(data_str)

  # copy colums
  data_str[, (table_cols_trunc) := lapply(.SD, as.character), .SDcols = table_cols]

  split_cols <- table_cols_trunc

  # if (!all(sapply(data_str[, split_cols, with = FALSE], is.character)))
  #   data_str <- data_str[, (split_cols) := lapply(.SD, as.character), .SDcols = split_cols]

  X <- lapply(data_str[, split_cols, with = FALSE], strsplit , split = split)

  SetUp <- lapply(X, function(x) {
    A <- vapply(x, length, 1L)
    list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
         Val = unlist(x))
  })

  Ncol <- max(unlist(lapply(SetUp, function(y)
    y[["Mat"]][, 2]), use.names = FALSE))

  X <- lapply(seq_along(SetUp), function(y) {
    M <- matrix("NNTable_Inserted_Blank", nrow = nrow(data_str), ncol = Ncol)
    M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
    M
  })

  indt <- data_str[rep(sequence(nrow(data_str)), each = Ncol)]

  X <- lapply(X, function(y) as.vector(t(y)))
  indt[, (split_cols) := lapply(X, unlist, use.names = FALSE)][]


  indt <- indt[!apply(indt[, split_cols, with = FALSE] == "NNTable_Inserted_Blank", 1, all), ]

  # We cannot clean this up yet because we need to group vars later and perhaps drop some of these cols
  if (is.null(.NNTable$grouped_columns))
    indt <- indt[, (split_cols) := lapply(.SD, function(x) ifelse(x == "NNTable_Inserted_Blank", "", x)), .SDcols = split_cols]

  .NNTable$truncation$split_cols <- split_cols

  .NNTable$data_str <- as.data.frame(indt)

  trunc_remove <- intersect(paste0(.NNTable$remove$columns, "_trunc"), colnames(.NNTable$data_str))
  .NNTable$remove$columns_trunc <- c(table_cols, trunc_remove)

  return(.NNTable)
}



