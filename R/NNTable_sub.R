#' Subsetting NNTables
#'
#' Subset a created NNTable so that only some columns are shown.
#'
#' @param x An NNtable
#' @param i Row subset
#' @param j Column subset
#' @param data \code{character} the data to subset, Can be either \code{"main"},
#'   \code{"exposure"} or both  \code{c("main", "exposure")} to subset the associated data set
#'
#' @name subsetting
#' @return An NNTable subset as specified
#' @export
#' @examples
#' library(NNR)
#' library(NNBiostat)
#'
#' # make sure the training data is up to date
#' NNtraining::createTrainingDB()
#' (db <- nnaccess("0001", root = "~/training"))
#'
#' adae_f <- db$adam("adae") %>% filter(SAFFL == "Y" & TRTA != "")
#' adsl_f <- db$adam("adsl") %>% filter(SAFFL == "Y" & TRT30A != "") %>%
#'  rename(TRTA = TRT30A)
#'
#'
#' subjects <- adsl_f %>%
#'  group_by(TRTA, SEX) %>%
#'  summarise("Number of subjects" = n_distinct(USUBJID),
#'            "Total exposure" = sum(TRDURY),
#'            .groups = "drop")
#'
#' stats <- adae_f %>%
#'   group_by(TRTA,AESOC,SEX) %>%
#'   summarise(AGE = mean(AGE),
#'             N = n_distinct(USUBJID),
#'             E = n(), .groups = "drop")
#'
#' table <- NNTable(stats, "TRTA", sex = "SEX", "AGE", "AESOC","N","E") %>%
#'   addTransWide("TRTA" = c("N","E")) %>%
#'   addGroupedColumns("AESOC") %>%
#'   addOrder(E = -1) %>%
#'   addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
#'                SEX   = c("Female" = "F", "Male" = "M"),
#'                AESOC = stringr::str_to_title)
#'
#'
#' # print the table
#' table
#'
#' # Subset to only females
#' table[SEX == "F"]
#'
#' # Remove the column age
#' table[, -"AGE"]
#'
#' # Adding an exposure table
#' exp_table <- table %>% addExposure(subjects)
#'
#' exp_table
#'
#' # subset to only females
#' exp_table[SEX == "F"]
#'
#' # subset and remove the columns SEX and AGE
#' # here it is important to note that AGE is only present in the main data
#'
#' exp_table[SEX == "F", -"SEX"][, -"AGE", "main"]
#'
#' # since AGE is not contained in the exposure data we need to instruct the
#' # subset to only look in the main data
#'
#' @importFrom rlang is_empty
`[.NNTable` <- function(x, i, j, data = c("main", "exposure")) {

  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if ("exposure" %in% data) {
    if (!is.null(x$exposure)) {
      if (!missingArg(i))
        x$exposure$data <-
          dplyr::filter(x$exposure$data, !!rlang::enquo(i))

      if (!missingArg(j)) {
        x$exposure$data <-
          dplyr::select(x$exposure$data, !!rlang::enquo(j))
      }
    }
  }

  if ("main" %in% data) {
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
  }

  return(x)
}
