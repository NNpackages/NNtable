
#' Add ordering variables for NNTable

#' @param .NNTable An \code{NNTable} generated by \code{\link{NNTable}}
#' @param ... Names of the columns to oder by, If the they are set equal to -1
#'   they will be sorted descending, e.g. p = -1 will sort descending according
#'   to P
#'
#' @return An object of class \code{NNTable} with the ordering applied
#' @export
#' @examples
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
#' # Order the table according to P in descending order
#' .NNTable <- .NNTable %>% addOrder(P = -1)
#'
#' # View the final result
#' .NNTable
#'
addOrder <- function(.NNTable, ...) {
  columns <- c(...)

  # create the descending column
  names(columns)[names(columns) == ""] <- columns[names(columns) == ""]
  columns[!columns %in% c(1, -1)] <- 1
  columns <- structure(as.numeric(columns), names = names(columns))

  .NNTable$order_columns <- list(columns = names(columns), descending = names(columns)[columns == -1])
  #.NNTable$remove$columns <- setdiff(.NNTable$order_columns$columns, .NNTable$columns)
  .NNTable
}

apply_order <- function(.NNTable) {

  # Get the data
  data_str <- .NNTable$data_str

  # order according to order columns
  data_str <- data.table::setorderv(data.table::as.data.table(data_str), c(.NNTable$order_columns$sort_columns))


  .NNTable$data_str <- as.data.frame(data_str, stringsAsFactors = FALSE)

  return(.NNTable)
}


#' @importFrom rlang sym syms
#' @importFrom dplyr group_by ungroup select
apply_add_sorting_vars <- function(.NNTable) {


  order_columns <- .NNTable$order_columns$columns
  descending <- .NNTable$order_columns$descending

  data_str <- .NNTable$data_str


  left_over <- unlist(sapply(.NNTable$tree$tree, function(x) if (is.atomic(x)) x))
  left_over_columns <- .NNTable$columns[names(left_over)]

  classes <- sapply(data_str, class)

  by_temp <- names(classes)[classes %in% c("factor", "character")]
  stable <- intersect(left_over_columns, by_temp)

  tree_columns <- sapply(.NNTable$tree$tree, get_unlist_vars)



  where <- sapply(tree_columns, function(x) any(names(.NNTable$filling$columns) %in% x))
  # if transpose to wide is used
  # if any filling columns are in data
  # if the fillings are contained in the
  if (!is.null(.NNTable$columns_to_wide$columns) && any(where) &&
      any(names(where[where]) %in% names(.NNTable$columns_to_wide$columns))) {

    nestings <- get_unlist_names_only(.NNTable$columns_to_wide$columns)
    stable_non_wide <- setdiff(left_over_columns[!is.na(left_over_columns)], nestings)

  } else {
    stable_non_wide <- stable
  }


  # functions used for sorting
  sort_desc <- function(x) {
    if (all(is.na(x)))
      return(-Inf)

    (-max(xtfrm(x), na.rm = TRUE))
  }

  sort_norm <- function(x) {
    if (all(is.na(x)))
      return(Inf)

    (min(xtfrm(x), na.rm = TRUE))
  }

  # In case we have grouped columns the nested needs to take place within

  if (!is.null(.NNTable$grouped_columns)) {

    grouped_columns <- .NNTable$grouped_columns$columns

    stable_non_wide <- setdiff(stable_non_wide, grouped_columns)


    if (length(stable_non_wide)) {
      data_str$NNTable_stable_filling_sort <-
        glue::glue_data(data_str, paste0("{", paste0("`", stable_non_wide, "`", collapse = "}_{"), "}"))

      grouped_columns <- c(grouped_columns, "NNTable_stable_filling_sort")
    }

    # create the sort columns
    if (!is.null(order_columns)) {
      for (i in rev(order_columns)) {
        for (j in rev(seq_along(grouped_columns))) {
          if (grouped_columns[j] %in% colnames(data_str)) {

            data_str <-
              data_str %>% dplyr::group_by(!!! rlang::syms(grouped_columns[j])) %>%
              dplyr::mutate(!! rlang::sym(paste("NNTable_sort", i, j, sep = "_")) :=
                              ifelse(i %in% descending,  sort_desc(!!rlang::sym(i)),
                                                         sort_norm(!!rlang::sym(i)))) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(!! rlang::sym(paste("NNTable_sort", i, j, sep = "_")) :=
                              (!! rlang::sym(paste("NNTable_sort", i, j, sep = "_"))))
          } else {
            data_str <- data_str %>% dplyr::ungroup() %>%
              dplyr::mutate(!! rlang::sym(paste("NNTable_sort", i, j, sep = "_")) :=
                              ifelse(i %in% descending,  sort_desc(!!rlang::sym(i)),
                                                         sort_norm(!!rlang::sym(i)))) %>%
              dplyr::mutate(!! rlang::sym(paste("NNTable_sort", i, j, sep = "_")) :=
                              (!! rlang::sym(paste("NNTable_sort", i, j, sep = "_"))))
          }
        }
      }

      # Build the sorting order
      sort_columns <- character(0)
      for (j in seq_along(grouped_columns)) {
        sort_columns_j <- character(0)
        for (i in order_columns) {
          sort_columns_j <- c(sort_columns_j, paste("NNTable_sort", i, j, sep = "_"))
        }
        if (j == 1) {
          sort_columns <- c(sort_columns, sort_columns_j, "NNTable_master_group", grouped_columns[j])
        } else {
          sort_columns <- c(sort_columns, sort_columns_j, grouped_columns[j])
        }
      }

      if (length(stable_non_wide)) {
        data_str <- data_str %>% select(-"NNTable_stable_filling_sort")
        sort_columns <- setdiff(sort_columns, "NNTable_stable_filling_sort")
      }

      # Add the new sorting columns to the NNTable object
      .NNTable$data_str <- data_str
      .NNTable$order_columns$sort_columns <- unique(sort_columns)
    } else {
      .NNTable$order_columns$sort_columns <-
        c("NNTable_master_group", setdiff(grouped_columns, "NNTable_stable_filling_sort"))
    }

  } else {
    sort_columns <- character(0)
    if (!is.null(order_columns)) {
      for (i in order_columns) {
        data_str <- data_str %>%
          dplyr::group_by(!!! rlang::syms(stable)) %>%
          dplyr::mutate(!! rlang::sym(paste("NNTable_sort", i, sep = "_")) :=
                          ifelse(i %in% descending, sort_desc(!!rlang::sym(i)),
                                                    sort_norm(!!rlang::sym(i)))) %>%
          dplyr::ungroup()

        sort_columns <- c(sort_columns, paste("NNTable_sort", i, sep = "_"))

      }
      .NNTable$data_str <- data_str
      .NNTable$order_columns$sort_columns <- sort_columns

    }
  }

  .NNTable$remove$columns <-
    c(.NNTable$remove$columns, .NNTable$order_columns$sort_columns)

  .NNTable
}

add_order_val <- function(x, ...) {
  UseMethod("add_order_val")
}

add_order_val.numeric <- function(x, ..., group_var = FALSE, descending = FALSE) {

  if (!length(x)) {
    return(x)
  }

  if (group_var) {
    x_u <- unique(x)

    if (length(x_u) > 1) {
      diff <- min(diff(sort(unique(x)))) / 2
    } else {
      diff <- 0
    }

    if (descending) {
      return(max(x) + diff)
    } else {
      return(min(x) - diff)
    }


  } else {
    if (descending) {
      return(max(x))
    } else {
      return(min(x))
    }
  }
}

add_order_val.factor  <- function(x, ..., group_var = FALSE, descending = FALSE) {

  if (!length(x)) {
    return(x)
  }

  if (descending)
    return(ifelse(group_var, levels(x)[length(levels(x))], levels(x)[levels(x) %in% x]))

  return(ifelse(group_var, (setdiff(levels(x), "NNTable_Empty"))[1], levels(x)[levels(x) %in% x]))
}
add_order_val.character <- function(x, ..., group_var = FALSE, descending = FALSE) {

  if (!length(x)) {
    return(x)
  }

  if (descending)
    return(ifelse(group_var, paste0(max(x), "0"), max(x)))

  ifelse(group_var, paste0("0", min(x)), min(x))
}
add_order_val.defeault <- function(x, ..., group_var = FALSE, descending = FALSE) {

  if (!length(x)) {
    return(x)
  }

  if (descending)
    return(max(x))

  return(min(x))
}


