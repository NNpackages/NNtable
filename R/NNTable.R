globalVariables(c(":=", "!!", ".SD", "Group", "orig_order_to_delte", "keep_order_xy"))

#' Initialise an NN table
#'
#' @param page_size \code{list} containing page.length and page.width dictating
#'   the number of lines and number of characters on the page
#' @param .data \code{data.frame} to output as table
#' @param ... Names of the columns in .data that should be included in the
#'   table. If none are suppied all columns are used. If named, e.g. "New name"
#'   = "old name" the new name will be displayed.
#'
#' @return An object of type \code{NNTable}
#' @export
#'
#' @details In order to change the presentation of data several "add" functions
#' have been made
#' * \link{addAlignment}
#'     * Add alignment to the columns, can be center = "c", left = "l", and right = "r"
#' * \link{addCellSplit}
#'     * Add cell split in order to divide one cell of a table into multiple rows, i.e. add a newline
#' * \link{addExposure}
#'     * Add exposure information such as total number of subjects and years of exposure.
#' * \link{addFilling}
#'     * When transposing from long to wide some of the columns may become empty.
#'     This function is used to fill those empty columns with a value, eg setting
#'     N = 0 for adverse events where no events were observed in a specific arm.
#' * \link{addFormat}
#'     * Add formatting info to an NNTable, e.g. number of decimals or specific
#'     formatting for each cell of the table
#' * \link{addGroupedColumns}
#'     * Add grouped columns to an NNTable
#' * \link{addOrder}
#'     * Add ordering variables for NNTable
#' * \link{addTransLong}
#'     * Add columns that needs to be transposed from wide to long
#' * \link{addTransWide}
#'     * Add columns that needs to be transposed from long to wide
#' * \link{addTruncation}
#'     * Truncate columns at specified width. If the columns is longer than the
#'     specified width it will continue on the following line with two spaces added
#' * \link{addUnderScore}
#'     * Add underscores in the header of NNTable
#' * \link{addWrapping}
#'     * Add title and footnote to the table. These are usually added using the
#'     \code{exportOutput} function stored within the \code{nnaccess} object created by the function \code{nnaccess}
#'
#' @md
#' @examples
#' ## *****************************************************************************
#' ## # Calculate summary statistics                                           ----
#' ##
#' ## The exposure/summary statistics are calculated for ADSL and ADLB
#' ## *****************************************************************************
#'
#' # filter and rename adsl to get totals
#' # library(NNR)
#' library(dplyr)
#'
#' adsl_f <- adsl       %>%
#' filter(FASFL == "Y") %>%
#'   rename(TRTP = TRT01P)
#'
#' # calculate the totals by treatment arm
#' totals <- adsl_f         %>%
#'   mutate(TRTP = "Total") %>%
#'   bind_rows(adsl_f)      %>%
#'   group_by(TRTP)         %>%
#'   summarize("Number of subjects"     = n_distinct(USUBJID),
#'             "Total exposure (years)" = sum(TR01DURY))
#'
#' # Calculate the summary statistcs
#' output <- adlb                                                  %>%
#'   mutate(TRTP = "Total")                                        %>%
#'   bind_rows(adlb)                                               %>%
#'   filter(SAFFL == "Y" & PARAMCD %in% c("ABSR148N", "ABTIT148")) %>%
#'   group_by(TRTP, PARAM, STUDYID)                                %>%
#'   summarize(N  = n_distinct(USUBJID),
#'             Mean = mean(AVAL, na.rm = TRUE),
#'             SD   = sd(  AVAL, na.rm = TRUE),
#'             Min  = min( AVAL, na.rm = TRUE),
#'             Max  = max( AVAL, na.rm = TRUE)
#'   )
#'
#' # The NNTable is generated in small steps to view what happens
#'
#' # Initiate the table
#' .NNTable <- NNTable(output, "PARAM", "TRTP", "N", "Mean (SD)", "Min ; Max")
#'
#' # View the table
#' .NNTable
#'
#'
#' # Transpose the summary columns to long format
#' .NNTable <- .NNTable                                    %>%
#'   addTransLong(SUM = c("N", "Mean (SD)" , "Min ; Max"),
#'                var_name = "Called var")                 %>%
#'   addFormat(format_data = c(N = "%.0f"), dec = 2)
#'
#' # View the table
#' .NNTable
#'
#' # Add exposure, transpose to wide and ad grouping variables
#' .NNTable <- addExposure(.NNTable, exposure = totals,
#'                         format_alone = TRUE,
#'                         format_data  = c("Number of subjects" = "%.0f")) %>%
#'             addTransWide(TRTP = "SUM")                                   %>%
#'             addGroupedColumns("PARAM", "Called var")
#'
#' # View the final result
#' .NNTable
#'
#'
#'
#' ## *****************************************************************************
#' ## # Example of transpose from wide to long                                 ----
#' ##
#' ## Calculate the statistics for an AE table
#' ## *****************************************************************************
#'
#' library(dplyr)
#' #library(NNR)
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
#' @importFrom dplyr ungroup
NNTable <- function(.data, ..., page_size = getEOTpaper("port", 8)) {
  # create columns vector
  columns_list <- list(...)

  if (length(columns_list) == 1 & is.null(names(columns_list)))
    columns_list <- as.list(columns_list[[1]])

  # columns <- c(...)
  if (!length(columns_list)) {
    columns_list <- as.list(colnames(.data))
  }



  columns <- get_column_names(columns  = columns_list)
  columns_tree <- get_column_tree(columns  = columns_list)
  .NNTable <- list(data           = dplyr::ungroup(.data),
                  data_str        = dplyr::ungroup(.data),
                  columns         = columns,
                  columns_attr    = list(columns_list = columns_list,
                                         columns_tree = columns_tree),
                  remove          = list(columns = setdiff(NULL, columns)),
                  page_size       = page_size,
                  order_columns   = list(columns = NULL, descending = NULL),
                  columns_to_long = list(),
                  columns_to_wide = list()
  )

  class(.NNTable) <- c("NNTable", class(.NNTable))

  .NNTable <- get_columns(.NNTable)

  return(.NNTable)
}


get_column_names <- function(columns, vector = character(0), name = character(0)) {

  c_names <- names(columns)
  if (is.null(c_names)) {
    c_names <- rep("", length(columns))
  }


  prior_name <- name

  for (i in seq_along(columns)) {
    if (is.recursive(columns[[i]])) {
      vector <- get_column_names(columns[[i]], vector, name = c_names[i])
    } else if (rlang::is_named(columns[[i]]) | length(columns[[i]]) > 1 | length(prior_name)) {
      if (is.null(names(columns[[i]]))) {
        names(columns[[i]]) <- columns[[i]]
      } else {
        names(columns[[i]])[names(columns[[i]]) == ""] <-
          columns[[i]][names(columns[[i]]) == ""]
      }

      if (length(prior_name)) {
        name <- paste(names(columns[[i]]), c_names[i], prior_name, sep  = "__#__")
      } else {
        name <- paste(names(columns[[i]]), c_names[i], sep  = "__#__")
      }

      vector <- c(vector, structure(columns[[i]], names = name))

    } else {
      c_name <- c_names[i]
      if (c_name == "") c_name <- columns[[i]]
      if (length(prior_name)) {
        name <- paste(c_name, prior_name, sep  = "__#__")
      } else {
        name <- c_name
      }
      vector <- c(vector, structure(columns[[i]], names = name))
    }
  }

  return(vector)
}


get_column_tree <- function(columns, name = character(0)) {

  c_names <- names(columns)
  if (is.null(c_names)) {
    c_names <- rep("", length(columns))
  }

  prior_name <- name

  column_tree <- list()

  for (i in seq_along(columns)) {
    if (is.recursive(columns[[i]])) {
      if (length(prior_name)) {
        name = paste(c_names[i], prior_name, sep  = "__#__")
      } else {
        name = c_names[i]
      }
      column_tree[[c_names[i]]] <- unlist(get_column_tree(columns[[i]], name = name))
      names(column_tree[[c_names[i]]]) <- NULL
    } else if (rlang::is_named(columns[[i]]) | length(columns[[i]]) > 1 | length(prior_name)) {
      if (is.null(names(columns[[i]]))) {
        names(columns[[i]]) <- columns[[i]]
      } else {
        names(columns[[i]])[names(columns[[i]]) == ""] <-
          columns[[i]][names(columns[[i]]) == ""]
      }

      if (length(prior_name)) {
        name <- paste(names(columns[[i]]), c_names[i], prior_name, sep  = "__#__")
      } else {
        name <- paste(names(columns[[i]]), c_names[i], sep  = "__#__")
      }
      column_tree[[c_names[i]]] <- name
    } else {
      c_name <- c_names[i]
      if (c_name == "") c_name <- columns[[i]]
      column_tree[c_name] <-  c_name #columns[[i]]
    }
  }
  return(column_tree)
}








#' Print an NNTable object
#'
#' @param x The NNTable object
#' @param ... Additional arguments currently not used
#' @param page The page to display
#' @param file \code{character} optional for writing output to a file
#' @param verbose when the print is used to generate formatting results it is not
#'   desired to print the result
#' @param check_empty if TRUE data sets with zero rows are output as empty
#'
#' @return prints the table
#' @export
#' @importFrom xfun stringsAsStrings
print.NNTable <- function(x, ..., page = 1, file = NULL, verbose = TRUE, check_empty = TRUE) {
  .NNTable <- x

  if (check_empty)
    .NNTable <- checkEmpty(.NNTable)

  xfun::stringsAsStrings()

  .NNTable$data_str <- .NNTable$data

  .NNTable <- apply_createTree(.NNTable)

  # Add the exposure data to data_str
  if (!is.null(.NNTable$exposure)) {
    .NNTable <- applyExposure(.NNTable)
  }

  if (!is.null(.NNTable$order_columns) && length(.NNTable$order_columns$columns) |
      !is.null(.NNTable$grouped_columns)) {

    .NNTable <- apply_add_sorting_vars(.NNTable)
  }

  if (!is.null(.NNTable$filling))
    .NNTable <- apply_filling(.NNTable)

  .NNTable <- apply_create_format_data(.NNTable)

  .NNTable <- apply_format_concat(.NNTable)

  if (!is.null(.NNTable$columns_to_long) && length((.NNTable$columns_to_long))) {
    .NNTable <- apply_tranToLong(.NNTable)
  }

  if (!is.null(.NNTable$columns_to_wide) && length((.NNTable$columns_to_wide))) {
    .NNTable <- apply_tranToWide(.NNTable)
  }


  if (!is.null(.NNTable$truncation) | !is.null(.NNTable$cell_split)) {
    if (!is.null(.NNTable$truncation))
      .NNTable <- apply_split(.NNTable)

    .NNTable <- apply_truncation(.NNTable)
  }

  if (!is.null(.NNTable$grouped_columns)) {
    .NNTable <- apply_groupColumns(.NNTable)
  }

  if (!is.null(.NNTable$order_columns) && length(.NNTable$order_columns$sort_columns)) {
    .NNTable <- apply_order(.NNTable)
  }

  .NNTable <- apply_add_separators(.NNTable)

  .NNTable <- apply_alignment(.NNTable)

  .NNTable <- apply_createHeader(.NNTable)

  .NNTable <- apply_width(.NNTable, spread = TRUE)

  .NNTable <- apply_splitPages(.NNTable)

  .NNTable <- apply_data_to_string(.NNTable)

  if (is.null(file)) {
    if (verbose)
      apply_print_cons(.NNTable, page = page)
  } else {
    apply_print_file(.NNTable, file = file)
  }

  return(invisible(.NNTable))
}



apply_print_cons <- function(.NNTable, page = 1) {

  # Find the lines to print
  lines <- seq((page - 1) * .NNTable$page_size$page.length + 1 ,
               page * .NNTable$page_size$page.length)

  lines <- lines[lines < length(.NNTable$output)]
  # print to the console
  cat(.NNTable$output[lines], sep = "\n")
}

apply_print_file <- function(.NNTable, file = tempfile(fileext = ".txt"),
                             verbose = TRUE) {

  write_encoded(.NNTable$output, file)
}

