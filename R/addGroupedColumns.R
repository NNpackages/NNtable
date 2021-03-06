#' Add grouped columns to an NNTable to form the table STUB
#'
#' @param .NNTable An \code{NNTable} generated by \code{\link{NNTable}}
#' @param ... elements with the names of the columns to group
#' @param name \code{character} with the name of grouping column
#' @param add_blank_row \code{logical} Add a blank line between every grouping.
#'   Alternative \code{character} with the column names of the group columns for
#'   which a blank should be added. Alternatively, it can be a \code{character}
#'   specifying which grouping columns should have a blank added, the grouping
#'   name can be assigned a value of either \code{"missing"} or \code{"always"}
#'   which overrules \code{add_blank_row_when}.
#' @param span_row \code{logical} should group columns span the entire row
#' @param invisible \code{character} Grouped columns that should not be
#'   displayed
#' @param add_blank_row_invisible \code{logical} should blank rows be added for
#'   invisible columns. Note, that this is an additional empty row compared to
#'   the header row that is blank. Ignored if \code{add_blank_row} is a
#'   character.
#' @param add_header_rows_invisible \code{logical} Should a blank header row be
#'   added to invisible grouped columns. Alternative, \code{character} with
#'   invisible groups that should have a blank header.
#' @param add_blank_row_when \code{character}, if set equal to \code{"missing"}
#'   blank rows are only added when an group header is missing, i.e. if the
#'   header is supplied in the data no blank row is added. When set equal to
#'   \code{always} it is added regardless of the group header was supplied in
#'   the data.
#' @param remove_duplicated_stub_row \code{logical} indicating whether or not
#'   duplicated rows should be displayed or be left blank. Defaults to TRUE as
#'   this is usually the wanted behavior, where the repeats are considered noise
#'
#' @return An object of class \code{NNTable} with the column grouping specified
#' @export
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
#'
addGroupedColumns <- function(.NNTable, ..., name = "", invisible = character(0),
                              span_row = TRUE,
                              add_blank_row = TRUE,
                              add_blank_row_when = c("missing", "always"),
                              add_blank_row_invisible  = FALSE,
                              add_header_rows_invisible = TRUE,
                              remove_duplicated_stub_row = TRUE) {

  add_blank_row_when <- match.arg(add_blank_row_when)
  columns <- c(...)

  add_blank_row <-
    NNtable_group_add_names(group_cols = columns,
                            invisible, add_blank_row,
                            add_blank_row_when,
                            add_blank_row_invisible)


  .NNTable$grouped_columns <- list(columns = columns, name = name,
                                   span_row = span_row,
                                   add_blank_row = add_blank_row,
                                   add_blank_row_when = add_blank_row_when,
                                   add_blank_row_invisible = add_blank_row_invisible,
                                   add_header_rows_invisible = add_header_rows_invisible,
                                   remove_duplicated_stub_row = remove_duplicated_stub_row,
                                   invisible = invisible)

  .NNTable$remove$columns <- c(.NNTable$remove$columns, "NNTable_master_group", columns, "NNTable_added_blank", "NNTable_added_group", "NNTable_group_level")

  return(.NNTable)
}


NNtable_group_add_names <- function(group_cols, invisible, add_blank_row,
                                    add_blank_row_when, add_blank_row_invisible) {

  if (is.logical(add_blank_row)) {

    if (add_blank_row) {

      add_blank_row <- group_cols[-1]

      if (!add_blank_row_invisible)
        add_blank_row <- setdiff(add_blank_row, invisible)

    } else {
      add_blank_row <- character(0)
    }
  }

  if (is.null(names(add_blank_row))) {
    add_blank_row <- structure(rep(add_blank_row_when, length(add_blank_row)), names = add_blank_row)
  } else {
    if (any(names(add_blank_row) == "")) {
      blank_names <- names(add_blank_row)
      blank_names[blank_names == ""] <- add_blank_row[blank_names == ""]

      named_vec <- structure(add_blank_row, names = blank_names)
      named_vec[names(add_blank_row) == "" ] <- add_blank_row_when
      add_blank_row <- named_vec
    }
  }

  return(add_blank_row)
}

# The strategy for the function is to add new rows for higher level groups if
# they are not already present
#
# Extra lines have been added to ensure blank space between groups
apply_groupColumns <- function(.NNTable) {


  # Get the data
  data_str <- data.table::as.data.table(.NNTable$data_str)

  # Get grouped columns
  group_cols <- .NNTable$grouped_columns$columns
  invisible <- .NNTable$grouped_columns$invisible
  current_colorder <- setdiff(colnames(data_str), group_cols)

  # Added blanks ----
  add_blank_row <- .NNTable$grouped_columns$add_blank_row


  # add_header_row ----
  add_header_row <- group_cols

  if (is.logical(.NNTable$grouped_columns$add_header_rows_invisible) &&
      !.NNTable$grouped_columns$add_header_rows_invisible) {

    add_header_row <- setdiff(add_header_row, invisible)
  }

  if (is.character(.NNTable$grouped_columns$add_header_rows_invisible)) {
    add_header_row <-
      setdiff(add_header_row,
              setdiff(invisible, .NNTable$grouped_columns$add_header_rows_invisible))
  }

  #---


  # Get the class of each column in data_str
  classes <- sapply(data_str[, group_cols, with = FALSE], class)

  # test for adding sorting to characters, The characters are made as factors
  # because of sorting issues
  for (char in names(classes[classes == "character"])) {
    data_str <- data_str[data_str[[char]] == "",  (char) := "NNTable_Empty"]
    data_str <- data_str[is.na(data_str[[char]]), (char) := "NNTable_Empty"]
    data_str[, char] <- as.factor(data_str[[char]])
    classes[char] <-  "factor"
  }


  # Add the empty level to each
  for (fac in names(classes[classes == "factor"]))
    data_str[, fac] <- factor(data_str[[fac]], levels = unique(c("NNTable_Empty", "NNTable_order", " ", levels(data_str[[fac]]))))


  # In case of truncation the columns are added a _trunc , which needs to be accounted for
  if (!is.null(.NNTable$truncation) | !is.null(.NNTable$cell_split)) {
    names(group_cols) <- paste0(group_cols, c("", "_trunc")[(!group_cols %in% invisible) + 1])
  } else {
    .NNTable$remove$columns_trunc <- paste0(group_cols, "_trunc")
    data_str_add <- data_str[, group_cols, with = FALSE]
    colnames(data_str_add) <- paste0(group_cols, "_trunc")
    data_str <- cbind(data_str, data_str_add)
    names(group_cols) <- paste0(group_cols, "_trunc")
  }


  # Replace NA with empty
  na_cols <- unique(c(group_cols, names(group_cols)))
  data_str <- data_str[, (na_cols) := lapply(.SD, function(x) {x[is.na(x)] <- " "; return(x)}), .SDcols = na_cols]


  # Initiate cols -----------------------------------------------

  # initiate grouped column
  NNTable_grouped_name <- NULL
  data_str$NNTable_grouped_name <- ""

  # create a master group to ensure we have something left for sorting
  data_str$NNTable_master_group <- data_str[[group_cols[1]]]
  data_str$NNTable_added_group <- FALSE
  data_str$NNTable_group_level <- length(group_cols)
  data_str$NNTable_added_blank = ""

  # Add the master col to the grouped cols
  group_cols <- c(structure("NNTable_master_group", names = "NNTable_master_group"), group_cols)

  # The groups are added in the reverse order
  seq <- rev(seq_along(group_cols)[-1])

  #browser()
  added_row <- FALSE
  for (i in seq) {

    # Find missing category rows
    # rows for the parent group (i-1) where we do not have an empty level on current (i)
    missing_groups <- !data_str[, get(group_cols[i - 1])] %in%
      data_str[data_str[[group_cols[i]]] %in% c("", "NNTable_Empty"), get(group_cols[i - 1])]

    # columns which are not empty needs to assign their value to the grouped column
    missing <- !(data_str[, get(group_cols[i])] %in% c("", "NNTable_Empty") &
                   data_str[, get("NNTable_added_blank")]  != "Y")

    # Create the group name for those not curently filled in on this or previous runs
    NNTable_current_fill <- missing &
      data_str$NNTable_grouped_name == "" &
      data_str$NNTable_group_level >= i - 1

    if (any(NNTable_current_fill)) {
      # calculate number of levels for indention
      indents <- sum(!group_cols[seq_len(i)] %in% invisible)

      if (!group_cols[i] %in% invisible)
        data_str[NNTable_current_fill,  NNTable_grouped_name :=
                   paste0(paste(rep(" ", 2*(indents - 2)), collapse = ""), # 2 because of the master level and the present level
                          data_str[NNTable_current_fill, get(names(group_cols[i]))])]
    }

    # In case all the previous visible columns are blank the added space is removed
    if (!group_cols[i] %in% invisible) {
      check_prev_cols <- setdiff(group_cols[seq_len(i - 1)], invisible)
      indents <- sum(!group_cols[seq_len(i)] %in% invisible)

      which_to_correct <-
        tidyr::unite(data_str, "new_col", !!!rlang::syms(check_prev_cols))$new_col %in%
        c(paste(rep(" ", indents - 1), collapse = "_"), paste(rep("NNTable_Empty", indents - 1), collapse = "_")) & missing_groups

      data_str[which_to_correct, NNTable_grouped_name :=
                 gsub(paste0("^", paste(rep(" ", 2*(indents - 2)), collapse = "")), "",
                      data_str$NNTable_grouped_name[which_to_correct])]
    }

    # get the order variables
    order_vars <- setdiff(unique(setdiff(.NNTable$order_columns$sort_columns, NULL)), group_cols[seq_len(i - 1)])

    # add order values to order columns
    by_vars <- c(unique(c(group_cols[seq_len(i - 1)], names(group_cols[seq_len(i - 1)]))))


    # Add the header rows ----
    all_empty <-
      apply(data_str[, lapply(.SD, function(x) x == "NNTable_Empty"), .SDcols = by_vars], 1, all)

    if (!group_cols[i] %in% add_header_row)
      all_empty[] <- TRUE

    # add one empty row above exposure data
    all_empty_cor <- all_empty
    if (any(grepl("NNTable_sort_NNTable_mj_order", colnames(data_str))) & !added_row) {

      correction <- missing_groups & missing & data_str[["NNTable_sort_NNTable_mj_order_1"]] == 0
      if (any(correction) & group_cols[i] %in% add_header_row) {
        added_row <- TRUE
        all_empty_cor[correction] <- FALSE
      }
    }


    #Create missing data frame with blanks except for variables used for sorting
    # for those we add 0 in front so that sorting is correct
    mis_df1 <- data_str[missing_groups & missing & !all_empty_cor, lapply(.SD, add_order_val),
                        by = by_vars,
                        .SDcols = setdiff(order_vars, c(group_cols))]


    mis_df2 <- data_str[missing_groups & missing & !all_empty_cor, lapply(.SD, add_order_val, group_var = TRUE),
                        by = by_vars,
                        .SDcols = intersect(order_vars, c(group_cols))]


    if (length(setdiff(order_vars, c(group_cols))) &
        length(intersect(order_vars, c(group_cols)))) {

      mis_df2[, (by_vars) := NULL]
      mis_df_group <- cbind(mis_df1, mis_df2)
    } else if (length(setdiff(order_vars, c(group_cols)))) {
      mis_df_group <- mis_df1
    } else {
      mis_df_group <- mis_df2
    }

    #browser()
    addons <- c("NNTable_added_blank", "NNTable_added_group", "NNTable_group_level")
    mis_df_group[, setdiff(colnames(data_str), c(colnames(mis_df_group), addons)) := ""]
    mis_df_group$NNTable_added_blank = ""
    mis_df_group$NNTable_added_group <- TRUE
    mis_df_group$NNTable_group_level <- i - 1L

    #mis_df_group$NNTable_added_group <- FALSE
    # add blank intersection columns between categories
    if (group_cols[i] %in% names(add_blank_row))  {

      by_vars2 <- c(unique(c(group_cols[seq_len(i - 1)], names(group_cols[seq_len(i - 2)]))))

      # get the order variables
      order_vars <- setdiff(unique(.NNTable$order_columns$sort_columns), group_cols[seq_len(i - 1)])

      if (add_blank_row[group_cols[i]] == "missing") {
        req <- missing_groups
      } else {
        req <- rep(TRUE, length(missing_groups))
      }

      mis_df3 <- data_str[req & !all_empty & data_str[, get(group_cols[i])] != "NNTable_Empty", lapply(.SD, add_order_val),
                          by = by_vars2,
                          .SDcols = setdiff(order_vars, c(group_cols))]

      mis_df4 <- data_str[req & !all_empty & data_str[, get(group_cols[i])] != "NNTable_Empty", lapply(.SD, add_order_val, group_var = TRUE),
                          by = by_vars2,
                          .SDcols = intersect(order_vars, c(group_cols)) ]

      if (length(setdiff(order_vars, c(group_cols))) &
          length(intersect(order_vars, c(group_cols)))) {

        mis_df4[, (by_vars2) := NULL]
        mis_df_blank <- cbind(mis_df3, mis_df4)
      } else if (length(setdiff(order_vars, c(group_cols)))) {
        mis_df_blank <- mis_df3
      } else {
        mis_df_blank <- mis_df4 # is NNTable_grouped_name missing from this when trunc has run
      }

      mis_df_blank[, setdiff(colnames(data_str), c(colnames(mis_df_blank), addons)) := ""]
      mis_df_blank$NNTable_added_group <- FALSE
      mis_df_blank <- unique(mis_df_blank, by = group_cols[seq_len(i - 1)])
      mis_df_blank$NNTable_added_blank = "Y"
      mis_df_blank$NNTable_group_level <- i - 1L
      mis_df <- rbind(mis_df_blank, mis_df_group)
    } else {
      mis_df <- mis_df_group
    }



    # In case of small datasets the classes may not end up as they began
    mis_df_data <- as.data.frame(mis_df)
    data_str_class <- lapply(data_str, class)
    dif_class <- mapply(function(x,y) any(y %in% x), data_str_class, lapply(mis_df_data, class)[names(data_str_class)])
    dif_class <- dif_class[!dif_class]
    #browser()
    for (col in names(dif_class)) {
       if (data_str_class[[col]] != "factor") {
         #browser()
         class(mis_df_data[, col]) <- data_str_class[[col]]
       }
    }

    glue_class <- sapply(data_str_class, function(x) "glue" %in% x)
    for (col in names(glue_class[glue_class])) {
      class(data_str[[col]]) <- "character"
      class(mis_df_data[, col]) <- "character"
    }

    # bind the data together
    data_str <- rbind(data_str, mis_df_data)
  }

  # correct the group column indicator
  data_str$NNTable_added_group[data_str$NNTable_grouped_name == ""] <- FALSE

  # Delete rows where all actual columns are blank and added for additional columns
  # Currently also deletes splits on other group vars
  if (!is.null(.NNTable$truncation) | !is.null(.NNTable$cell_split)) {
    split_cols <- .NNTable$truncation$split_cols
    split_cols2 <- unique(c(split_cols, "NNTable_grouped_name"))
    cols <- setdiff(.NNTable$truncation$split_cols, names(group_cols))

    delete <- grepl("^\\s*NNTable_Inserted_Blank$", data_str[["NNTable_grouped_name"]]) &
      apply(data_str[, mget(cols), drop = FALSE], 1, function(x) all(x == "NNTable_Inserted_Blank" | x == ""))

    data_str <- data_str[!delete, ]

    data_str <- data_str[, (split_cols2) := lapply(.SD, function(x) ifelse(grepl("^\\s*NNTable_Inserted_Blank$", x), "", x)), .SDcols = split_cols2]
  }


  # remove repeated group columns
  # collapse all these columns together, if they are duplicated replace NNTable_grouped_name with blank
  if (.NNTable$grouped_columns$remove_duplicated_stub_row)
    data_str$NNTable_grouped_name[duplicated(data_str[, mget(c(group_cols, "NNTable_grouped_name"))])] <- ""

  order_columns <-
    c(.NNTable$order_columns$sort_columns, "NNTable_master_group", .NNTable$grouped_columns$columns)
  # order according to the original columns
  added_vars <- unique(c(setdiff(order_columns, .NNTable$columns), .NNTable$grouped_columns$columns,
                         names(group_cols[group_cols != "NNTable_master_group"])))
  data_str <- data.table::setcolorder(data_str, c("NNTable_grouped_name",  setdiff(current_colorder, added_vars), added_vars, "NNTable_added_group", "NNTable_group_level"))


  # Return variables to characters
  .NNTable$data_str <- as.data.frame(data_str)

  return(.NNTable)
}



