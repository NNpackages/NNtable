globalVariables(c(":=", "!!", ".SD", "Group", "orig_order_to_delte", "keep_order_xy"))


#' Create a tree structure of the table
#'
#' @param .NNTable An \code{NNTable} object
#'
#' @return The \code{NNTable} object with the tree added
apply_createTree <- function(.NNTable) {

  # get the table
  table_cols <- .NNTable$concat$table

  if (!is.null(.NNTable$columns_to_long$columns)) {
    long_cols <- get_unlist_vars(.NNTable$columns_to_long$columns)
  } else{
    long_cols <- character(0)
  }

  if (!is.null(.NNTable$columns_to_wide$columns)) {
    wide_cols <- get_unlist_vars(.NNTable$columns_to_wide$columns)

    int <- intersect(wide_cols, names(.NNTable$columns_attr$columns_tree))

    if (length(wide_cols) == length(int)) {
      wide_cols <- unlist(.NNTable$columns_attr$columns_tree[wide_cols])
    } else {
      v2 <- c()
      trans <- .NNTable$columns_attr$columns_tree
      for (add in wide_cols) {
        if (add %in% names(trans)) {
          v2 <- c(v2, unlist(.NNTable$columns_attr$columns_tree[add]))
        } else {
          v2 <- c(v2, add)
        }
      }
      wide_cols <- v2
    }
    names(wide_cols) <- NULL
  } else{
    wide_cols <- character(0)
  }

  unlist(.NNTable$columns_attr$columns_tree[wide_cols])

  tree_base_1 <- c(setdiff(names(table_cols), long_cols),
                   names(.NNTable$columns_to_long$columns))
  tree_base_2 <- c(setdiff(tree_base_1, wide_cols),
                   names(.NNTable$columns_to_wide$columns))


  tree <- as.list(tree_base_2)
  names(tree) <- tree_base_2

  var <- intersect(tree_base_2, names(.NNTable$columns_to_wide$columns))
  if (length(var))
    tree[[var]] <- .NNTable$columns_to_wide$columns[[1]]


  find_nested_long <- function(x, longs = .NNTable$columns_to_long$columns) {

    if (is.recursive(x)) {
      x[[1]] <- find_nested_long(x[[1]], longs = longs)
    }

    else {
      if (x %in% names(longs)) {
        out <- list()

        out[[x]] <- .NNTable$concat$table[longs[[1]]]

        if (length(out[[1]]) == 1)
          out <- out[[1]]

        return(out)

      } else {
        out <- .NNTable$concat$table[x]

        if (length(out[[1]]) == 1)
          out <- out[[1]]

        return(out)

      }
    }
    return(x)
  }

  if (!is.null(.NNTable$columns_to_long$columns))
    tree <- lapply(tree, find_nested_long)

  .NNTable$tree <- list(tree = tree)
  return(.NNTable)
}

apply_create_format_data <- function(.NNTable) {

  if (!"NNFormat" %in% names(.NNTable)) {

    .NNTable <- Format(.NNTable)

  } else {

    .NNTable <- Format(.NNTable,
                       dec         = .NNTable$NNFormat$dec,
                       format_data = .NNTable$NNFormat$format_data,
                       big.mark    = .NNTable$NNFormat$big.mark,
                       small.mark  = .NNTable$NNFormat$small.mark,
                       group_by    = .NNTable$NNFormat$group_by)

  }

  return(.NNTable)
}

apply_data_to_string <- function(.NNTable) {
  data_split <- .NNTable$data_split

  # Combine header parts
  header1 <- c(.NNTable$wrapping$title,
              hline(times = .NNTable$page_size$page.width),
              .NNTable$header$header,
              hline(times = .NNTable$page_size$page.width))

  header_other <- c(paste0("\f", .NNTable$wrapping$title[1]), .NNTable$wrapping$title[-1],
          hline(times = .NNTable$page_size$page.width),
          .NNTable$header$header,
          hline(times = .NNTable$page_size$page.width))

  # Combine footer parts
  footer <- c(hline(times = .NNTable$page_size$page.width),
              .NNTable$wrapping$footer,
              .NNTable$wrapping$auto_foot)

  # Function for adding the header and footer to the output list
  collapse_fun <- function(x, header = header_other) {
    names(x) <- paste0("col_", seq_len(ncol(x)))
    string <- glue::glue_data(x, paste0("{", paste(colnames(x), collapse = "}{"), "}"))

    c(header, string, footer)
  }

  # Apply the function
  if (length(data_split) > 1) {
    .NNTable$output <- c(collapse_fun(data_split[[1]], header = header1),
                        unlist(lapply(data_split[-1], collapse_fun)))
  } else {
    .NNTable$output <- unlist(lapply(data_split, collapse_fun, header = header1))
  }
  return(.NNTable)
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



apply_tranToLong <- function(.NNTable) {


  #Create a data.table
  data <- data.table::as.data.table(.NNTable$data_str)


  # retrieve the columns
  columns <- .NNTable$columns_to_long$columns

  # create the value name
  if (.NNTable$columns_to_long$value_name != "") {
    value = .NNTable$columns_to_long$value_name
  } else {
    value = "value"
  }

  # create a current order variable
  data$orig_order_to_delte <- seq_len(nrow(data))

  # reshape the data
  out <- data.table::melt(data, measure.vars  = columns,
                          value.name = value, variable.name = "Group")


  # if remove blanks is set to TRUE blank rows are removed
  if (.NNTable$columns_to_long$remove_blank)
    out <- out[out[[value]] != "", ]


  # if colums vector is named, the names are used for the group names
  group_names <- initName(columns)
  if (!is.null(names(group_names))) {
    new_names = structure(names(group_names),
                          names = group_names)

    out <- out[ , Group := factor(new_names[unlist(out[, Group])], levels = new_names)]
  } else {
    out <- out[ , Group := factor(unlist(out[, Group]), levels = group_names)]
  }

  # order according to the original variable
  out <- out[order(orig_order_to_delte)]
  out <- out[, orig_order_to_delte := NULL]

  # Rename the group column to the supplied name
  out <- data.table::setnames(out, old = "Group", new = .NNTable$columns_to_long$var_name)


  # Replace the string data set
  .NNTable$data_str <- as.data.frame(out)

  # Return the updated NNTable
  return(.NNTable)
}



#' @importFrom data.table dcast as.data.table
#' @importFrom stats formula
apply_tranToWide <- function(.NNTable) {

  # Extract the data
  data <- data.table::as.data.table(.NNTable$data_str)

  # Get the call
  columns_to_wide_1 <- .NNTable$columns_to_wide$columns


  translate <- function(x) {

    out <- x
    for (i in seq_along(x)) {
      if (is.recursive(x[[i]])) {
        out[[i]] <- translate(x[[i]])
      } else {

        miss <- setdiff(x[[i]], colnames(data))

        if (length(miss)) {
          orig <- intersect(x[[i]], names(.NNTable$columns_attr$columns_tree[miss]))
          new  <- intersect(x[[i]], colnames(data))
          out[[i]] <- c(unlist(.NNTable$columns_attr$columns_tree[orig]), new)
          names(out[[i]]) <- NULL
        } else {
          out[[i]] <- x[[i]]
        }
      }
    }

    out
  }

  columns_to_wide <- translate(x = columns_to_wide_1)


  data$totally_stable_column <- "TESTER"

  getListLevel <- function(x, level = 2) {
    for (i in seq_len(level))
      x <- x[[1]]
    return(x)
  }

  varnames <- #gsub("#", "NNable_square",
    initName(columns_to_wide)#)

  data_out <- data

  if (.NNTable$columns_to_wide$add_cat_space) {
    data_out$space.column.1 <- ""
    data_out$space.column.2 <- ""
    varnames = c("space.column.1", varnames, "space.column.2")
  }

  sequence <-  rev(seq_len(depth(columns_to_wide)) - 1)


  #colnames(data_out) <- gsub("#", "NNable_square", colnames(data_out))

  for (i in sequence) {

    columns_to_wide.i <- getListLevel(columns_to_wide, i)
    #columns_to_wide.i <- gsub("#", "NNable_square", columns_to_wide.i)
    stable.vars <- setdiff(colnames(data_out), c(names(columns_to_wide.i), varnames))


    comb <- data.table::dcast(data_out,
                              formula(paste(paste0("`", stable.vars, "`", collapse = " + " ), " ~ ",
                                            paste0("`", names(columns_to_wide.i)[1], "`"), collapse = "")),
                              value.var = varnames, fill = "", fun.aggregate = .NNTable$columns_to_wide$fun.aggregate,
                              sep = "__#__")


    if (is.factor(data[[names(columns_to_wide.i)[1]]])) {
      f <- levels(data[[names(columns_to_wide.i)[1]]])
      f <- f[f %in% as.character(data[[names(columns_to_wide.i)[1]]])]
    } else {
      f <- sort(unique(data[[names(columns_to_wide.i)[1]]]))
    }

    varnames <- paste(rep(varnames, length(f)  ), rep(f, each = length(varnames)), sep = "__#__")

    cols <- c(stable.vars, varnames)
    data_out <- comb[, cols,  with = FALSE]
  }


  data_out <- data_out[, !"totally_stable_column"]

  # find space columns
  if (.NNTable$columns_to_wide$.remove_empty_columns) {
    spacers.l <- grepl("^space.column.", cols)

    acc_cols <- setdiff(cols[!spacers.l], stable.vars)

    all_empty <-
      apply(data_out[, acc_cols, with = FALSE] == "", 2, all)

    if (any(all_empty)) {
      acc_empty_names <- names(all_empty)[all_empty]

      if (.NNTable$columns_to_wide$.remove_empty_level == 1) {
        which <- match(acc_empty_names, cols)
        data_out[, (cols[unique(c(which - 1, which, which + 1))]) := NULL]
      } else {
        remove_cols <- character(0)

        empty_col_split <- strsplit(acc_empty_names, "__#__")

        max_depth <- max(sapply(empty_col_split, length))

        if (max_depth >= .NNTable$columns_to_wide$.remove_empty_level) {
          for (i in max_depth:.NNTable$columns_to_wide$.remove_empty_level) {
            uniq_level <- unique(sapply(empty_col_split, function(x) {
              paste0(x[min(i, length(x)):length(x)], collapse = "__#__")}))

            for (level in uniq_level) {

              cols_2 <- acc_cols[stringr::str_detect(acc_cols, stringr::fixed(paste0("__#__", level)))]


              if (length(cols_2) > 0 && all(cols_2 %in% acc_empty_names)) {
                remove_cols <- c(remove_cols, cols_2)
              }
            }
          }

          remove_cols2 <- unique(remove_cols)

          which <- match(remove_cols2, cols)
          data_out[, (cols[unique(c(which - 1, which, which + 1))]) := NULL]
        }
      }


    }
  }

  # Remove unwanted spacers
  spacers   <- grep("^space.column.", colnames(data_out))

  dup.spacers <- spacers[diff(spacers) == 1]

  if (max(spacers) == ncol(data_out))
    dup.spacers <- c(dup.spacers, ncol(data_out))

  if (min(spacers) == 1)
    dup.spacers <- c(1, dup.spacers)

  .NNTable$data_str <- as.data.frame(data_out[, (dup.spacers) := NULL])


  return(.NNTable)
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
    data_str <- data_str[data_str[[char]] == "", (char) := "NNTable_Empty"]
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
  data_str[, (na_cols) := lapply(.SD, function(x) {x[is.na(x)] <- " "; return(x)}), .SDcols = na_cols]


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

  for (i in seq) {

    # Find missing category rows
    # rows for the parent group (i-1) where we do not have an empty level on current (i)
    missing_groups <- !data_str[, get(group_cols[i - 1])] %in%
         data_str[data_str[[group_cols[i]]] %in% c("", "NNTable_Empty"), get(group_cols[i - 1])]

    # columns which are not empty needs to assign their value to the grouped column
    missing3 <- !(data_str[, get(group_cols[i])] %in% c("", "NNTable_Empty") & data_str[, get("NNTable_added_blank")]  != "Y")

    # Create the group name for those not curently filled in on this or previous runs
    NNTable_current_fill <- missing3 & data_str$NNTable_grouped_name == "" & data_str$NNTable_group_level >= i - 1

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

    #Create missing data frame with blanks except for variables used for sorting
    # for those we add 0 in front so that sorting is correct
    mis_df1 <- data_str[missing_groups & missing3 & !all_empty, lapply(.SD, add_order_val),
                   by = by_vars,
                   .SDcols = setdiff(order_vars, c(group_cols))]


    mis_df2 <- data_str[missing_groups & missing3 & !all_empty, lapply(.SD, add_order_val, group_var = TRUE),
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

    mis_df_group[, setdiff(colnames(data_str), c(colnames(mis_df_group))) := ""]
    mis_df_group$NNTable_added_blank = ""
    mis_df_group$NNTable_added_group <- TRUE
    mis_df_group$NNTable_group_level <- i - 1

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

      mis_df_blank[, setdiff(colnames(data_str), c(colnames(mis_df_blank))) := ""]
      mis_df_blank$NNTable_added_group <- FALSE
      mis_df_blank <- unique(mis_df_blank, by = group_cols[seq_len(i - 1)])
      mis_df_blank$NNTable_added_blank = "Y"
      mis_df_blank$NNTable_group_level <- i - 1
      mis_df <- rbind(mis_df_blank, mis_df_group)
    } else {
      mis_df <- mis_df_group
    }

    # bind the data together
    data_str <- rbind(data_str, as.data.frame(mis_df))
    data_str$NNTable_added_group <- as.logical(data_str$NNTable_added_group)
  }


  # correct the group column indicator
  data_str$NNTable_added_group[data_str$NNTable_grouped_name == ""] <- FALSE

  # Delete rows where all actual columns are blank and added for additional columns
  # Currently also deletes splits on other group vars
  if (!is.null(.NNTable$truncation) | !is.null(.NNTable$cell_split)) {
    split_cols <- .NNTable$truncation$split_cols
    split_cols2 <- unique(c(split_cols, "NNTable_grouped_name"))
    cols <- setdiff(.NNTable$truncation$split_cols, names(group_cols))

    delete <- grepl("Inserted Blank", data_str[["NNTable_grouped_name"]]) &
      apply(data_str[, mget(cols), drop = FALSE], 1, function(x) all(x == "Inserted Blank" | x == ""))

    data_str <- data_str[!delete, ]

    data_str <- data_str[, (split_cols2) := lapply(.SD, function(x) ifelse(x == "Inserted Blank", "", x)), .SDcols = split_cols2]
  }

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

apply_order <- function(.NNTable) {

  # Get the data
  data_str <- .NNTable$data_str

  # order according to order columns
  data_str <- data.table::setorderv(data.table::as.data.table(data_str), c(.NNTable$order_columns$sort_columns))


  .NNTable$data_str <- as.data.frame(data_str, stringsAsFactors = FALSE)

  return(.NNTable)
}



#' Apply specified format and concatenate columns
#'
#' @param .NNTable An \code{NNTable} generated by \code{\link{NNTable}}
#'
#' @importFrom stringr str_glue_data
apply_format_concat <- function(.NNTable) {
  .NNTable$columns

  data_str <- data.table::as.data.table(.NNTable$data_str)

  # find the numeric columns
  numerics <- sapply(.NNTable$data_str, is.numeric)
  numerics_table <- numerics[names(numerics) %in% unlist(.NNTable$concat$table)]

  characters <-  sapply(.NNTable$data_str, is.character)
  characters_table <- characters[names(characters) %in% unlist(.NNTable$concat$table)]

  factors <-  sapply(.NNTable$data_str, is.factor)
  factors_table <- factors[names(factors) %in% unlist(.NNTable$concat$table)]


  if (any(!(numerics | characters | factors))) {
    others <- !(numerics | characters | factors)
    .cols <- names(others)[others]
    data_str[, (.cols) := lapply(.SD, as.character), .SDcols =  .cols]

    characters <-  sapply(data_str, is.character)
    characters_table <- characters[names(characters) %in% unlist(.NNTable$concat$table)]
  }


  .cols <- names(numerics_table)[numerics_table]
  if (length(.cols))
    data_str[, (.cols) := lapply(.cols,
              function(name) alignLeft(Format(data_str[[name]],
                format = as.character(.NNTable$NNFormat$format[, name]))))]


  .char_cols <- names(characters_table)[characters_table]
  if (!is.null(.NNTable$grouped_columns$columns))
    .char_cols <- setdiff(.char_cols, .NNTable$grouped_columns$columns)

  if (length(.char_cols))
    data_str[, (.char_cols) := lapply(.SD,  function(x) {x[is.na(x)] <- ""; x}),
             .SDcols = .char_cols]

  if (.NNTable$NNFormat$big.mark != "" | .NNTable$NNFormat$small.mark != "") {
    data_str[, (.cols) := lapply(.SD, prettyNum,
                                 big.mark = .NNTable$NNFormat$big.mark,
                                 small.mark = .NNTable$NNFormat$small.mark,
                                 preserve.width = c("individual")),
             .SDcols = .cols]

    data_str[, (.cols) := lapply(.SD, function(x) {x[grepl("NA|NaN", x)] <- ""; x}), .SDcols = .cols]
  }


  strings <- .NNTable$concat$string

  # check for blank rows and insert
  for (i in seq_along(strings)) {
    vars <- .NNTable$concat$table[names(strings)[i]][[1]]

    non_blank <- apply(data_str[, vars, with = FALSE] != "", 1, any)

    data_str <- data_str[, (vars) := lapply(.SD, function(x)
      ifelse(x == "" & non_blank, "NA", x )), .SDcols = vars ]

    # same thing but on a data.frame
    # data_str[, vars][data_str[, vars] == "" & apply(data_str[, ..vars] != "", 1, any)] <- "NA"
  }

  # create the collapsed strings
  data_str <- cbind(data_str,
                    data.table::as.data.table(sapply(strings,
                    function(string) stringr::str_glue_data(data_str, string))))

  for (i in seq_along(strings)) {
    vars <- .NNTable$concat$table[names(strings)[i]][[1]]

    data_all <- apply((data_str[, vars, with = FALSE]) == "", 1, all)
    data_str <- data_str[data_all, (names(strings)[i]) := ""]

    # same for data.frame
    # data_str[data_all, names(strings)[i]] <- ""
  }

  rename <- .NNTable$columns[.NNTable$columns %in% colnames(data_str)]

  #data_str <- data.table::as.data.table(data_str)
  #
  data_str <- (data.table::setnames(data_str, old = rename, new = names(rename)))

  .NNTable$data_str <- data_str[, unique(c(names(.NNTable$columns),
                                          intersect(.NNTable$order_columns$sort_columns, colnames(data_str)))), with = FALSE]

  .NNTable$data_str <- as.data.frame(.NNTable$data_str)

  # If any of the order variables are numeric and used in the table
  # order_data <- .NNTable$data[, colnames(.NNTable$data) %in% .NNTable$order_columns]
  # order_numeric <- sapply(order_data, is.numeric)
  #
  # order_numeric <- order_numeric & names(order_numeric) %in% .NNTable$columns
  #
  # numeric_v <- data_str[, names(order_numeric)[order_numeric], drop = FALSE]
  # colnames(numeric_v) <- paste(colnames(numeric_v), "n", sep = ".")
  # data_str <- cbind(data_str, numeric_v)


  return(.NNTable)
}


#' @importFrom Matrix t
apply_createHeader <- function(.NNTable) {

  data_str <- .NNTable$data_str[, setdiff(colnames(.NNTable$data_str),
                  c(.NNTable$remove$columns, .NNTable$remove$columns_trunc)), drop = FALSE]

  # Remove the added _trunc from the colnames
  colnames(data_str)[grep("_trunc$", colnames(data_str))] <-
    gsub("_trunc$", "", colnames(data_str)[grep("_trunc$", colnames(data_str))])

  # Get the number of columns
  ncol <- ncol(data_str)

  # Find the number of characters in the header
  rep_name = ""
  if (!is.null(.NNTable$grouped_columns$name))
    rep_name = .NNTable$grouped_columns$name
  list <- strsplit(gsub("NNTable_grouped_name", rep_name , colnames(data_str)), "__#__")
  n.header.rows <- max(sapply(list, length))

  # create the initial header matrix
  header.mat <- sapply(list, function(x) rev(c(x, rep("", n.header.rows - length(x)))))

  if (!is.matrix(header.mat)) {
    header.mat <- as.matrix(t(header.mat))
  }

  # find the columns_to_wide spacers and rename to blank
  spacers <- grep("^space.column.|^sep.column", colnames(data_str))
  prev.space <- FALSE

  for (i in seq_len(nrow(header.mat))) {
    if (i > 1) prev.space <- header.mat[i - 1, spacers - 1] != header.mat[i - 1, spacers + 1]
    header.mat[i, spacers[header.mat[i, spacers - 1] != header.mat[i, spacers + 1] | prev.space ]] <- ""
  }

  header_last_row <- header.mat[i, ]

  # clear last row if only one repeat is present for the second to last row
  if (i > 1 && length(.NNTable$columns_to_wide) > 0 && .NNTable$columns_to_wide$.remove_last_header_row) {
    if (max(rle(header.mat[ i - 1  , header.mat[ i - 1, ] != ""])$lengths) == 1) {
      header.mat[i, header.mat[ i - 1, ] != ""] <- ""

      if (all(header.mat[i - 1, header.mat[ i, ] != ""] == "")) {
        header.mat[i - 1, header.mat[ i, ] != ""] <- header.mat[i, header.mat[ i, ] != ""]
        header.mat <- header.mat[-i, ]
      }
    }
  }

  if (!is.matrix(header.mat)) {
    header.mat <- as.matrix(t(header.mat))
  }

  # remove header name for the long format column
  if (!is.null(.NNTable$columns_to_long) && length(.NNTable$columns_to_long)) {
    header.mat[header.mat == .NNTable$columns_to_long$var_name ] <-
      .NNTable$columns_to_long$display_name
  }


  ### split the header matrix into more rows in accordance with split
  if (!is.null(.NNTable$cell_split)) {

    if (.NNTable$cell_split$align %in% c("bottom", "centre")) {

      header.mat_pre <- header.mat

      n_new_lines <-
        matrix(sapply(gregexpr(.NNTable$cell_split$split, header.mat),
                      function(x) sum(x > 0)), nrow = nrow(header.mat), byrow = FALSE)



      if (.NNTable$cell_split$align == "centre") {
        n_new_lines
        n_missing_lines <- apply(n_new_lines, 1, max) - n_new_lines
        n_left_splits <- ceiling(n_missing_lines / 2)
        n_right_splits <- n_missing_lines - n_left_splits

        left_splits <- sapply(c(Matrix::t(n_left_splits)),
                              function(n) paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n),
                                                collapse = ""))

        right_splits <- sapply(c(Matrix::t(n_right_splits)),
                               function(n) paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n),
                                                 collapse = ""))

      } else {
        left_splits <- sapply(c(Matrix::t(apply(n_new_lines, 1, max) - n_new_lines)),
                              function(n) paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n),
                                                collapse = ""))

        right_splits <- rep("", length(left_splits))
      }

      header.mat <- data.table::as.data.table(
        matrix(paste0(left_splits, c(Matrix::t(header.mat)), right_splits),
               nrow = nrow(header.mat), byrow = TRUE))


      header.mat[header.mat_pre == ""] <- ""

    }


    header.mat <- data.table::as.data.table(header.mat)

    # header.mat[header.mat == gsub("\\\\", "", .NNTable$cell_split$split)] <- ""

    X <- lapply(header.mat, strsplit , split = .NNTable$cell_split$split)

    repeats <-
      apply(as.data.frame(lapply(X, function(x) vapply(x, length, 1L))), 1, max)

    repeats <- rep(seq_along(repeats), repeats)

    SetUp <- lapply(X, function(x) {
      A <- vapply(x, length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(x))
    })

    Ncol <- max(unlist(lapply(SetUp, function(y)
      y[["Mat"]][, 2]), use.names = FALSE))

    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix("Inserted Blank", nrow = nrow(header.mat), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })

    indt <- header.mat[rep(sequence(nrow(header.mat)), each = Ncol)]

    X <- lapply(X, function(y) as.vector(t(y)))

    split_cols <- colnames(header.mat)
    indt[, (split_cols) := lapply(X, unlist, use.names = FALSE)][]

    indt <- indt[!apply(indt[, split_cols, with = FALSE] == "Inserted Blank", 1, all), ]

    indt <- indt[, (split_cols) := lapply(.SD, function(x)
      ifelse(x == "Inserted Blank", "", x)), .SDcols = split_cols]


    header.mat <- as.matrix(indt)
  } else {
    repeats <- seq_len(nrow(header.mat))
  }
  ###

  if (!is.null(.NNTable$header)) {
    .NNTable$header$matrix <- header.mat
    .NNTable$header$repeats <- repeats
  } else {
    .NNTable$header <- list(matrix = header.mat, underscore = FALSE,
                            repeats = repeats)

  }




  return(.NNTable)
}


apply_alignment <- function(.NNTable) {

  if (is.null(.NNTable$alignment)) {
    .NNTable$alignment <- list()
  }

  # get size of header
  n.header.rows <- nrow(.NNTable$header$matrix)


  data <- .NNTable$data
  data_str <- .NNTable$data_str[, setdiff(colnames(.NNTable$data_str),
              c(.NNTable$remove$columns, .NNTable$remove$columns_trunc)), drop = FALSE]

  ncol <- ncol(data_str)

  header <- colnames(data_str)

  alignment = c("l", rep("c", max(0, ncol - 2)), "c")[seq_len(ncol)]

  # give names to the alignment
  names(alignment) <- colnames(data_str)

  # make sure that the characters are left aligned

  classes <- sapply(data, class)
  classes_str <- sapply(data_str, class)

  # for columns that are transposed from wide to long the name column is left aligned
  # as well as combination columns
  if (!is.null(.NNTable$columns_to_long) && length((.NNTable$columns_to_long))) {
    classes <- c(classes, structure("character", names = .NNTable$columns_to_long$var_name))

    # Check if any columns are concatinated
    if (length(setdiff(initName(.NNTable$columns_to_long$columns), colnames(data)))) {
      alignment[grep(paste0("^", .NNTable$columns_to_long$value_name), colnames(data_str))] <- "l"
    }
  }

  # Add the grouped name
  if (!is.null(.NNTable$grouped_columns)) {
    classes <- c(classes, structure("character", names = "NNTable_grouped_name"))
  }

  # if not numeric the columns are left aligned
  alignment[names(classes[!classes %in% c("numeric", "integer") & names(classes) %in% header])] <- "l"
  # in case the header is not a part of the original columns we make them left aligned
  alignment[header[!header %in% names(classes) & !grepl("^sep.column.", header)]] <- "l"


  numerics <- names(classes[classes %in% c("numeric", "integer")])

  # some columns are renamed which needs to be taken into account
  # names_conv <- unlist(.NNTable$concat$table[sapply(.NNTable$concat$table, length) == 1])
  # to_numeric <- names_conv[match(numerics, names_conv)]
  # alignment[names(to_numeric)[names(to_numeric) %in% header]] <- "c"

  names_conv <- names(.NNTable$concat$table[sapply(.NNTable$concat$table, function(x) {all( x %in% numerics)})])
  alignment[names_conv[names_conv %in% header | paste0(names_conv, "_trunc") %in% header]] <- "c"

  # columns that are really short should be centred
  small <- sapply(data_str[classes_str == "character"], function(x) max(nchar(x,  keepNA = FALSE))) <= 4
  alignment[names(small)[small]] <- "c"

  # something seems odd here
  if (!is.null(.NNTable$columns_to_long) && length((.NNTable$columns_to_long))) {

    # extract the variables
    vars <- initName(.NNTable$columns_to_long$columns)

    # if all columns are numerical we centre the created column
    # this is taken out again since we want the values to be left aligned
    #if (all(vars %in% names_conv))
    #  alignment[.NNTable$columns_to_long$value_name[.NNTable$columns_to_long$value_name %in% header]] <- "c"


    cols <- intersect(vars, names(classes))
    if (length(cols) && !any(classes[cols] %in% c("numeric", "integer"))) {

      list <- strsplit(gsub("NNTable_grouped_name", "" , colnames(data_str)), "__#__")
      cols <- sapply(list, function(x) rev(c(x, rep("", n.header.rows - length(x)))))
      if (is.matrix(cols)) {
        which <- apply(cols == .NNTable$columns_to_long$value_name, 2, any)
      } else {
        which <- cols == .NNTable$columns_to_long$value_name
      }
      alignment[which] <- "l"
    }
  }

  if (!is.null(.NNTable$alignment$alignment_specified)) {


    if (!is.null(.NNTable$grouped_columns)) {
      list <- strsplit(gsub("NNTable_grouped_name", .NNTable$grouped_columns$name,
                            gsub("_trunc$", "", names(alignment))), "__#__")
    } else {
      list <- strsplit(gsub("_trunc$", "", names(alignment)), "__#__")
    }


    l_depth <- max(sapply(list, length))
    cols <- sapply(list, function(x) rev(c(x, rep("", l_depth - length(x)))))
    if (is.matrix(cols)) {
      for (name in names(.NNTable$alignment$alignment_specified)) {
        alignment[apply(cols == name, 2, any)] <-
          .NNTable$alignment$alignment_specified[name]
      }
    } else {
      for (name in names(.NNTable$alignment$alignment_specified)) {
        alignment[cols == name] <- .NNTable$alignment$alignment_specified[name]
      }
      #which <- cols == .NNTable$columns_to_long$value_name
    }
  }

  .NNTable$alignment <- list(alignment = alignment)
  return(.NNTable)
}


apply_width <- function(.NNTable, spread = TRUE) {

  # Get data
  alignment <- .NNTable$alignment$alignment

  if ("NNTable_added_group" %in% colnames(.NNTable$data_str) &&
      .NNTable$grouped_columns$span_row) {
    NNTable_added_group <- "NNTable_added_group"
  } else {
    NNTable_added_group <- NULL
  }
  data_str <- .NNTable$data_str[, setdiff(colnames(.NNTable$data_str),
       setdiff(c(.NNTable$remove$columns, .NNTable$remove$columns_trunc),
               NNTable_added_group)), drop = FALSE]

  ncol <- ncol(data_str)

  header <- setdiff(colnames(data_str), c(NNTable_added_group, "NNTable_group_level"))

  header.mat <- .NNTable$header$matrix

  # Make sure that at least one space is added between space cols
  data_str[, grep("^space.column", colnames(data_str))] <- ""


  n.headers <- nrow(header.mat)


  #---------------------------------------------------------------------------#
  ######              Establish the width of the columns                 ######
  #---------------------------------------------------------------------------#


  # get the data counts
  if ("NNTable_added_group" %in% colnames(data_str) && .NNTable$grouped_columns$span_row) {
    do_span <- TRUE
    to_format <- data_str$NNTable_added_group == FALSE
    count_base_1 <- rbind(apply(data_str[data_str$NNTable_added_group == FALSE,
                                         setdiff(colnames(data_str), "NNTable_added_group")], 2, nchar),
                          apply(nchar(header.mat[n.headers, , drop = FALSE]), 2, max, na.rm = TRUE))

    data_str <- data_str[, setdiff(colnames(data_str), "NNTable_added_group")]
  } else {
    do_span <- FALSE
    to_format <- rep(TRUE, nrow(data_str))
    count_base_1 <- rbind(apply(data_str, 2, nchar, keepNA = FALSE),
                          apply(nchar(header.mat[n.headers, , drop = FALSE]),
                                2, max, na.rm = TRUE))
  }

  count_base <- apply(count_base_1, 2, max)
  count_base_w1s <- count_base
  count_base_w1s[grep("space.column|sep.column", names(count_base))] <- 1
  # initialize the pre/post headers
  pre_header_spaces  <- rep("", n.headers)
  post_header_spaces <- rep("", n.headers)

  # When more than one header is present we need to do something in order to
  # align the headers
  if (n.headers > 1) {

    count_mat <- nchar(header.mat)

    needed_space_count <- cbind(0, count_mat[,, drop = FALSE], 0)

    needed_space_count[, ] <- 0

    colnames(needed_space_count) <-
      c("NNTable_pre_space", colnames(data_str), "NNTable_post_space")


    for (i in rev(seq_len(n.headers)[-n.headers])) {
      needed_space_count_c <- numeric(ncol(data_str) + 2)

      names(needed_space_count_c) <-
        c("NNTable_pre_space", colnames(data_str), "NNTable_post_space")


      x <- header.mat[i ,]
      count <- rle(x)

      to   <- cumsum(count$lengths)

      from <- c(0, to[-length(to)]) + 1

      count_prev <- function(from, to) {
        sum(count_base_w1s[seq(from, to)])
      }

      prev_counts <- mapply(FUN = count_prev, from, to )

      # The added 2 is to ensure blank space between columns when two long names meet
      needed_space <- count_mat[i, from] - prev_counts

      needed_space_where <- needed_space > 0

      #needed_space[needed_space_where] <- needed_space[needed_space_where] + 2

      prev_space <- ceiling(needed_space / 2)
      post_space <- needed_space - prev_space


      if (any(needed_space_where)) {
        for (j in which(needed_space_where)) {
          seq <- seq(from[j], to[j])
          if (length(seq) == 1) {
            if (alignment[to[j]] == "l") {
              prev_space[j] <- 0
              post_space[j] <- needed_space[j]
            } else if (alignment[to[j]] == "r") {
              prev_space[j] <- needed_space[j]
              post_space[j] <- 0
            }
          }
        }
      }

      # for the first column we delete one needed space
      #prev_space[1] <- prev_space[1] - 1

      needed_space_count_c[(from - 1 + 1)[needed_space_where]] <- prev_space[needed_space_where]
      needed_space_count_c[(to   + 1 + 1)[needed_space_where]] <-
        post_space[needed_space_where] + needed_space_count_c[(to   + 1 + 1)[needed_space_where]]

      needed_space_count[i,] <- needed_space_count_c
    }

    column.chars <- count_base + apply(needed_space_count[, -c(1, ncol(needed_space_count)), drop = FALSE], 2, max)

    if (max(needed_space_count[, "NNTable_pre_space"]) > 0) {
      n.spcae <- max(needed_space_count[, "NNTable_pre_space"])

      data_str <-
        cbind(data.frame(NNTable_pre_space = paste(rep(" ", n.spcae), collapse = "")), data_str)

      # update the columns chars with the new var

      column.chars <- c(structure(n.spcae, names = "NNTable_pre_space"), column.chars)
      alignment    <- c(structure("l", names = "NNTable_pre_space"), alignment)
      header.mat   <- cbind("", header.mat)

      n.spcae <- needed_space_count[, "NNTable_pre_space"]

      pre_header_spaces <-
        sapply(max(n.spcae) - n.spcae, function(n) paste(rep(" ", n), collapse = ""))
    }

    if (max(needed_space_count[, "NNTable_post_space"]) > 0) {
      n.spcae <- max(needed_space_count[, "NNTable_post_space"])
      data_str <-
        cbind(data_str, data.frame(NNTable_post_space = paste(rep(" ", n.spcae), collapse = "")))

      # update the columns chars with the new var

      column.chars <- c(column.chars, structure(n.spcae, names = "NNTable_post_space"))
      alignment    <- c(alignment, structure("l", names = "NNTable_post_space"))
      header.mat   <- cbind(header.mat, "")


      n.spcae <- needed_space_count[, "NNTable_post_space"]

      post_header_spaces <-
        sapply(max(n.spcae) - n.spcae, function(n) paste(rep(" ", n), collapse = ""))
    }


    header <- colnames(data_str)
  } else {
    column.chars <- count_base
  }
  nested_header <- grepl("__#__", header)

  #---------------------------------------------------------------------------#
  ######              Establish the width of the table                   ######
  #---------------------------------------------------------------------------#


  res.chars <- .NNTable$page_size$page.width - sum(column.chars, na.rm = TRUE) #- (ncol - 1)

  # find the columns_to_wide spacers
  spacers <- grep("^space.column.", colnames(data_str))
  seppers <- grep("^sep.column.", colnames(data_str))



  if (res.chars - (length(spacers) + length(seppers)) < 0) {
    warning("The actual columns are too wide to fit the output")
    .NNTable$page_size$used.page.width <- (.NNTable$page_size$used.page.width -
      res.chars) + (length(spacers) + length(seppers))
    res.chars <- length(spacers) + length(seppers)
  }

  if (spread) {
    # initialise width
    width <- column.chars

    sep_width <- max(min(floor(res.chars / (length(seppers) + length(spacers))), 50), 1)

    if (length(spacers)) {
      if (sep_width <= 3) {
        sep_width <- 1
        space_width   <- column.chars[spacers] + max(floor((res.chars - (length(seppers))) / length(spacers)), 0)
      } else {
        sep_width <- min(floor(res.chars / (length(seppers) + 3 * length(spacers))), 50)
        space_width   <- column.chars[spacers] + max(floor((res.chars - sep_width * length(seppers)) / length(spacers)), 0)
      }
      width[spacers] <-  space_width
    }


    width[seppers] <- width[seppers] + sep_width

  } else {
    width <- column.chars
  }

  # add names to the width
  names(width) <- header


  #---------------------------------------------------------------------------#
  ######                  Add the required width                         ######
  #---------------------------------------------------------------------------#


  # data_str$NNTable_grouped_name

  factors <- sapply(data_str, is.factor)
  for (fac in names(factors)[factors])
    data_str[, fac] <- as.character(data_str[, fac])

  data_str[to_format,] <-
    as.data.frame(sapply(header, function(name) {
      align(x = data_str[to_format, name],
            alignment[name], width[name], keep.empty = FALSE)
    }, simplify = FALSE))


  if (do_span)
    data_str[!to_format, setdiff(colnames(data_str), "NNTable_grouped_name")] <- ""

  .NNTable$data_str[, colnames(data_str)] <- data_str

  if ("NNTable_pre_space" %in% colnames(data_str)) {
    .NNTable$data_str <- cbind(data_str[, "NNTable_pre_space", drop = FALSE], .NNTable$data_str )
  }

  #---------------------------------------------------------------------------#
  ######                  header align                                   ######
  #---------------------------------------------------------------------------#


  underscoreWidth <- function(x, width) {
    v <- rle(x)
    from_to <- c(0, cumsum(v$length))

    hline_width <- width

    # Above the adjust in accordance with needed withs
    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]
      new_width <-
        sum(width[seq]) # + length(seq) - 1 # let the extra space count

      needed_space <- nchar(v$values[i]) - new_width
      if (needed_space > 0) {
        prev_space <- ceiling(needed_space / 2)
        post_space <- needed_space - prev_space

        hline_width[from_to[i]] <-
          hline_width[from_to[i]] - prev_space
        hline_width[from_to[i + 1]] <-
          hline_width[from_to[i + 1]] + needed_space
        hline_width[from_to[i + 2]] <-
          hline_width[from_to[i + 2]] - post_space
      }
    }
    return(hline_width)
  }


  # create the alignment of the header
  headAlign <- function(x, width, alignment, nrows, any_pre_p,
                        pre_paste, post_paste, underscore, under_mat,
                        nested_header) {

    #browser()
    v <- rle(x)
    from_to <- c(0, cumsum(v$length))

    use_width <- width
    #browser()
    # Above the adjust in accordance with needed widths
    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]
      new_width <- sum(width[seq]) # + length(seq) - 1 # let the extra space count

      head_space <- max(apply(under_mat[, seq, drop = FALSE], 1, sum))

      #needed_space <- nchar(v$values[i]) - new_width
      needed_space <- head_space - new_width
      if (needed_space > 0) {
        # the needed space is distributed appropriately
        if (from_to[i] == 0 | !any(nested_header[seq]) & v$length[i] == 1 & alignment[from_to[i + 1]] == "l") {
          prev_space <- 0
          post_space <- needed_space
        } else if (is.na(from_to[i + 2]) | !any(nested_header[seq]) & v$length[i] == 1 & alignment[from_to[i + 1]] == "r") {
          prev_space <- needed_space
          post_space <- 0
        } else {
          prev_space <- use_width[from_to[i]] - min(under_mat[, from_to[i]])
          post_space <- needed_space - prev_space
        }

        use_width[from_to[i]] <- use_width[from_to[i]] - prev_space

        use_width[from_to[i + 1]] <-
          use_width[from_to[i + 1]] + needed_space

        use_width[from_to[i + 2]] <- use_width[from_to[i + 2]] - post_space
      }
    }

    hline <- character(0)
    hline_make <- .NNTable$header$underscore && underscore && max(v$lengths[v$values != ""]) > 1
    header.text <- character(0)
    #browser()
    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]
      new_width <- sum(use_width[seq]) # + length(seq) - 1 # let the extra space count


      if ((v$length[i] == 1  & nrows == 1 & i == 1) | (!any(nested_header[seq]) & v$length[i] == 1)) {
        header.text[i] <- align(x = v$values[i], alignment[from_to[i + 1]], new_width, keep.empty = FALSE)
      } else {
        header.text[i] <- align(x = v$values[i], "c", new_width, keep.empty = FALSE)
      }
      if (hline_make)
        hline[i] <- ifelse(v$values[i] != "", #& length(seq) > 1
                           hline(times = max(apply(under_mat[, seq, drop = FALSE], 1, sum))),
                           paste(rep(" ", new_width), collapse = ""))
    }
    pre_paste <- ""
    c(
      paste(c(pre_paste, header.text, post_paste), collapse = ""),
      if (hline_make) paste(c(pre_paste, hline, post_paste), collapse = "")
    )
  }

  #
  # alignment_p[setdiff(names(alignment_p), "NNTable_pre_space")[1]]
  #
  # nested_header <- grepl("__#__", header)
  # if (nested_header)

  under_mat <- width_mat <-
    matrix(rep(width, each = n.headers), nrow = n.headers, byrow = FALSE)



  if (nrow(under_mat) > 1) {
    # extract the length of the underscores
    for(i in seq_len(nrow(under_mat) - 1))
      under_mat[i, ] <- underscoreWidth(x = header.mat[i, ], width =  width_mat[i, ])

    #under_mat <- under_mat[seq_len(nrow(under_mat) - 1), ]
  }

  alignment_p <- alignment
  underscore <- c(diff(.NNTable$header$repeats), 0) > 0
  header.mat.align <- character()



  for (i in seq_len(n.headers)) {
    header.mat.align <-
      c(
        header.mat.align,
        headAlign(x = header.mat[i, ],
                  width      = width_mat[i, ],
                  alignment  = alignment,
                  nrows      = n.headers,
                  any_pre_p  = any(nchar(pre_header_spaces)),
                  pre_paste  = pre_header_spaces[i],
                  post_paste = post_header_spaces[i],
                  underscore = underscore[i],
                  under_mat  = under_mat[setdiff(seq_len(nrow(under_mat)), seq_len(i - 1)), , drop = FALSE],
                  nested_header = nested_header)
      )
  }

  header.mat.align

  .NNTable$width <- list(width = width)
  .NNTable$header$header <- header.mat.align

  return(.NNTable)
}


#' @importFrom stringr str_trim
apply_splitPages <- function(.NNTable) {

  #---------------------------------------------------------------------------#
  ######                        lines calculation                        ######
  #---------------------------------------------------------------------------#

  # Calculate title lines
  title.lines <- max(1, length(.NNTable$wrapping[["title"]]))
  title <- paste0(paste(.NNTable$wrapping[["title"]], collapse = "\n"), "\n")


  # calculate the number of header lines
  header.lines <- 2 + length(.NNTable$header$header)

  # calculate footer lines
  footer.lines <- 1 + max(1, length(.NNTable$wrapping[["footer"]])) + 1 +
    max(0, length(.NNTable$wrapping[["sys_footnote"]]))
  footer       <- paste0(paste(.NNTable$wrapping[["footer"]], collapse = "\n"), "\n")
  auto_foot    <- paste0(paste(alignRight(.NNTable$wrapping[["sys_footnote"]],
                                          width = .NNTable$page_size$page.width), collapse = "\n"), "\n")

  # body lines
  body.lines <- .NNTable$page_size$page.length - (title.lines + header.lines + footer.lines)

  .NNTable$wrapping$title_p <- title
  .NNTable$wrapping$footer_p <- footer
  .NNTable$wrapping$auto_foot <- alignRight(c(.NNTable$wrapping[["sys_footnote"]], ""),
                                             width = .NNTable$page_size$page.width)
  .NNTable$wrapping$auto_foot_p <- auto_foot

  #---------------------------------------------------------------------------#
  ######                        split the data                           ######
  #---------------------------------------------------------------------------#

  data_str <- data.table::as.data.table(.NNTable$data_str)

  group_cols <- .NNTable$grouped_columns$columns

  # if we have any grouping we ensure the breaks are reasonable
  if (!is.null(group_cols)) {
    group_cols <- c("NNTable_master_group", group_cols[c(-1, -length(group_cols))])

    # Collapse columns such that groups and sub groups can have same name
    for (i in seq_along(group_cols[-1])) {
      data_str <- tidyr::unite_(data_str, paste(group_cols[-1][i], "split", sep = "_"),
                                group_cols[1:(i + 1)], remove = FALSE)
    }

    # Make sure that we split page according to the new columns
    if (length(group_cols) > 1)
      group_cols <- c("NNTable_master_group", paste(group_cols[-1], "split", sep = "_"))

    wrongBreaks <- function(data, group_cols = "NNTable_master_group", correction = 0) {

      # get the outermost grouping
      grouping_col <- group_cols[1]
      grouping     <- data[, get(grouping_col)]
      group_size   <- table(grouping)[unique(grouping)]
      group_breaks <- cumsum(group_size) + correction


      wrong_breaks <- function(group_breaks, group_size, body.lines,
                               missings = list(), added = 0,
                               corrected_levels = character(0)) {

        fromV <- c(0, group_breaks[-length(group_breaks)]) + 1
        names(fromV) <- NULL
        to   <- group_breaks

        # correction for first break falling later than page 1
        body_check <- body.lines * (floor(fromV / body.lines) + 1)


        wrong_break <- fromV < body_check & to > body_check & # wrong break
          to - fromV < body.lines  # check that it is breakable

        any_wrong_break <- any(wrong_break)

        #a <- data.frame(fromV, to, body_check, wrong_break)
        sub_missing <- list()

        check_groups <- setdiff(names(wrong_break), corrected_levels)

        if (length(group_cols) > 1) {
          pot_unbreak <- (fromV < body_check & to > body_check)[check_groups]
          if (any(pot_unbreak)) {
            if (!any_wrong_break ||
                names(group_breaks[check_groups][wrong_break][1]) !=
                names(group_breaks[check_groups][pot_unbreak][1])) {

              # Get the unbreakable name
              unbreakable <- names(group_breaks[check_groups][pot_unbreak][1])


              # The line number on the page is found
              correction <- max(0, fromV[which(names(wrong_break) == unbreakable)[1]] -
                                  body.lines * (ceiling(fromV[which(names(wrong_break) == unbreakable)[1]] / body.lines) - 1) - 1)


              # Get the wrongbreaks within the subgroup
              sub_missing <- wrongBreaks(data = data[data[, get(grouping_col)] == unbreakable, ],
                                         group_cols = group_cols[-1], correction = correction)


              # If any new lines needs to be added they are added to the overall global missing
              # and the line counter is corrected to match the new number
              if (length(sub_missing)) {
                # add the sub_missing to the missing
                for (name in names(sub_missing))
                  missings[[name]] <- c(missings[[name]], sub_missing[[name]])

                group_breaks <- group_breaks + sum(unlist(sub_missing))

                # repeat the check for wrong breaks
                fromV <- c(0, group_breaks[-length(group_breaks)]) + 1
                names(fromV) <- NULL
                to   <- group_breaks

                # correction for first break falling later than page 1
                body_check <- body.lines * (floor(fromV / body.lines) + 1)

                wrong_break <- fromV < body_check & to > body_check & # wrong break
                  to - fromV < body.lines  # check that it is breakable
              }

            }
          }
        }

        if (any_wrong_break) { # I need to get in here when something happens above
          missings[[grouping_col]][[names(group_breaks[wrong_break][1])]] <-
            body.lines * ceiling(fromV[wrong_break][1] / body.lines) - fromV[wrong_break][1] + 1

          group_breaks <- group_breaks[group_breaks >= fromV[wrong_break][1]] +
            missings[[grouping_col]][[names(group_breaks[wrong_break][1])]] -
            body.lines * ceiling(fromV[wrong_break][1] / body.lines)

          group_size <- group_size[group_breaks >= fromV[wrong_break][1]]

          return(wrong_breaks(group_breaks, group_size, body.lines, missings))
        } else {
          if (length(sub_missing)) { # try to get a grip on what is happening
                                     # when no breaks occur in the outer level
                                     # but one does occur in the inner

           # group_breaks <- group_breaks[setdiff(names(group_breaks), unbreakable)]

          #  group_size <- group_size[setdiff(names(group_size), unbreakable)]

            return(wrong_breaks(group_breaks, group_size, body.lines, missings,
                                corrected_levels = c(corrected_levels, unbreakable)))
          } else {
            return(missings)
          }
        }
      }

      wrong_breaks(group_breaks, group_size, body.lines)
    }

    wrong_break_list <- wrongBreaks(data = data_str, group_cols)

    # Find the blank space inserted above a new group

    # for (grouping_col in group_cols)
    #   numerics <- which(data_str[, get(grouping_col)]  %in% names(wrong_break_list[[grouping_col]]) &
    #                     stringr::str_trim(data_str[, get("NNTable_grouped_name")]) == "" &
    #                     data_str[, get("NNTable_added_blank")] == "")
    numerics <-
      lapply(group_cols,  function(grouping_col)
        if (grouping_col == "NNTable_master_group") {
          which(data_str[, get(grouping_col)]  %in% names(wrong_break_list[[grouping_col]]) &
                  stringr::str_trim(data_str[, get("NNTable_grouped_name")]) == "" &
                  data_str[, get("NNTable_added_blank")] == "")
        } else {

          potentials <- data_str[, get(grouping_col)]  %in% names(wrong_break_list[[grouping_col]]) &
            stringr::str_trim(data_str[, get("NNTable_grouped_name")]) == "" &
            data_str[, get("NNTable_added_blank")] == "Y"

          data_str[, get(grouping_col)][potentials]

          which(potentials)[!duplicated( data_str[, get(grouping_col)][potentials])]
        }
      )

    base <- data_str[rep(unlist(numerics), unlist(wrong_break_list)), ]

    .NNTable$remove$split_page <- group_cols
    data_str <- rbind(base, data_str)
    data_str <- data.table::setorderv(data.table::as.data.table(data_str), c(.NNTable$order_columns$sort_columns))

  }

  # Ensure that we have data.frame
  data_str <- as.data.frame(data_str, stringsAsFactors = FALSE)

  # remove unwanted colums
  data_str <- data_str[, setdiff(colnames(data_str), c(.NNTable$remove$columns,
                                                       .NNTable$remove$columns_trunc,
                                                       .NNTable$remove$split_page)), drop = FALSE]

  # clear colnames
  colnames(data_str) <- NULL

  # split the data into list in accordance with body.lines
  pages  <- rep(seq_len(ceiling(nrow(data_str) / body.lines)), each = body.lines)[seq_len(nrow(data_str))]
  data_split <- lapply(split(x = seq_len(nrow(data_str)), f = pages, drop = FALSE), function(line) data_str[line, ])

  .NNTable$data_split <- data_split
  return(.NNTable)
}

apply_add_separators <- function(.NNTable) {

  # extract the data.frame as a data.table
  data_str <- data.table::as.data.table(.NNTable$data_str)

  # Extract the columns wanted in the final table
  remove <- c(.NNTable$remove$columns,
              .NNTable$remove$columns_trunc)

  cola <- setdiff(colnames(data_str), remove)

  # subset data_str accordingly
  a <- data_str[, (cola), with = FALSE]


  # no sep columns are wanted next to space columns
  spacers <- grep("^space.column.", cola)
  non_space_columns <- setdiff(seq_len(ncol(a)), spacers)

  # For all non-space colums that are next to each other a sep column is needed
  width_columns <- non_space_columns[c(0, diff(non_space_columns)) == 1]

  if (length(width_columns)) {

    # # Extract the column headers from the titles
    # space_char <- sapply(gregexpr("__#__", cola), function(x) {x[1]} )
    #
    # # Find the number where the header separators
    # pasters <- substring(cola, space_char)
    # pasters[space_char < 0] <- ""
    #
    # # If the column headers are identical for the before and after columns
    # # it should be added to the separator name
    #
    # p1 <- pasters[width_columns - 1]
    # p2 <- pasters[width_columns]
    # pf <- rep("", length(p1))
    # pf[p1 == p2] <- p1[p1 == p2]
    #
    # new_cols <- paste0("sep.column.", width_columns - 1, ".", width_columns, pf)

    split_cols <- strsplit(cola, "__#__")

    pf <- rep("", length(width_columns))
    pf_pot <- which(sapply(split_cols, length) > 1)[-1]
    wh_split <- intersect(width_columns, c(pf_pot))


    for (i in seq_along(wh_split)) {

      j <- wh_split[i]

      if (length(split_cols[[j]]) == length(split_cols[[j - 1]])) {
        wh <- split_cols[[j - 1]][(seq_len(length(split_cols[[j - 1]]))[-1])] ==
          split_cols[[j]][(seq_len(length(split_cols[[j]]))[-1])]

        if (any(!wh) & !all(!wh)) {
          wh[seq_len(max(which(!wh)))] <- FALSE
        }


        vec <- split_cols[[j]]
        vec[!c(FALSE, wh)] <- ""

        pf[j == width_columns] <- paste(vec, collapse = "__#__")
      }
    }

    new_cols <- paste0("sep.column.", width_columns - 1, ".", width_columns, pf)


    # assign the new columns to a
    a[, (new_cols) := ""]
    data_str[, (new_cols) := ""]

    # reorder a according to the wanted column order
    new_ord <-
      apply(data.frame(
        a = width_columns - 1,
        b = width_columns
      ), 1, mean)


    col_order <- c(
      names(sort(structure(c(seq_along(cola), new_ord), names = colnames(a)))),
      setdiff(colnames(.NNTable$data_str), cola)
    )

    # assign the reordered data.frame back to .NNTable
    .NNTable$data_str <- as.data.frame(data.table::setcolorder(data_str, col_order))
  }

  return(.NNTable)
}

