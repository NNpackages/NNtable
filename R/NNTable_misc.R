
#' Create a tree structure of the table
#'
#' @param .NNTable An \code{NNTable} object
#'
#' @return The \code{NNTable} object with the tree added
#' @keywords internal
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


# Function for finding nested columns
get_unlist_vars <- function(x, current = character(0)) {
  if (is.recursive(x)) {
    current <- c(current, names(x))
    for (i in seq_along(x))
      current <- c(get_unlist_vars(x[[i]], current))
  }
  else {
    current <- c(current, x)
  }
  return(current)
}

get_unlist_names <- function(x, current = character(0)) {
  if (is.recursive(x)) {
    current <- c(current, names(x))
    for (i in seq_along(x))
      current <- c(get_unlist_vars(x[[i]], current))
  }
  return(current)
}

get_unlist_names_only <- function(x, current = character(0)) {
  if (is.recursive(x)) {
    current <- c(current, names(x))
    for (i in seq_along(x))
      if (is.recursive(x[[i]]))
        current <- c(get_unlist_names_only(x[[i]], current))
  }
  return(current)
}


get_columns <- function(.NNTable, new_data = NULL) {

  if (is.null(new_data))
    x <- .NNTable$data
  else
    x <- new_data

  columns <- colnames(x)

  table_form <- .NNTable$columns

  # get the names of the columns not in x
  missing_names <- names(table_form[!table_form %in% columns])

  if (length(missing_names)) {

    table_list <- lapply(table_form, function(name) if (name %in% columns) {
      out <- table_form[table_form == name]; names(out) <- NULL; out})

    strings <- list()

    for (name in missing_names) {

      # find colums that match part of the string
      matches   <- stringr::str_locate(table_form[name], columns[order(nchar(columns), decreasing = TRUE)])
      character <- stringr::str_sub(table_form[name], matches)
      possible  <- seq_along(character)[!is.na(character)]

      # Only use the column matches corresponding to the longest matches
      if (sum(!is.na(character)) > 1)
        for (i in seq_along(character)[!is.na(character)][-1])
          if (any(matches[i, 1] <= matches[possible[possible < i], 2] &
                  matches[i, 2] >= matches[possible[possible < i], 1])) {
            matches[i, ] <- NA
            possible <- setdiff(possible, i)
          }

      if (all(is.na(matches))) {
        table_list[[name]] <- NULL
        strings[[name]]    <- name
      } else {

        # Get only those with match
        sub_match <- matches[!is.na(matches[, 1]), , drop = FALSE]

        if (nrow(sub_match) > 1)
          sub_match <- sub_match[order(sub_match[,1 ]), ]

        table_list[[name]] <- stringr::str_sub(table_form[name], sub_match)

        string <- stringr::str_sub(table_form[name], 1, min(sub_match) - 1)

        string <- paste0(string, "{", stringr::str_sub(table_form[name], start = sub_match[1, 1], end = sub_match[1, 2]), "}")

        # intermediate stings
        if (nrow(sub_match) > 1)
          for (i in seq_len(nrow(sub_match))[-1]) {
            if (sub_match[i, 1] > sub_match[i - 1, 2])
              string <- paste0(string, stringr::str_sub(table_form[name], start =  sub_match[i - 1, 2] + 1,
                                                        end = sub_match[i, 1] - 1))
            string <- paste0(string, "{", stringr::str_sub(table_form[name], start = sub_match[i, 1], end = sub_match[i, 2]), "}")
          }

        # Finalising the string
        string <- paste0(string, stringr::str_sub(table_form[name], max(sub_match) + 1, -1L))

        strings[[name]] <- string
      }
    }
    .NNTable$concat <- list(table = table_list, strings = strings)
  } else {
    .NNTable$concat <- list(table = as.list(.NNTable$columns))
  }
  return(.NNTable)
}


#stringi::stri_unescape_unicode("\u2014")

#' Line to separate header and footnote from table
#'
#' @param symbol \code{character} symbol for the line
#' @param times the number of times the \code{symbol} should be repeated.
#'
#' @return the horizontal line
#' @importFrom stringi stri_unescape_unicode
#' @keywords internal
hline <- function(symbol = "horizontal", times = 141) {
  if (symbol == "horizontal")
    symbol <- "\u2014"
  paste(rep(symbol, times), collapse = "")
}



#' Get the depth of a list
#'
#' @param list The list to determine the with
#' @param current_depth The starting value of depth
#'
#' @return \code{numeric} indicating the deepest neting level of the list
#' @keywords internal
depth <- function(list, current_depth=0) {
  if (!is.list(list)) {
    return(current_depth)
  } else {
    return(max(unlist(lapply(list, depth, current_depth = current_depth + 1))))
  }
}

# get first non list element of list. This is used instead of unlist to preserve
# names
initName <- function(x) {
  if (is.atomic(x)) return(x)
  initName(x[[1]])
}

