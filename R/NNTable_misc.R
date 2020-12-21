

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



# `[.NNTable` <- function(x, i, j) {
#
#  x$data  <- `[.data.frame`(x$data, i, j, FALSE)
#
#  if ("NNFormat" %in% names(x))
#    x <- do.call(Format, as.list(x$NNFormat$call))
#
#   x
#
# }

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


#' Title
#'
#' @param x The plot to save as png
#'
#' @importFrom grDevices png dev.off
basePNGplot <- function(x) {
  filename <- tempfile(fileext = ".png")[1]
  grDevices::png(filename = filename)
    x
  dev.off()
  system(paste("open", filename))
  filename
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

      # Only use the column matches corresponding to the longes matches
      if (sum(!is.na(character)) > 1)
        for (i in seq_along(character)[!is.na(character)][-1])
          if (any(matches[i, 1] <= matches[possible[possible < i], 2] & 
                  matches[i, 2] >= matches[possible[possible < i], 1]))
            matches[i, ] <- NA

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



split_format <- function(format) {
  fsplit <- strsplit(as.character(format), "[%.f]")
  num <- sapply(fsplit, "[", 2)
  dec <- sapply(fsplit, "[", 3)

  num[num == ""] <- 0
  dec[dec == ""] <- 0

  out <- list(num = as.numeric(num), dec = as.numeric(dec), format = as.character(format))
  out$num <- pmax(0, out$num - out$dec - 1)

  out
}


fillFormat <- function(format) {
  fsplit <- strsplit(as.character(format), "[%.f]")
  num <- sapply(fsplit, "[", 2)
  dec <- sapply(fsplit, "[", 3)

  num[num == ""] <- 0
  dec[dec == ""] <- 0

  out <- list(num = as.numeric(num), dec = as.numeric(dec))

  out$num[is.na(out$num)] <- max(out$num, na.rm = TRUE)
  out$dec[is.na(out$dec)] <- max(out$dec, na.rm = TRUE)

  out$num <- pmax(0, out$num - out$dec - 1)

  num <- out$num
  num[num == 0] <- ""
  format <- paste0("%", num, ".", out$dec, "f")
}



updatejointFormat <- function(data, format_data, dec, big.mark = "", small.mark = "") {

  # find the numeric columns
  numerics <- sapply(data, is.numeric)

  # find the column match as the non numeric column in both datasets
  column_match <- intersect(colnames(data)[!numerics], colnames(format_data))

  # get the order of the format_data
  fmt_order <- match(apply(data[, column_match, drop = FALSE],         1, paste, collapse = ""),
                     apply(format_data[, column_match, drop = FALSE], 1, paste, collapse = ""))

  # restrict to the applicable colnames
  old_format <- format_data[, intersect(colnames(data), colnames(format_data)), drop = FALSE]
  new_format <- format_data[fmt_order, intersect(colnames(data), colnames(format_data)), drop = FALSE]
  new_format[, column_match] <- data[, column_match]

  if (all(is.na(fmt_order))) {
    filled_format <- as.data.frame(lapply(rbind(old_format, new_format)[, setdiff(colnames(new_format), column_match)], fillFormat))

    new_format[, setdiff(colnames(new_format), column_match)] <-
      filled_format[-seq_len(nrow(old_format)), ]

  } else {
    new_format[, setdiff(colnames(new_format), column_match)] <-
      as.data.frame(lapply(new_format[, setdiff(colnames(new_format), column_match), drop = FALSE], fillFormat))
  }

  # Get the number of characters in front of the .
  x <- unlist(data[, numerics])
  x <- x[!is.na(x)]
  
  if (big.mark != "") {
    num_all <- max(nchar(prettyNum(c(floor(min(x)), max(ceiling(x))), big.mark = big.mark, scientific = FALSE)))
  } else {
    num_all <- max(nchar(prettyNum(c(floor(min(x)), max(ceiling(x))), scientific = FALSE)))
  }

  
  # Get the decimals for those missing
  x <- unlist(data[, numerics & !(colnames(data) %in% colnames(format_data))])
  missing_dec <- min(dec, decimalPlaces(x))

  if (small.mark != "" & missing_dec > 0) {
    missing_dec <- missing_dec + floor((missing_dec - 1) / 5)
  }
  
  
  # Get the decimals for the specified
  spec_fmt <- unique(as.character(unlist(new_format[, setdiff(colnames(new_format), column_match)])))
  spec_fmt_max <- lapply(split_format(spec_fmt), max)

  #adjusted_num <- num_all + max(missing_dec, spec_fmt_max$dec) + 1

  new_dec <- sapply(new_format[, setdiff(colnames(new_format), column_match), drop = FALSE],
                    function(x) split_format(x)$dec)

  # Create the adjusted and new format data frames
  out_format_adj <- sapply(as.data.frame(new_dec), function(dec2) paste0("%", num_all + dec2 + ifelse(dec2 > 0, 1, 0), ".", dec2, "f"))
  out_format_new <- data[, numerics & !(colnames(data) %in% colnames(format_data)), drop = FALSE]
  out_format_new[,] <- paste0("%",  num_all + missing_dec + ifelse(missing_dec > 0, 1, 0), ".", missing_dec, "f")

  # bind the created formats together
  cbind(new_format[, column_match, drop = FALSE], out_format_adj, out_format_new)[, c(column_match, names(numerics)[numerics])]
}


updateUniFormat <- function(data, format_data, dec, big.mark = "", small.mark = "") {
  
  # find the numeric columns
  numerics <- sapply(data, is.numeric)

  missing <- setdiff(colnames(data)[numerics], colnames(format_data))
  
  if (length(missing)) {
    mis_form <- as.data.frame(t(structure(rep(paste0("%.", dec, "f"), length(missing)), names = missing)))
    
    if (nrow(format_data) == 1)
      format_data <- dplyr::bind_cols(format_data, mis_form)
    else 
      format_data <- dplyr::bind_rows(format_data, mis_form)
  }

  # find the column match as the non numeric column in both datasets
  column_match <- intersect(colnames(data)[!numerics], colnames(format_data))

  # get the order of the format_data
  fmt_order <- match(apply(data[, column_match, drop = FALSE],        1, paste, collapse = ""),
                     apply(format_data[, column_match, drop = FALSE], 1, paste, collapse = ""))


  old_format <- format_data[, intersect(colnames(data), colnames(format_data))]
  new_format <- format_data[fmt_order, intersect(colnames(data), colnames(format_data))]

  new_format[, column_match] <- data[, column_match]

     
  
  if (all(is.na(fmt_order))) {
    filled_format <- as.data.frame(lapply(rbind(old_format, new_format)[,
                      setdiff(colnames(new_format), column_match), drop = FALSE], fillFormat))

    new_format[, setdiff(colnames(new_format), column_match)] <-
      filled_format[-seq_len(nrow(old_format)), ]

    all.na <- sapply(new_format, function(x)  all(is.na(x)))
    
  } else {
      if (anyNA(format_data[, column_match])) {
        na_fmt <- format_data[is.na(format_data[[column_match]]), ][1, ]
        
        new_format[new_format[[column_match]] %in% 
                     setdiff(new_format[[column_match]], format_data[[column_match]]), 
                   setdiff(colnames(na_fmt), column_match)] <- na_fmt[, setdiff(colnames(na_fmt), column_match)]
          
      }
    
      all.na <- sapply(new_format, function(x)  all(is.na(x)))
      if (length(all.na))
      new_format[, setdiff(colnames(new_format), c(column_match, names(all.na[all.na])))] <-
        as.data.frame(lapply(
          new_format[, setdiff(colnames(new_format), c(column_match, names(all.na[all.na]))), drop = FALSE], fillFormat))
  }

  data_form <- sapply(data[, numerics, drop = FALSE], guessFormat, dec = dec, big.mark = big.mark, small.mark = small.mark)

  in_format <- colnames(new_format)[colnames(new_format) %in% names(numerics)[numerics] & 
                                      !colnames(new_format) %in% names(all.na[all.na])]

  new_format[, in_format] <-
    as_tibble(sapply(in_format, function(name)
      adjustFormat(new_format[, name], data_form[name])))

  
  missing <- as.data.frame(t(data_form[setdiff(names(data_form) , colnames(format_data))]))
  cbind(new_format, missing)
}


adjustFormat <- function(format, data_format) {
  app_format  <- split_format(data_format)
  spec_format <- split_format(unlist(format))

  num <- max(app_format$num, spec_format$num) # + max(app_format$dec, spec_format$dec) + 1
  dec <- spec_format$dec

  ifelse
  paste0("%", num + ifelse(dec > 0, dec + 1, 0), ".", dec, "f")
}




#' Format an NNTable
#'
#' @param x An \code{NNTable} object
#' @param ... Not currently used
#' @param format_data A \code{data.frame} with formats to use on the tables
#' @param group_by The formatting can be grouped accoding to a column. This
#'   means that decimal places will only be aligned within each group and not
#'   the total column
#' @param dec Number of decimals to use as base
#' @param big.mark \code{character} specifying the character used to separate 1000 in a big number
#' @param small.mark \code{character} specifying the character to use to separate every 5th decimal.
#'
#' @return The NNTable with the format to be used for each value attached
#' @export
#'
#' @examples
#' \dontrun{
#' nrows <- 200
#' letters_col <- character(122)
#' for(i in 1:122)
#'   letters_col[i] <- sample(paste(sample(letters, 25, replace = TRUE), collapse = ""))
#'
#' tab <- data.frame(sex = as.character(sample(c("Female", "Male"), nrows, replace = TRUE)),
#'                   by_var = sample(c("type a", "type b"), nrows, replace = TRUE),
#'                   letter = as.character(sample(letters_col, nrows, replace = TRUE)),
#'                   n    = round(rnorm(nrows, 10, 20)),
#'                   mean = rnorm(nrows, 10, 20),
#'                   sd   = exp(rnorm(nrows, 0, 1)),
#'                   min  = rnorm(nrows, 10, 20),
#'                   max  = rnorm(nrows, 10, 20),
#'                   stringsAsFactors = FALSE)
#'
#' NNtable <- NNTable(tab, columns =  c("sex", "by_var", "letter", "n",
#'                                 "Mean (SD)" = "mean (sd)", "min ; max"))
#'
#'
#' n_form <- length(unique(tab$letter))
#' format_data <- data.frame(letter = unique(tab$letter),
#'                           n    = "%.0f",
#'                           mean = sample(c("%.1f", "%.2f"), n_form, replace  = TRUE),
#'                           sd   = sample(c("%.1f", "%.2f"), n_form, replace  = TRUE),
#'                           min  = sample(c("%.1f", "%.2f"), n_form, replace  = TRUE),
#'                           stringsAsFactors = FALSE)
#' format_data$max = format_data$min
#'
#' NNTable <- Format(NNtable, format_data = format_data)
#' }
#' @importFrom data.table ':='
Format.NNTable <- function(x, ..., format_data = NULL, group_by = NULL, dec = 3, big.mark = "", small.mark = "") {

  .NNTable <- x
  x <- data.table::as.data.table(.NNTable$data_str)

  # Get the concatination info
  concat <- .NNTable$concat

  # Get the columns that needs to be formated together
  combine <- initName(.NNTable$columns_to_long$columns)


  # find the numeric columns

  numerics <- sapply(x, is.numeric)
  numerics_table <- numerics[names(numerics) %in% unlist(concat$table)]

  # column names that are not concatinated are found
  first_cols <- sapply(concat$table, function(x) x[1])
  combine_first <- first_cols[names(first_cols) %in% combine]
  
  if (is.null(format_data)) {

    # function for creating the format data from scratch
    create_format_data <- function(x, keep = character(0)) {
      out_format <- data.frame(matrix(ncol = length(c(unlist(concat$table), keep)),
                                      nrow = nrow(x)),
                               stringsAsFactors = FALSE)

      
      row.names(out_format) <- NULL
      colnames(out_format) <- c(unlist(concat$table), keep)

      out_format[, c(unlist(concat$table), keep) ] <- x[, c(unlist(concat$table), keep), with = FALSE]

      joint <- combine_first[combine_first %in% names(numerics_table)[numerics_table]]
      if (length(joint))
        out_format[, joint] <- guessFormat(unlist(x[, joint, with = FALSE]), dec = dec, big.mark = big.mark, small.mark = small.mark)

      non_joint <- setdiff(names(numerics_table)[numerics_table], joint)
      
      if (length(non_joint)) {
        formats <- sapply(x[, non_joint, with = FALSE, drop = FALSE], function(x)
          guessFormat(x, dec = min(dec, decimalPlaces(x)), big.mark = big.mark, small.mark = small.mark))
        
        out_format[, non_joint] <- as.data.frame(t(formats),
                                                 stringsAsFactors = FALSE)
      }
      
      return(out_format)
    }


    # add an ordering to ensure the same order is supplied after grouping
    x$keep_order_xy <- seq_len(nrow(x))

    if (!is.null(group_by)) {
      group <- apply(x[, group_by, drop = FALSE, with = FALSE], 1, function(x) paste(x, collapse = ""))
      data_split <- lapply(split(x = seq_len(nrow(x)), f = group, drop = FALSE), function(line) x[line, ])

      out_format <- data.table::rbindlist(lapply(data_split, create_format_data, keep = "keep_order_xy"))
    } else {
      out_format <- data.table::as.data.table(create_format_data(x, keep = "keep_order_xy"))
    }
    data.table::setorder(out_format, "keep_order_xy")
    out_format <- out_format[, keep_order_xy := NULL]
  } else {

    create_format_data <- function(x, keep = character(0)) {
      out_format <- data.frame(matrix(ncol = length(c(unlist(concat$table), keep)),
                                      nrow = nrow(x)),
                               stringsAsFactors = FALSE)

      row.names(out_format) <- NULL
      colnames(out_format) <- c(unlist(concat$table), keep)

      out_format[, c(unlist(concat$table), keep) ] <- x[, c(unlist(concat$table), keep), with = FALSE]


      # find the column matches as the non numeric columns in both datasets
      column_match <- intersect(colnames(x)[!numerics], colnames(format_data))

      # column names that are not concatinated are found
      first_cols <- sapply(concat$table, function(x) x[1])
      combine_first <- first_cols[names(first_cols) %in% combine]

      joint <-  combine_first[combine_first %in% names(numerics_table)[numerics_table]]
      if (length(joint))
        out_format[, joint] <- 
        updatejointFormat(data = as.data.frame(x)[, c(column_match, joint)], 
                          format_data, dec = dec, big.mark = big.mark, small.mark = small.mark)[, joint]


      non_joint <- setdiff(names(numerics_table)[numerics_table], joint)
      if (length(non_joint)) {
        out_temp <- 
          updateUniFormat(data = as.data.frame(x)[, c(column_match, non_joint)], format_data, dec = dec,
                          big.mark = big.mark, small.mark = small.mark)#[, non_joint]
        out_format[, colnames(out_temp)] <- out_temp
        
      }
      return(out_format)
    }

    # add an ordering to ensure the same order is supplied after grouping
    x$keep_order_xy <- seq_len(nrow(x))

    if (!is.null(group_by)) {
      group <- apply(x[, group_by, with = FALSE, drop = FALSE], 1, function(x) paste(x, collapse = ""))
      data_split <- lapply(split(x = seq_len(nrow(x)), f = group, drop = FALSE), function(line) x[line, ])

      out_format <- data.table::rbindlist(lapply(data_split, create_format_data, keep = "keep_order_xy"))
    } else {
      out_format <- data.table::as.data.table(create_format_data(x, keep = "keep_order_xy"))
    }
    data.table::setorder(out_format, "keep_order_xy")
    out_format <- out_format[, keep_order_xy := NULL]

  }

  .NNTable$NNFormat <- list(format = as.data.frame(out_format), format_data = format_data,
                           big.mark = big.mark, small.mark = big.mark, group_by = group_by)
  return(.NNTable)
}


#' Center text in monospaced output
#'
#' @param text \code{character} to center
#' @param width the number of characters of the column
#' @param type should text be trimmed or not
#' @param fill Should trailing blanks be added
#' @param keep.empty should completely empty comlumns be left blank
#'
#' @return The \code{character} \code{text} with added spaced in front
alignCenter <- function(text, width = max(nchar(text)), 
                        type = c("non-trimmed", "trimmed"), 
                        fill = TRUE, 
                        keep.empty = TRUE) {
  textWidth <- nchar(text)
  spare <- width - textWidth
  nspaces   <- pmax(ceiling(spare/2), 0)

  spaces    <- sapply(nspaces, function(n) paste(rep(" ", n), collapse = ""))
  if (fill) {
    
  spaces.trail <-
    sapply(pmax(width - textWidth - nspaces, 0), function(n) paste(rep(" ", n), collapse = ""))
  } else {
    spaces.trail <- ""
  }

  c.text    <- paste(spaces, text, spaces.trail, sep = "")

  if (keep.empty) {
    c.text[textWidth == 0] <- ""
  }
  
  return(c.text)
}

#' Add space on left for monospaced output
#'
#' @param text \code{character} to center
#' @param width the number of characters of the column
#' @param type should text be trimmed or not
#' @param keep.empty should completely empty comlumns be left blank
#'
#' @return The \code{character} \code{text} wtihout leading and trailing blanks
alignRight <- function(text, width = max(nchar(text)), type = c("non-trimmed", "trimmed"), keep.empty = TRUE) {
  textWidth <- nchar(text)
  nspaces   <- floor((width - textWidth))
  spaces    <- sapply(nspaces, function(n) paste(rep(" ", n), collapse = ""))
  l.text    <- paste(spaces, text, sep = "")

  if (keep.empty) {
    l.text[textWidth == 0] <- ""
  }
  
  return(l.text)
}

#' Add space on right for monospaced output
#'
#' @param text \code{character} to center
#' @param width the number of characters of the column
#' @param type should text be trimmed or not
#' @param sep The inserted space generator, defaults to " ".
#' @param keep.empty should completely empty comlumns be left blank
#'
#' @return The \code{character} \code{text} wtihout leading and trailing blanks
alignLeft <- function(text, width = max(nchar(text)), 
                      type = c("non-trimmed", "trimmed"), 
                      sep = " ", 
                      keep.empty = TRUE) {
  textWidth <- nchar(text, keepNA = FALSE)
  nspaces   <- floor((width - textWidth))
  spaces    <- sapply(nspaces, function(n) paste(rep(sep, n), collapse = ""))
  r.text    <- paste(text, spaces, sep = "")

  if (keep.empty) {
    r.text[textWidth == 0] <- ""
  }
  
  return(r.text)
}

#' Align text in monospaced output
#'
#' @param x \code{character} to center
#' @param width the number of characters of the column
#' @param type should text be trimmed or not
#' @param alignment the type of alignment.
#' @param keep.empty should completely empty comlumns be left blank
#'
#' @return The \code{character} \code{x} with added spaces
#' @export
align <- function(x, alignment = c("left", "center", "right"),
                  width = max(nchar(text)),
                  type = c("non-trimmed", "trimmed"),
                  keep.empty = TRUE) {

  x <- as.character(x)
  x[is.na(x)] <- ""
  alignment <- match.arg(alignment)
  type      <- match.arg(type)

  text      <- switch(type,
                      trimmed  = trimws(x),
                      x)

  if (alignment == "left")
    return(alignLeft(text, width, type, keep.empty = keep.empty))

  if (alignment == "center")
    return(alignCenter(text, width, type, keep.empty = keep.empty))

  if (alignment == "right")
    return(alignRight(text, width, type, keep.empty = keep.empty))
}

#stringi::stri_unescape_unicode("\u2014")

#' Line to separate header and footnote from table
#'
#' @param symbol \code{character} symbol for the line
#' @param times the number of times the \code{symbol} should be repeated.
#'
#' @return the horizontal line
#' @importFrom stringi stri_unescape_unicode
hline <- function(symbol = "horizontal", times = 141) { 
  if (symbol == "horizontal")
    symbol <- "\u2014"
  paste(rep(symbol, times), collapse = "")
}


decimalPlaces <- function(x) {
  if (is.null(x)) return(0)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(0) 
  if (max(abs(x - round(x))) > .Machine$double.eps^0.5) {
    decif <- function(x) {
      ifelse(length(x) == 2, nchar(x[[2]]), 0)
    }
    max(sapply(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE), decif))
  } else {
    return(0)
  }
}

guessFormat <- function(x, dec = min(3, max_dec), max_dec = decimalPlaces(x), big.mark = "", small.mark = "") {
  if (is.numeric(x)) {
    if (all(is.na(x))) return(NA)
    
    x <- x[!is.na(x)]
    
    if (small.mark != "" & dec > 0) {
      dec <- dec + floor((dec - 1) / 5)
    }
    
    if (big.mark != "") {
      num <- max(nchar(prettyNum(c(floor(min(x)), max(ceiling(x))), big.mark = big.mark, scientific = FALSE))) + dec
    } else {
      num <- max(nchar(prettyNum(c(floor(min(x)), max(ceiling(x))), scientific = FALSE))) + dec
    }

    if (dec > 0)
      num <- num + 1 
    
    return(paste0("%", num, ".", dec, "f"))
  } else {
    return("string")
  }
}

#' Format numeric
#'
#' @param x The vector to format
#' @param ... additional arguments passed on
#'
#' @return The formatted vector
#' @export
#'
#' @examples
#' Format(c("ad", "chrjf"))
#' Format(c(rnorm(10, 10, 20)))
Format <- function(x, ...) {
  UseMethod("Format")
}

#' Format default
#'
#' @param x The vector to format
#' @param ... additional arguments passed on
#'
#' @return The formatted vector
#' @export
Format.default <- function(x, ...) as.character(x)

#' Format numeric
#'
#' @param x \code{numeric} to
#' @param format The format to use
#' @param ... additional arguments passed on
#'
#' @return The formated vector
#' @export
Format.numeric <- function(x, ..., format = guessFormat(x, dec = min(3, decimalPlaces(x)))) {
  y <- x
  y[!is.na(x)] <- sprintf(format[!is.na(x)], x[!is.na(x)])
  
  y[is.na(y) | grepl("NA", y) ] <- ""
  y
}



#' Format numeric
#'
#' @param x \code{numeric} to
#' @param format The format to use
#' @param ... additional arguments passed on
#'
#' @return The formated vector
#' @export
Format.integer <- function(x, ..., format = guessFormat(x, dec = min(3, decimalPlaces(x)))) {
  Format(as.numeric(x), ..., format = format)
}

#' Get the depth of a list
#'
#' @param list The list to determine the with
#' @param current_depth The starting value of depth
#'
#' @return \code{numeric} indicating the deepest neting level of the list
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


