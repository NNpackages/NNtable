











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

  col_names <- gsub("NNTable_grouped_name", rep_name , colnames(data_str))

  if (!is.null(.NNTable$header$underscore) && .NNTable$header$underscore) {
    col_names <- gsub("__#__", "__#__NNTable_undescore__#__" , col_names)
  }

  list <- strsplit(col_names, "__#__")
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

    header.mat_pre <- header.mat

    alignment <- .NNTable$alignment$alignment

    # make sure that all top level rows are aligned center
    for (i in seq_len(nrow(header.mat) - 1)) {
      split_list <- strsplit(header.mat[i, ], .NNTable$cell_split$split)
      header.mat[i, ] <- sapply(lapply(split_list, align, "c"), paste, collapse = gsub("\\\\", "", .NNTable$cell_split$split))
    }

    # For the last row we align as dictated unless it is a grouped header, then we centre
    split_list <- strsplit(header.mat[nrow(header.mat), ], .NNTable$cell_split$split)

    apply(header.mat[seq_len(nrow(header.mat) - 1), ] != "", 2, any)

    alignment[apply(header.mat[seq_len(nrow(header.mat) - 1), ] != "", 2, any)] <- "c"

    split_align <- mapply(align, x = split_list, alignment)

    header.mat[nrow(header.mat), ] <-
      sapply(split_align, paste, collapse = gsub("\\\\", "", .NNTable$cell_split$split))


    # make sure that within each group level each the same number of cell-splits occur

    created_blanks <- rep(0, ncol(header.mat))

    if (nrow(header.mat) > 1) {
      for (i in seq_len(nrow(header.mat))[-1]) {

        non_blank <- apply(header.mat_pre[seq_len(i - 1),, drop = FALSE] != "", 2, any)
        at_non_blank <- header.mat[i, non_blank]
        n_lines <- sapply(gregexpr(.NNTable$cell_split$split, at_non_blank), function(x) sum(x > 0))
        n_missing_lines <- max(n_lines) - n_lines

        if (.NNTable$cell_split$align == "centre") {
          n_left_splits  <- ceiling(n_missing_lines / 2)
          n_right_splits <- n_missing_lines - n_left_splits
        } else if (.NNTable$cell_split$align == "bottom") {
          n_left_splits  <- n_missing_lines
          n_right_splits <- rep(0, length(n_missing_lines))
        } else {
          n_left_splits  <- rep(0, length(n_missing_lines))
          n_right_splits <- n_missing_lines
        }

        created_blanks[non_blank] <- n_missing_lines

        left_splits <- sapply(c(Matrix::t(n_left_splits)),
                              function(n) paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n),
                                                collapse = ""))

        right_splits <- sapply(c(Matrix::t(n_right_splits)),
                               function(n) paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n),
                                                 collapse = ""))

        header.mat[i, non_blank] <- paste0(left_splits, at_non_blank, right_splits)

      }


      non_blank <- apply(header.mat_pre[-nrow(header.mat_pre),, drop = FALSE] != "", 2, any)

      header.mat[-nrow(header.mat_pre), non_blank] <-
        matrix(paste0(c(Matrix::t(header.mat[-nrow(header.mat_pre), non_blank])),
                      gsub("\\\\", "", .NNTable$cell_split$split)),
               nrow = (nrow(header.mat) - 1), byrow = TRUE)

      head_collapsed <- apply(header.mat, 2, paste0, collapse = "")

    } else {
      head_collapsed <- header.mat
    }



    n_new_lines <-
      sapply(gregexpr(.NNTable$cell_split$split, head_collapsed), function(x) sum(x > 0))

    n_missing_lines <- max(n_new_lines) - n_new_lines

    if (.NNTable$cell_split$align == "centre") {
      n_left_splits  <- ceiling(n_missing_lines / 2)
      n_right_splits <- n_missing_lines - n_left_splits
    } else if (.NNTable$cell_split$align == "bottom") {
      n_left_splits  <- n_missing_lines
      n_right_splits <- rep(0, length(n_missing_lines))
    } else {
      n_left_splits  <- rep(0, length(n_missing_lines))
      n_right_splits <- n_missing_lines
    }


    left_splits <-
      sapply(n_left_splits, function(n)
        paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n), collapse = ""))

    right_splits <-
      sapply(n_right_splits, function(n)
        paste(rep(gsub("\\\\", "", .NNTable$cell_split$split), n), collapse = ""))


    head_collapsed <- paste0(left_splits, head_collapsed, right_splits)


    new_list <- strsplit(head_collapsed, .NNTable$cell_split$split)

    header.mat <- sapply(new_list, function(x) c(x, rep("", max(n_new_lines) + 1 - length(x))))

    if (!is.matrix(header.mat)) {
      header.mat <- as.matrix(t(header.mat))
    }
  }
  ###

  if (!is.null(.NNTable$header)) {
    .NNTable$header$matrix <- header.mat
  } else {
    .NNTable$header <- list(matrix = header.mat,
                            underscore = FALSE)

  }

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

  header.mat_underscore <- .NNTable$header$matrix

  # Make sure that at least one space is added between space cols
  data_str[, grep("^space.column", colnames(data_str))] <- ""


  n.headers <- nrow(header.mat_underscore)

  # Take the NNTable_underscore tag into account
  header.mat_underscore_rep <- header.mat_underscore

  wh_underscore_1 <- header.mat_underscore == "NNTable_undescore"
  wh_underscore_2 <- rbind(wh_underscore_1[-1,], wh_underscore_1[1,])
  header.mat_underscore_rep[wh_underscore_1] <- header.mat_underscore[wh_underscore_2]
  header.mat <- header.mat_underscore_rep
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



    # Above the adjust in accordance with needed widths
    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]
      new_width <- sum(width[seq])

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
                        pre_paste, post_paste, underscore, under_mat, under_mat_c,
                        nested_header) {

    #browser()
    v <- rle(x)
    from_to <- c(0, cumsum(v$length))

    use_width <- width
    #browser()
    # Above the adjust in accordance with needed widths
    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]

      new_width <- sum(width[seq])

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

    header.text <- character(0)

    for (i in seq_along(v$lengths)) {
      seq <- (from_to[i] + 1):from_to[i + 1]
      new_width <- sum(use_width[seq]) # + length(seq) - 1 # let the extra space count

      if (v$values[i] == "NNTable_undescore") {
        header.text[i] <- hline(times = max(apply(under_mat[, seq, drop = FALSE], 1, sum)))
      } else if ((v$length[i] == 1  & nrows == 1 & i == 1) | (!any(nested_header[seq]) & v$length[i] == 1)) {
        header.text[i] <- align(x = v$values[i], alignment[from_to[i + 1]], new_width, keep.empty = FALSE)
      } else {
        header.text[i] <- align(x = v$values[i], "c", new_width, keep.empty = FALSE)
      }
    }
    pre_paste <- ""

    paste(c(pre_paste, header.text, post_paste), collapse = "")

  }



  under_mat <- width_mat <-
    matrix(rep(width, each = n.headers), nrow = n.headers, byrow = FALSE)

  if (nrow(under_mat) > 1) {
    # extract the length of the underscores
    for (i in seq_len(nrow(under_mat) - 1)) {
      under_mat[i, ] <- underscoreWidth(x = header.mat[i, ], width =  width_mat[i, ])
    }
    #under_mat <- under_mat[seq_len(nrow(under_mat) - 1), ]
  }

  # in order to keep the changes made to header.mat a new matrix is made that
  # incorporates those changes but also keeps "NNTable_underscore" tag
  header.mat_underscore2 <- header.mat
  header.mat_underscore2[header.mat_underscore == "NNTable_undescore"]  <- "NNTable_undescore"

  header.mat.align <- character()
  #browser()
  for (i in seq_len(n.headers)) {
    header.mat.align <-
      c(
        header.mat.align,
        headAlign(x = header.mat_underscore2[i, ],
                  width      = width_mat[i, ],
                  alignment  = alignment,
                  nrows      = n.headers,
                  any_pre_p  = any(nchar(pre_header_spaces)),
                  pre_paste  = pre_header_spaces[i],
                  post_paste = post_header_spaces[i],
                  under_mat  = under_mat[setdiff(seq_len(nrow(under_mat)), seq_len(i - 1)), , drop = FALSE],
                  under_mat_c = under_mat[seq_len(i), , drop = FALSE],
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





