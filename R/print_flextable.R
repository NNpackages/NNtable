

#' @importFrom flextable flextable
#' @importFrom officer read_docx
print_flextable <- function(.NNTable) {

  col_chars <- get_column_chars(.NNTable, font = "apis")


  .NNTable$data_str[, colnames(col_chars$data_str)] <- col_chars$data_str

  if ("NNTable_pre_space" %in% colnames(col_chars$data_str)) {
    .NNTable$data_str <- cbind(col_chars$data_str[, "NNTable_pre_space", drop = FALSE], .NNTable$data_str )
  }


  #---------------------------------------------------------------------------#
  ######                   Calculate table height                        ######
  #---------------------------------------------------------------------------#

  .NNTable <- apply_n_bodylines(.NNTable)

  .NNTable <- apply_splitPages(.NNTable)

  #---------------------------------------------------------------------------#
  ######                   Calculate table height                        ######
  #---------------------------------------------------------------------------#

  .NNTable$flex <- lapply(.NNTable$data_split, apply_to_flextable,
                          .NNTable = .NNTable, col_chars = col_chars)


  .NNTable
}



apply_to_flextable <- function(data_split, .NNTable, col_chars) {

  cell_height <- .NNTable$page_size$cell_height

  data_output <- data_split[, setdiff(colnames(data_split), c(.NNTable$remove$columns,
                                                          .NNTable$remove$columns_trunc,
                                                          .NNTable$remove$split_page)), drop = FALSE]

  # blanks are replaced with NA because of a bug in flextable where blanks are ignored in font settings
  data_output[data_output == ""] <- NA


  ft <- flextable::flextable(data_output)
  ft <- flextable::height_all(ft, height =  cell_height / 2.54, part = "all")
  ft <- flextable::hrule(ft, rule = "exact", part = "all")


  #---------------------------------------------------------------------------#
  ######                      Define the header                          ######
  #---------------------------------------------------------------------------#

  # start with no header
  ft <- flextable::delete_part(ft, part = "header")

  header <- col_chars$header
  header_mat <- col_chars$header.mat_underscore
  header_mat[header_mat == ""] <- " "
  ft <- flextable::add_header(x = ft,  values = as.list(structure(header_mat[nrow(header_mat), ], names = header)), top = FALSE)
  count <- 1

  if (.NNTable$header$underscore & nrow(header_mat) > 1) {
    for (i in rev(seq_len(nrow(header_mat))[c(TRUE, FALSE)])[-1]) {
      count <- count + 1

      ft <- flextable::add_header(ft, values = as.list(structure(header_mat[i, ], names = header)),  top = TRUE )
      to <- rle(header_mat[i + 1, ])
      c_sum_1 <- cumsum(to$lengths)
      c_sum <- c(0, cumsum(to$lengths), ncol(header_mat))
      for (j in which(to$values == "NNTable_undescore")) {
        cols <- seq(min(c_sum[j] + 1, ncol(header_mat)), c_sum[j + 1])
        ft <- flextable::border(ft, i = 1, j = header[cols] , part = "header",
                                border.bottom = officer::fp_border(color = "black", style = "solid", width = 1))
        ft <- flextable::align(ft, i = 1, j = header[cols] , part = "header", align = "center")
      }

    }

  } else {

    for (i in rev(seq_len(nrow(header_mat) - 1))) {
      count <- count + 1
      ft <- flextable::add_header(ft, values = as.list(structure(header_mat[i, ], names = header)),  top = TRUE )
    }
  }


  .NNTable$page_size
  ft <- flextable::fontsize(ft, size = .NNTable$font_size, part = "all")
  ft <- flextable::font(ft, fontname = "Apis For Office", part = "all")

  # merge horizontally when there are identical values
  ft <- flextable::merge_h(ft, part = "header")
  ft <- flextable::hline_top(ft,    part = "header", border = officer::fp_border(color = "black", width = 2))
  ft <- flextable::hline_bottom(ft, part = "header", border = officer::fp_border(color = "black", width = 2))

  ft <- flextable::height_all(ft, height =  cell_height / 2.54, part = "all")
  ft <- flextable::hrule(ft, rule = "exact", part = "all")
  #---------------------------------------------------------------------------#
  ######                      Define the alignment                       ######
  #---------------------------------------------------------------------------#


  align <- col_chars$alignment
  classes_str <- col_chars$classes_str
  align[classes_str[names(align)] %in% c("integer", "numeric")] <- "l"

  align[] <- structure(c("left", "center", "right", "justify"), names = c("l", "c", "r", "j"))[align]


  for (alignment in unique(align))
    ft <- flextable::align(ft, j = names(align[align == alignment]),  align = alignment, part = "body")


  ft <- flextable::padding(ft, j = header , padding = 0, part = "all")#padding.left = 0, padding.right = 0)


  #---------------------------------------------------------------------------#
  ######          Calculate the width of seppers and spacers             ######
  #---------------------------------------------------------------------------#


  res.chars <- .NNTable$page_size$page.width - sum(col_chars$column.chars, na.rm = TRUE) #- (ncol - 1)

  # find the columns_to_wide spacers
  spacers <- grep("^space.column.", colnames(data_output))
  seppers <- grep("^sep.column.", colnames(data_output))

  min_width <- stringWidth(" ") / 2.54

  if (res.chars - (length(spacers) + length(seppers)) * min_width < 0) {
    warning("The actual columns are too wide to fit the output")
    .NNTable$page_size$used.page.width <- (.NNTable$page_size$used.page.width -
                                             res.chars) + (length(spacers) + length(seppers))

    res.chars <- min_width * (length(spacers) + length(seppers))
  }

  if (is.null(.NNTable$spacing )) {
    .NNTable$spacing <- list(max.sep   = 50,
                             max.space = 50,
                             spread    = TRUE,
                             space     = 1,
                             sep       = 1)
  }
  if (.NNTable$spacing$spread) {
    # initialise width

    width <- column.chars <- col_chars$column.chars

    sep_width <- max(min(res.chars / (length(seppers) + length(spacers)), .NNTable$spacing$max.sep), min_width)

    if (length(spacers)) {
      if (sep_width <= 3 * 2 * min_width) {
        sep_width <- min_width
        space_width   <- column.chars[spacers] + min(max((res.chars - length(seppers)) / length(spacers), 0), .NNTable$spacing$max.space)
      } else {
        sep_width <- min(res.chars / (length(seppers) + 3 * length(spacers)), .NNTable$spacing$max.sep)
        space_width   <- column.chars[spacers] + min(max((res.chars - sep_width * length(seppers)) / length(spacers), 0), .NNTable$spacing$max.space)
      }
      width[spacers] <-  space_width
    }


    width[seppers] <- width[seppers] + sep_width

  } else {
    if (res.chars - (length(spacers)*.NNTable$spacing$space + length(seppers) * .NNTable$spacing$sep) < 0) {
      warning("The actual columns are too wide to fit the output with the supplied widths")
      .NNTable$page_size$used.page.width <- (.NNTable$page_size$used.page.width -
                                               res.chars) + (length(spacers) + length(seppers) * min_width)
      res.chars <- (length(spacers) + length(seppers)) * min_width
    }

    width <- column.chars
    width[seppers] <- .NNTable$spacing$sep
    width[spacers] <- .NNTable$spacing$space
  }

  # add names to the width
  names(width) <- header

  width_inch <- width / 2.54
  for (col in header) {
    ft <- flextable::width(ft, col, width_inch[col])
  }

  #---------------------------------------------------------------------------#
  ######        merge cells that represent the same original cell        ######
  #---------------------------------------------------------------------------#

  ft <- flextable::set_formatter_type(ft, na_str = " ")

  # First the coluns are merged
  if ("NNTable_added_group" %in% colnames(data_split))
    for (row in which(data_split$NNTable_added_group))
      ft <- flextable::merge_at(ft, i = row, j = seq_along(header), part = "body")

  # Next we find the locations where the rows have been split
  if ("NNTable_trunc_id" %in% colnames(data_split)) {
    split_ids <- data_split$NNTable_trunc_id[data_split$NNTable_trunc_id[-1] == data_split$NNTable_trunc_id[-nrow(data_split)] &
                                             data_split$NNTable_trunc_id[-1] != "" & data_split$NNTable_trunc_id[-nrow(data_split)] != ""]

    for (i in split_ids) {
      for (j in seq_along(header)) {
        rows <- which(data_split$NNTable_trunc_id == i)

        ft <- flextable::merge_at(ft, i = rows, j = j, part = "body")

        values <- data_output[rows, j]
        values <- values[!is.na(values)]
        if (length(values))
          ft <- flextable::compose(ft, i = rows[1], j = j,
                                   flextable::as_paragraph(list_values = paste0(
                                     values,
                                     c(rep("\n", length(values) - 1), ""))))
      }
    }
  }

  ft
}






count_leading_space <- function(x) {

  count <- stringr::str_length(stringr::str_extract(x,"^(\\s+)[^ ]")) - 1

  count[is.na(count)] <- 0

  return(count)
}

double_leading_space <- function(x) {
  nspaces <- count_leading_space(x)
  spaces <- sapply(nspaces, function(n) paste(rep(" ", n), collapse = ""))
  paste(spaces, x, sep = "")
}



#' @importFrom graphics par strheight strwidth
stringWidth <- function(x, font_size = 8, penalty =  1.07) {
  font <- system.file("apis/ApisForOffice-Regular.ttf", package = "NNtable")
  strwidth(x, units = "inches", family = font, cex = font_size / par("ps")) * 2.54 * penalty
}


cum_string_wrap <- function(x, width) {
  lines <- character(0)
  while (length(x)) {
    wh <- which(x <= width )
    lines <- c(lines, paste(names(x[wh]), collapse = " "))
    x <- x - x[wh[length(wh)]]
    x <- x[-wh]
  }
  lines
}

stringWrap <- function(text, width = 5, font_size = 8) {

  text_list <- lapply(strsplit(text, " "), function(x)
    structure(stringWidth(x, font_size = font_size) + c(rep(stringWidth(" ", font_size = font_size), max(length(x) - 1, 0)), 0), names = x))

  text_list_sum <- lapply(text_list, cumsum)

  sapply(text_list_sum, cum_string_wrap, width = width)
}


stringHeight <- function(x, font_size = 8, font_type = 1) {
  font <- system.file("apis/ApisForOffice-Regular.ttf", package = "NNtable")
  strheight(x, units = "inches", family = font, cex = font_size / par("ps"), font = font_type) * 2.54
}

height_adj <- readRDS(system.file("apis/height_adj.rds", package = "NNtable"))

#' @importFrom  stats predict
stringHeightAdj <- function(x, font_size = 8, font_type = 1) {
  height <- stringHeight(x, font_size = font_size, font_type = font_type)
  predict(height_adj, newdata = data.frame(str = height, size = font_size))
}


get_classes <- function(data, data_str, .NNTable) {

  classes <- sapply(data, class)
  classes_str <- sapply(data_str, class)

  header <- colnames(data_str)

  org_names <- sapply(strsplit(header, "__#__"), function(x) x[1])
  names(org_names) <- header

  trans_list <- .NNTable$concat$table[unique(org_names[org_names %in% names(.NNTable$concat$table)])]

  for (var in names(trans_list)) {
    trans <- trans_list[[var]]

    if (length(trans) == 1) {
      classes_str[org_names == var] <- classes[trans]
      org_names[org_names %in% c(var, paste0(var, "_trunc"))]  <- trans
    } else {
      if (all(classes[trans]) %in% c(c("integer", "numeric")))
        classes_str[org_names == var] <- "all_numeric"
      org_names[org_names %in% c(var, paste0(var, "_trunc"))]  <- paste(trans, collapse = "_")
    }
  }

  classes_str
}




