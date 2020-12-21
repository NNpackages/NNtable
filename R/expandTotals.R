


#' expand with Totals for specific columns in a tibble
#'
#' Add the totals for specified groups of variables (i.e. "var")
#' Note, the cells will be populated with the text in "name"
#'
#' @param .data data that needs to have totals added 
#' @param var column names for which a total needs to be added 
#' @param name Name the value that is assigned as the total
#'
#' @return a tibble with the new rows added
#'
#' @examples
#' # library(dplyr)
#' # summary_data <- adsl %>% filter(SAFFL == "Y") %>% expandTotals(c("TRT01A", "COUNTRY2")) %>% 
#' #     group_by_at(c("TRT01A", "COUNTRY2")) %>% summarize(N = n_distinct(USUBJID))
#' @importFrom dplyr bind_rows mutate
#' @importFrom rlang sym  
expandTotals <- function(.data, var, name = rep("Total", length(var))) {
  
  # Loop over the variables and add totals as specified in the name-vector
  for(i in seq_along(var)) {
    new_var <- rlang::sym(var[i])
    .data <- .data %>%
      dplyr::mutate(!!new_var := name[i]) %>%
      dplyr::bind_rows(.data)
  }
  return(.data)
}
