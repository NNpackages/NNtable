
# test created for bug 388785

# When NA is used in subcats they are not read as empty lines

context("Test NNTable bug 388785")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)
db_0002 <- nnaccess("0002", root = "~/training")

dm_f   <- db$sdtm("dm")
adsl_f <- db$adam("adsl")


## *****************************************************************************
## # Define functions                                                       ----
##
## Needed functions are defined
## *****************************************************************************


# Function to summarise population groups
popsum <- function(ds, infilter, label, ord, by = NULL){

  infilter <- enquo(infilter)

  if(is.null(by)){
    outds <- ds %>% filter((!! infilter)) %>%
      summarise(N = n_distinct(USUBJID)) %>%
      mutate(cat = label, order = ord)

  } else {

    # Not all functions take a quoted input so an unquoted variant is created
    by_s <- rlang::sym(by)

    outds <- ds %>% filter((!! infilter)) %>%
      group_by(!!by_s) %>%
      summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
      mutate(cat = label,
             subcat = stringr::str_to_sentence(!!by_s),
             order = ord,
             suborder = if_else(str_to_upper(!!by_s) == "OTHER",99,0)) %>%
      select(-!!by_s)
  }

  return(outds)
}


# Function for masking numbers -------------------------------------------------


## *****************************************************************************
## # Derive counts                                                          ----
##
##
## *****************************************************************************


subj_disp <-
  bind_rows(
    popsum(dm_f, ACTARMCD != "","Screened", 1),
    popsum(dm_f, ACTARMCD == "SCRNFAIL", "Screening failures", 2),
    popsum(adsl_f, ACTARMCD == "NOTASSGN", "Withdrawn or discontinued before randomisation", 3),
    popsum(adsl_f, ACTARMCD == "NOTASSGN", "Withdrawn or discontinued before randomisation", 3, by = "DCSREAS"),
    popsum(adsl_f, ACTARMCD != "NOTASSGN" & ACTARMCD != "SCRNFAIL" & ACTARMCD != "", "Randomised", 4),
    popsum(adsl_f, ACTARMCD != "NOTASSGN" & TRTDURD == 0 & EOSSTT == "DISCONTINUED", "Non-exposed subjects withdrawn or discontinued after randomisation",5),
    popsum(adsl_f, ACTARMCD != "NOTASSGN" & TRTDURD == 0, "Non-exposed subjects withdrawn or discontinued after randomisation", 5, by = "DCSREAS"),
    popsum(adsl_f, TRTDURD > 0, "Exposed", 7),
    popsum(adsl_f, TRTDURD > 0 & EOSSTT == "DISCONTINUED", "Exposed subjects withdrawn or discontinued", 8),
    popsum(adsl_f, TRTDURD > 0 & EOSSTT == "DISCONTINUED", "Exposed subjects withdrawn or discontinued", 9, by = "DCSREAS"),
    popsum(adsl_f, EOSSTT == "COMPLETED", "Completed", 10),
    popsum(adsl_f, SAFFL == "Y", "Safety analysis set", 11),
    popsum(adsl_f, FASFL == "Y", "Full analysis set", 12)
  )

subj_disp_p <- subj_disp %>%
  filter(cat == "Randomised") %>%
  select(N) %>%
  rename(NP = N) %>%
  cbind(subj_disp) %>%
  mutate(P = ifelse(order > 3, (N/NP)*100, NA), TRT = "Total")


# Generate NNTable ----------------------------------------------------------


test_that("Bug 388785", {

  .NNTable <- subj_disp_p %>%
    NNTable("cat","subcat","N","(%)" = "(P)","TRT") %>%
    addTransWide("TRT" = c("N","(%)")) %>%
    addUnderScore() %>%
    addGroupedColumns("cat","subcat", name = "") %>%
    addFormat(dec = 1) %>%
    addOrder(order = 1, suborder = 1)


  file <- "table_1_388785"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})
