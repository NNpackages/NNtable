# test created for bug 285824

# When adding nested columns for tables with columns that already
# includes line shift it breaks on top

context("Test NNTable bug 285824")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)




(db <- nnaccess("0001", root = "~/training"))


adsl_01 <- db$adam("adsl") %>% filter(SAFFL == "Y" & TRT30A != "") %>%
  rename(TRTA = TRT30A)



set.seed(100)

adsl_f <- adsl_01 %>%
  select(USUBJID, SUBJID, AGE, SEX, BMIBL, RANDFL, TRTSDT, TRTEDT, EOSDT, DCSREAS, DCSREASP, TRTA) %>%
  mutate(EXPFL   = if_else(is.na(TRTSDT) == FALSE, "Y", "N"),
         SUBJID  = as.numeric(SUBJID),
         ICOD    = sample(c("Y", "N"), n(), replace = TRUE),
         ICODD   = sample(c("Y", "N"), n(), replace = TRUE),
         ICOTD   = sample(c("Y", "N"), n(), replace = TRUE),
         IGLARD  = sample(c("Y", "N"), n(), replace = TRUE),
         IGLARDD = sample(c("Y", "N"), n(), replace = TRUE),
         IGLARTD = sample(c("Y", "N"), n(), replace = TRUE))

# Setup columns
cols <- list("Subject ID/|Age/Sex/BMI" = "SUBJID/|AGE/SEX/BMIBL",
             "Randomised|(Y/N)"          = "RANDFL",
             "Exposed|(Y/N)"             = "EXPFL",
             "Exposed in treatment|period (Y/N)/|Double dose (Y/N)/|Triple dose (Y/N)" = c(
               "Ico  "     = "ICOD/ICODD/ICOTD",
               "IGlar"   = "IGLARD/IGLARDD/IGLARTD"
             ),
             "First drug date/|last drug date"    = "TRTSDT/|TRTEDT",
             "Last|participation|date"            = "EOSDT",
             "Primary reason|participation|ended" = "DCSREAS",
             "Comment"                            = "DCSREASP"
)



# Create NNTable ---------------------------------------------------------------


test_that("Bug 285824 test 1", {

  .NNTable <- adsl_f %>%
    mutate(order = as.factor(SUBJID)) %>%
    NNTable(cols, page_size = getEOTpaper("land", 9)) %>%
    addGroupedColumns(names(cols)[1], name = names(cols)[1]) %>%
    addOrder(order = 1) %>%
    addCellSplit(align = "bottom") %>%
    addAlignment("Exposed in treatment" = "l", "period (Y/N)/" = "l") %>%
    addUnderScore() %>%
    addTruncation(Comment = 30) %>%
    addFormat(dec = 2)


  file <- "table_285824_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})




test_that("Bug 285824 test 2", {

  names(cols)[4] <- paste(align(c("Exposed in treatment",
                                  "period (Y/N)/",
                                  "Double dose (Y/N)/",
                                  "Triple dose (Y/N)"), "l"), collapse = "|")



  .NNTable <- adsl_f %>%
    mutate(order = as.factor(SUBJID)) %>%
    NNTable(cols, page_size = getEOTpaper("land", 9)) %>%
    addGroupedColumns(names(cols)[1], name = names(cols)[1]) %>%
    addOrder(order = 1) %>%
    addCellSplit(align = "bottom") %>%
    addAlignment("Exposed in treatment" = "l", "period (Y/N)/" = "l") %>%
    addUnderScore() %>%
    addTruncation(Comment = 30) %>%
    addFormat(dec = 2)


  file <- "table_285824_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})




test_that("Bug 285824 test 3", {

  cols <- list("Subject ID/|Age/Sex/BMI" = "SUBJID/|AGE/SEX/BMIBL",
               "Randomised|(Y/N)"          = "RANDFL",
               "Exposed|(Y/N)"             = "EXPFL",
               "Exposed in treatment|period (Y/N)/|Double dose (Y/N)/|Triple dose (Y/N)" = c(
                 "Ico|andnf|jfj"     = "ICOD/ICODD/ICOTD",
                 "IGlar"   = "IGLARD/IGLARDD/IGLARTD"
               ),
               "First drug date/|last drug date"    = "TRTSDT/|TRTEDT",
               "Last|participation|date"            = "EOSDT",
               "Primary reason|participation|ended" = "DCSREAS",
               "Comment"                            = "DCSREASP"
  )


  .NNTable <- adsl_f %>%
    mutate(order = as.factor(SUBJID)) %>%
    NNTable(cols, page_size = getEOTpaper("land", 9)) %>%
    addGroupedColumns(names(cols)[1], name = names(cols)[1]) %>%
    addOrder(order = 1) %>%
    addCellSplit(align = "bottom") %>%
    addAlignment("Exposed in treatment" = "l", "period (Y/N)/" = "l") %>%
    addUnderScore() %>%
    addTruncation(Comment = 30) %>%
    addFormat(dec = 2)


  file <- "table_285824_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})
