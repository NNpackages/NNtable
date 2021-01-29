context("Test NNTable fr 332974")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

(db <- nnaccess("0001", root = "~/training"))

adae_f <- db$adam("adae") %>% filter(SAFFL == "Y" & TRTA != "")

adsl_f <- db$adam("adsl") %>% filter(SAFFL == "Y" & TRT30A != "") %>%
  rename(TRTA = TRT30A)


cols <- c("Subject ID/|Age (years)/|Sex (M/F)/|BMI (kg/m^2)" = "SUBJID/|AGE/SEX/|BMIBL",
           "Treatment/|day" = "TRTA/APERADY",
           "AE|no" = "AEREFID",
           "Preferred term/|investigator's|description" = "AEDECOD/|AETERM",
           "Severity" ="AESEV",
           "SAE|(Y/N)" = "AESER",
           "Outcome" = "AEOUT",
           "Onset/outcome|date and time|(date:h:m)" = "ASTDTM/|AENDTM",
           "Duration|of AE" = "ADURN",
           "Relation|to trial|product#" = "AREL1"
)


adae_clean <- adae_f %>% mutate(AETERM = str_to_sentence(AETERM),
                                AEREFID = as.numeric(AEREFID),
                                AESEV  = str_to_sentence(AESEV),
                                AREL1  = str_to_sentence(AREL1),
                                AEOUT  = str_to_sentence(gsub("/","/|", AEOUT)))




table <- NNTable(adae_clean, cols, page_size = getEOTpaper("land")) %>%
  addCellSplit(align = "bottom") %>%
  addTruncation("Preferred term/|investigator's|description" = 29, exdent = 1)



test_that("Bug 333968", {

  .NNTable <- table %>%
    addGroupedColumns(names(cols)[1], "AE|no", name = names(cols)[1])

  file <- "table_333968"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 332974 test 1", {

  .NNTable <- table %>%
    addGroupedColumns(names(cols)[1], name = names(cols)[1])

  file <- "table_fr_332974"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 332974 test 2", {

  .NNTable <- table %>%
    addGroupedColumns(names(cols)[1], name = names(cols)[1], remove_duplicated_stub_row = FALSE)

  file <- "table_fr_332974_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


