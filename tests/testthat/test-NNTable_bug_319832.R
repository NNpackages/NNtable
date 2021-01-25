
# test created for bug 319832

# If the two inputs `format_alone = ` and `format_data =` are used, when applying
# the addExposure function then it seems to interfere with the way
# addFormat formats numbers.

context("Test NNTable bug 319832")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

NNtraining::createTrainingDB()
db <- nnaccess("0001", root = "~/training")

adae_f <- db$adam("adae") %>% filter(SAFFL == "Y" & TRTA != "")
adsl_f <- db$adam("adsl") %>% filter(SAFFL == "Y" & TRT30A != "") %>%
  rename(TRTA = TRT30A)


subjects <- adsl_f %>%
  group_by(TRTA) %>%
  summarise("Number of subjects" = n_distinct(USUBJID),
            "Total exposure" = sum(TRDURY),
            .groups = "drop")


stats <- adae_f %>%
  group_by(TRTA,AESOC) %>%
  summarise(N = n_distinct(USUBJID),
            E = n(), .groups = "drop")


table <- NNTable(stats, "TRTA","AESOC","N","E") %>%
  addTransWide("TRTA" = c("N","E"))



test_that("Bug 319832 baseline", {

  .NNTable <- table

  file <- "table_0_319832"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 319832 baseline + filling", {

  .NNTable <-  table %>% addFilling(N = 0)

  file <- "table_1_319832"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Bug 319832 baseline + order  + filling ", {

  .NNTable <- table %>% addFilling(N = 0) %>%
    addOrder(E = -1)

  file <- "table_2_319832"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Bug 319832 baseline + order + filling + grouping", {

  .NNTable <- table %>% addFilling(N = 0) %>%
    addOrder(E = -1) %>% addGroupedColumns("AESOC")

  file <- "table_3_319832"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

