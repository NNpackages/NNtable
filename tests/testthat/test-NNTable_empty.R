
context("Test NNTable empty output")


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
            E = n(), .groups = "drop") %>%
  filter(FALSE)




table <- NNTable(stats, "TRTA","AESOC","N","E") %>%
  addTransWide("TRTA" = c("N","E"))



test_that("empty output", {

  .NNTable <- table

  file <- "table_empty"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

