# test created for bug 343919

# addFormat and addTransLong interferes with each other

# The error occured when only one variable was added to format data (uni)
# In this case the variable SD was the only not to be formatted jointly

context("Test NNTable bug 343919")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

(db1 <- nnaccess("0001", root = "~/training"))

adlb_f <- db1$adam("adlb") %>% filter(FASFL == "Y" & TRTP != "" & ANL01FL == "Y" & TOPIC_CD == "HBA1C_BLOOD")

stats <- adlb_f %>%
  group_by(TRTP,PARAMCD,AVISIT) %>%
  summarise(N = n_distinct(USUBJID),
            MEAN = mean(AVAL),
            SD = stats::sd(AVAL),
            .groups = "drop")


# Create NNTable ---------------------------------------------------------------


test_that("Bug 343919 test 1", {

  .NNTable <- NNTable(stats, "TRTP","PARAMCD","AVISIT","N","Mean (SD)" = "MEAN (SD)") %>%
    addTransLong(SUM = c("N","Mean (SD)")) %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2)


  file <- "table_343919_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 343919 test 1 reverse order", {

  .NNTable <- NNTable(stats, "TRTP","PARAMCD","AVISIT","N","Mean (SD)" = "MEAN (SD)") %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2) %>%
    addTransLong(SUM = c("N","Mean (SD)"))

  file <- "table_343919_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

