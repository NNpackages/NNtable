
# test created for bug 340549

# addFormat fails when used together with addExposure and
# first column is not mentioned in addFormat

context("Test NNTable bug 340549")

## Treatment arm
trt <- c("sema",
         "sema + GIP 2 mg",
         "sema + GIP 3 mg",
         "GIP 2 mg",
         "GIP 3 mg")

# Dummy ADPP data
m <- lvm()
m <- categorical(m, ~TRTP, K = 5, p = c(0.2, 0.2, 0.2, 0.2, 0.2), labels = trt)
distribution(m, ~AVAL) <- normal.lvm(mean = 120, sd = 20)

adpc <- sim(m, 200)

## Add cohorts
adpc <- adpc %>%
  mutate(cohort = ifelse(TRTP %in% grep("sema", trt, value = TRUE), "cohort1", "cohort2")) %>%
  group_by(cohort, TRTP) %>%
  summarize(MEAN = mean(AVAL),
            MIN = min(AVAL),
            MAX = max(AVAL))


## NNtable
table <- NNTable(adpc,"cohort", "TRTP", "MEAN","MIN", "MAX") %>%
  addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
  addTransWide(cohort = list(TRTP = "SUM"))




test_that("Bug 340549 baseline", {

  .NNTable <- table

  file <- "table_0_340549"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 340549 baseline + format", {

  .NNTable <- table %>% addUnderScore()

  file <- "table_1_340549"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 340549 baseline + format explicit 1", {

  .NNTable <-  table %>% addUnderScore() %>% addAlignment(SUM = "c")

  file <- "table_2_340549"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

