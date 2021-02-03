
# test created for bug 336364

# addFormat fails when used together with addExposure and
# first column is not mentioned in addFormat

context("Test NNTable bug 336364")
options(lifecycle_verbosity = "warning")

nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

(db <- nnaccess("0001", root = "~/training"))

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
  addTransWide("TRTA" = c("N","E")) %>%
  addGroupedColumns("AESOC") %>%
  addExposure(exposure = subjects,
              format_alone = TRUE,
              format_data = c("Number of subjects" = "%.0f", "Total exposure" = "%.2f")) %>%
  addOrder(E = -1)




test_that("Bug 336364 baseline", {

  .NNTable <- table

  file <- "table_0_336364"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 336364 baseline + format", {

  .NNTable <- table %>% addFormat(format_data = c(E = "%.1f"))

  file <- "table_1_336364"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 336364 baseline + format explicit 1", {

  .NNTable <- table %>% addFormat(format_data = data.frame("AESOC" = "Musculoskeletal and connective tissue disorders", E = "%.1f"))

  file <- "table_2_1_336364"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Bug 336364 baseline + format explicit 2", {


  .NNTable <- NNTable(stats, "TRTA","AESOC","N","E") %>%
    addTransWide("TRTA" = c("N","E")) %>%
    addGroupedColumns("AESOC")  %>%
    addFormat(format_data = data.frame("AESOC" = c("Musculoskeletal and connective tissue disorders", NA), E = c("%.2f", NA)))

  file <- "table_2_2_336364"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})



adsl_f <- adsl       %>%
  filter(FASFL == "Y") %>%
  rename(TRTP = TRT01P)

# calculate the totals by treatment arm
totals <- adsl_f         %>%
  mutate(TRTP = "Total") %>%
  bind_rows(adsl_f)      %>%
  filter(FASFL == "Y")   %>%
  group_by(TRTP)         %>%
  summarize("Number of subjects"     = n_distinct(USUBJID),
            "Total exposure (years)" = sum(TR01DURY))

# Calculate the summary statistcs
output <- adlb                                                  %>%
  mutate(TRTP = "Total")                                        %>%
  bind_rows(adlb)                                               %>%
  filter(SAFFL == "Y" & PARAMCD %in% c("ABSR148N", "ABTIT148")) %>%
  group_by(TRTP, PARAM, STUDYID)                                %>%
  summarize(N  = n_distinct(USUBJID),
            keep = mean(AVAL, na.rm = TRUE),
            Mean = mean(AVAL, na.rm = TRUE),
            SD   = sd(  AVAL, na.rm = TRUE),
            Min  = min( AVAL, na.rm = TRUE),
            Max  = max( AVAL, na.rm = TRUE)
  )


table <- NNTable(output, "TRTP", "PARAM", "keep", "N", "Mean (SD)", "Min ; Max")

test_that("Bug 336364 summary table format + exposure", {


  .NNTable <-  table                                      %>%
    addTransLong(SUM = c("N", "Mean (SD)" , "Min ; Max"),
                 var_name = "Called var")                 %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2)       %>%
    addExposure(exposure = totals,
                format_alone = TRUE,
                format_data  = c("Number of subjects" = "%.0f"))


  file <- "table_sum_336364"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})
