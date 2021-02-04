

context("Test NNTable feature request 302334 bug 342059")

# In some tables and listings we often would like to trade out the
# strings/abbreviations provided by flags and other variables.
#
# It could be very nice if you could provide a named vector to NNTable that
# would translate strings/abbreviations at print, such that data manipulation
# is kept separate from presentation.


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

(db <- nnaccess("0001", root = "~/training"))

adae_f <- db$adam("adae") %>% filter(SAFFL == "Y" & TRTA != "")
adsl_f <- db$adam("adsl") %>% filter(SAFFL == "Y" & TRT30A != "") %>%
  rename(TRTA = TRT30A)


subjects <- adsl_f %>%
  group_by(TRTA, SEX) %>%
  summarise("Number of subjects" = n_distinct(USUBJID),
            "Total exposure" = sum(TRDURY),
            .groups = "drop")

subjects$SEX <- as.factor(subjects$SEX)


stats <- adae_f %>%
  group_by(TRTA,AESOC,SEX) %>%
  summarise(N = n_distinct(USUBJID),
            E = n(), .groups = "drop")

stats$SEX <- as.factor(stats$SEX)

table <- NNTable(stats, "TRTA", sex = "SEX", "AESOC","N","E") %>%
  addTransWide("TRTA" = c("N","E")) %>%
  addGroupedColumns("AESOC") %>%
  addOrder(E = -1)



test_that("Feature request 302334 test 1", {

  .NNTable <- table %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  file <- "table_fr_302334_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 302334 test 2 exposure", {

  .NNTable <- table %>%
    addExposure(subjects, format_alone = TRUE) %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  file <- "table_fr_302334_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 302334 test 2 wrong name", {

  .NNTable <- table %>%
    addExposure(subjects, format_alone = TRUE) %>%
    addTranslate(TRTA2  = c("Ico" = "A", "NOVO rapid" = "B"))

  file <- "table_fr_302334_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})



subjects <- adsl_f %>%
  group_by(TRTA, SEX) %>%
  summarise("Number of subjects" = n_distinct(USUBJID),
            "Total exposure" = sum(TRDURY),
            .groups = "drop")

subjects$SEX <- as.factor(subjects$SEX)

subjects$`sex 2` <- subjects$SEX
subjects$`TRTA  2` <- subjects$TRTA

stats <- adae_f %>%
  group_by(TRTA,AESOC,SEX) %>%
  summarise(N = n_distinct(USUBJID),
            E = n(), .groups = "drop")

stats$SEX <- as.factor(stats$SEX)

stats$`sex 2`    <- stats$SEX
stats$`TRTA  2`  <- stats$TRTA
stats$`AESOC  2` <- stats$AESOC

table <- NNTable(stats, "TRTA  2", "SEX", sex = "sex 2", "AESOC  2","N","E") %>%
  addTransWide("TRTA  2" = c("N","E")) %>%
  addGroupedColumns("AESOC  2") %>%
  addOrder(E = -1)



test_that("Feature request 342059 test space 1", {

  .NNTable <-  NNTable(stats, "TRTA  2", "SEX", sex = "sex 2", "AESOC  2","N","E") %>%
    addTransWide("TRTA  2" = c("N","E")) %>%
    addGroupedColumns("AESOC  2") %>%
    addOrder(E = -1) %>%
    addTranslate("TRTA  2"  = c("Ico" = "A", "NOVO rapid" = "B"),
                 "sex 2"    = c("Female" = "F", "Male" = "M"),
                 "AESOC  2" = stringr::str_to_title)

  file <- "table_fr_342059_s1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 302334 test space 2 exposure", {

  .NNTable <- table %>%
    addExposure(subjects, format_alone = TRUE) %>%
    addTranslate("TRTA  2"  = c("Ico" = "A", "NOVO rapid" = "B"),
                 "sex 2"    = c("Female" = "F", "Male" = "M"),
                 "AESOC  2" = stringr::str_to_title)

  file <- "table_fr_302334_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Feature request 302334 test space 2 wrong name", {

  .NNTable <- table %>%
    addExposure(subjects, format_alone = TRUE) %>%
    addTranslate(TRTA2  = c("Ico" = "A", "NOVO rapid" = "B"))

  file <- "table_fr_302334_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})



