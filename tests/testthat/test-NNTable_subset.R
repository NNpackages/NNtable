

context("Test NNTable subset functionality")


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


stats <- adae_f %>%
  group_by(TRTA,AESOC,SEX) %>%
  summarise(AGE = mean(AGE),
            N = n_distinct(USUBJID),
            E = n(), .groups = "drop")

table <- NNTable(stats, "TRTA", sex = "SEX", "AGE", "AESOC","N","E") %>%
  addTransWide("TRTA" = c("N","E")) %>%
  addGroupedColumns("AESOC") %>%
  addOrder(E = -1)



test_that("NNTable subset include only females", {

  .NNTable <- table %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[SEX == "F", ]

  file <- "table_subset_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("NNTable subset remove column", {

  .NNTable <- table %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[, -"AGE"]

  file <- "table_subset_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("NNTable subset remove column and subset rows", {

  .NNTable <- table %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[SEX == "F", -"AGE"]

  file <- "table_subset_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})






test_that("NNTable subset include only females", {

  .NNTable <- table %>%
    addExposure(subjects) %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[SEX == "F", ]

  file <- "table_subset_exp_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("NNTable subset remove column", {

  .NNTable <- table %>%
    addExposure(subjects) %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[, -"AGE", "main"]

  file <- "table_subset_exp_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("NNTable subset remove column in exp", {

  .NNTable <- table %>%
    addExposure(subjects) %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)


  .NNTable <- .NNTable[, -"SEX"]

  file <- "table_subset_exp_sex_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("NNTable subset remove column and subset rows", {

  .NNTable <- table %>%
    addExposure(subjects) %>%
    addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
                 SEX   = c("Female" = "F", "Male" = "M"),
                 AESOC = stringr::str_to_title)

  .NNTable <- .NNTable[SEX == "F", -"SEX"][, -"AGE", "main"]

  file <- "table_subset_exp_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})




# test_that("NNTable subset remove column in exp", {
#
#   .NNTable <- table %>%
#     addExposure(subjects) %>%
#     addTranslate(TRTA  = c("Ico" = "A", "NOVO rapid" = "B"),
#                  SEX   = c("Female" = "F", "Male" = "M"),
#                  AESOC = stringr::str_to_title) %>%
#     addFilling(N = 0)
#
#
#   .NNTable <- .NNTable[, -"SEX"]
#
#   file <- "table_subset_exp_sex_2"
#
#   # print(.NNTable, file = file.path(output_path, "expected", file))
#
#   print(.NNTable, file = file.path(output_path, "got", file))
#
#   expect_equal(read_encoded(file.path(output_path, "expected", file)),
#                read_encoded(file.path(output_path, "got",      file)))
# })

