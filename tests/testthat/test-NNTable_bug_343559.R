# test created for bug 343559

# NNTable fails when column names have a string in common

context("Test NNTable bug 343559")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

(db1 <- nnaccess("0001", root = "~/training"))

adlb_f <- db1$adam("adlb") %>% filter(FASFL == "Y" & TRTP != "" & ANL01FL == "Y" & TOPIC_CD == "HBA1C_BLOOD")

stats <- adlb_f %>%
  group_by(TRTP,PARAMCD,AVISIT) %>%
  summarise(N = n_distinct(USUBJID),
            MEAN = mean(AVAL),
            GEOMEAN = MEAN*2.75)

# Create NNTable ---------------------------------------------------------------


test_that("Bug 343559 test nested names", {

  .NNTable <- NNTable(stats, "TRTP","N", Mean = "MEAN", "Geo Mean" = "GEOMEAN")

  file <- "table_343559_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})



stats <- adlb_f %>%
  group_by(TRTP,PARAMCD,AVISIT) %>%
  summarise(N = n_distinct(USUBJID),
            MEAN = mean(AVAL),
            GEOMEAN = MEAN*2.75,
            CV = log(GEOMEAN))


test_that("Bug 343559 test nested names 2", {

  .NNTable <-  NNTable(stats, "TRTP","N","GEOMEAN (CV)")

  file <- "table_343559_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Bug 343559 test nested names 3", {

  .NNTable <-  NNTable(stats, "TRTP","N","MEAN","Geom.mean (CV%)" = "GEOMEAN (CV)")

  file <- "table_343559_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})
