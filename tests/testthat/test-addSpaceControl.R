

context("Test NNTable addSpaceControl")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)


## Treatment arm
trt <- c("sema",
         "sema + GIP 2 mg",
         "sema + GIP 3 mg",
         "GIP 2 mg",
         "GIP 3 mg",
         "ico",
         "ico + GIP 2 mg",
         "ico + GIP 3 mg",
         "Iglar GIP 2 mg",
         "Iglar GIP 3 mg")

set.seed(100)

# Dummy ADPP data
m <- lvm()
m <- categorical(m, ~TRTP, K = 10, p = rep(0.1, 10), labels = trt)
distribution(m, ~AVAL) <- normal.lvm(mean = 120, sd = 20)

adpc <- as_tibble(sim(m, 200))

## Add cohorts
adpc <- adpc %>%
  mutate(cohort = case_when(grepl("sema", TRTP)   ~ "cohort1",
                            TRTP %in% c("GIP 2 mg", "GIP 3 mg") ~ "cohort2",
                            grepl("ico", TRTP)   ~ "cohort3",
                            TRUE ~ "cohort4"),
         sex = sample(c("Female", "Male + a long name"), n(), replace = TRUE)) %>%
  group_by(cohort, TRTP, sex) %>%
  summarize(MEAN = mean(AVAL),
            MIN = min(AVAL),
            MAX = max(AVAL))



table <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
  addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
  addTransWide(cohort = list(TRTP = "SUM")) %>%
  addUnderScore() %>%
  addGroupedColumns("Summary") %>%
  addAlignment(SUM = "c") %>%
  addPageSplit(keepers = "Sex", cuts = sort(unique(adpc$TRTP))) %>%
  addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")



test_that("control space - no spread", {

  .NNTable <-  table %>% addSpaceControl(spread = FALSE)

  file <- "table_addSpaceControl_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("control space - set sep and space", {

  .NNTable <-  table %>% addSpaceControl(spread = FALSE, sep = 10, space = 20)

  file <- "table_addSpaceControl_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("control space - set max sep and max.space", {

  .NNTable <-  table %>% addSpaceControl(spread = TRUE, max.sep = 10, max.space = 20)

  file <- "table_addSpaceControl_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("control space - get warning", {

  .NNTable <-  table %>% addSpaceControl(spread = FALSE, sep = 10, space = 100)


  expect_warning({
    a <- print(.NNTable, verbose = FALSE)
  })
})


