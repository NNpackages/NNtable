

context("Test NNTable addPageSplit")


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




test_that("add page split - keep sex", {

  .NNTable <-  NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex") %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_1"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("add page split - no keepers", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 120)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    # addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit() %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_2"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("add page split - no keepers keep first space", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 120)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    # addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(drop_intial_space = FALSE) %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")



  file <- "table_addPageSplit_3"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})


test_that("add page split - keep sex and grouped", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 120)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex") %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")



  file <- "table_addPageSplit_4"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("add page split - keep sex and grouped small width", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex") %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")



  file <- "table_addPageSplit_5"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})


test_that("add page split - keep sex and grouped wide width", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 200)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex") %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_6"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("add page split - specify cuts with keepers", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex", cuts = c("cohort2", "ico + GIP 3 mg")) %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_7"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})


test_that("add page split - specify all top levels", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex", cuts = sort(unique(adpc$cohort))) %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_8"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("add page split - specify all mid levels", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 81, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex", cuts = sort(unique(adpc$TRTP))) %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  file <- "table_addPageSplit_9"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})





test_that("add page split - specify all mid levels - three pages long", {

  .NNTable <- NNTable(adpc, Sex = "sex", "cohort", "TRTP", "MEAN","MIN", "MAX", page_size = list(page.length = 15, page.width = 80)) %>%
    addTransLong(SUM = c("MEAN", "MIN" , "MAX"), var_name = "Summary") %>%
    addTransWide(cohort = list(TRTP = "SUM")) %>%
    addUnderScore() %>%
    addGroupedColumns("Summary") %>%
    addAlignment(SUM = "c") %>%
    addPageSplit(keepers = "Sex", cuts = sort(unique(adpc$TRTP))) %>%
    addWrapping(title = "how does the title look", footnote = "You need to give a footnote", sys_footnote = "Hi I am a system")


  #print(.NNTable, page = 2)

  file <- "table_addPageSplit_10"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

db <- nnaccess("0001", root = "~/training")

adae_f <- db$adam("adae") %>% filter(SAFFL == "Y" & TRTA != "")
adsl_f <- db$adam("adsl") %>% filter(SAFFL == "Y")

set.seed(100)

# create the NNTable
output <- adae_f %>%
  mutate(TRTA   = paste0(TRTA, "|with a longer and longer|name added"),
         AGEGR2 = sample(c("65<= years", "18<= to <65 years"), n(), replace = TRUE))        %>%
  group_by(AGEGR2, AEBODSYS, AEDECOD, TRTA) %>%
  dplyr::summarise(N = n_distinct(USUBJID),
                   P = N / nrow(adsl_f) * 100,
                   E = n())



output$AGEGR2 <- forcats::fct_relevel(forcats::as_factor(output$AGEGR2),
                                      "65<= years", "18<= to <65 years")
output$TRTA <- forcats::fct_relevel(forcats::as_factor(output$TRTA),
                                    "B|with a longer and longer|name added", "A|with a longer and longer|name added")


test_that("add page split - specify all mid levels - three pages long", {


  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E",
                      page_size = list(page.length = 80, page.width = 78)) %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30") %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore() %>%
    addPageSplit()

  # Check that we get an error when we cannot split
  expect_error({
    a <- print(.NNTable, verbose = FALSE)
  })


  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E",
                      page_size = list(page.length = 80, page.width = 100)) %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30") %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore() %>%
    addPageSplit()


  file <- "table_addPageSplit_12"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))


  .NNTable <- NNTable(output, "AGEGR2", "TRTA", "N", "(%)" = "(P)", "E",
                      page_size = list(page.length = 80, page.width = 99)) %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore() %>%
    addPageSplit()

  file <- "table_addPageSplit_13"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))


  .NNTable <- NNTable(output, "AGEGR2", "TRTA", "N", "(%)" = "(P)", "E",
                      page_size = list(page.length = 80, page.width = 98)) %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore() %>%
    addPageSplit()

  file <- "table_addPageSplit_14"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))

})


