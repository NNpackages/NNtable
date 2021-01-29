context("Test NNTable")


nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

test_that("Make simple_table on one page", {

  output <- adae %>% filter(TRTA != "") %>% group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  .NNTable <- NNTable(output[1:11, ], "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addFormat(format_data = c(N = "%.0f", P = "%.2f", E = "%.0f"))

  file <- "adae_simple.txt"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("Make simple_table on multiple pages", {

  output <- adae %>% filter(TRTA != "") %>% group_by(AEBODSYS, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(rbind(output, output))

  file <- "adae_simple_mutiple_pages.txt"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("Make grouped table on multiple pages", {

  output <- adae %>% filter(TRTA != "") %>% group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD", remove_duplicated_stub_row = FALSE)



  file <- "adae_group_mutiple_pages.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})


test_that("NNTable trapose to wide with two levels", {

  # create the NNTable
  output <- adae %>% filter(!is.na(TRTA) & TRTA != "") %>%
    group_by(AGEGR2, AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl) * 100,
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30") %>%
    addOrder(N = -1)




  file <- "adae_wide_two_level_01.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  #expect_equal(read_encoded(file.path(output_path, "expected", file)),
  #             read_encoded(file.path(output_path, "got",      file)))


  # change the order of the transpose
  .NNTable <- .NNTable %>% addTransWide(TRTA = list(AGEGR2 = c("N", "(%)", "E")))

  file <- "adae_wide_two_level_02.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  #expect_equal(read_encoded(file.path(output_path, "expected", file)),
  #             read_encoded(file.path(output_path, "got",      file)))


  # reordering of the columns will test the case where a new column is necessar
  # to accommodate the the with of the header name
  output$AGEGR2 <- forcats::fct_relevel(forcats::as_factor(output$AGEGR2),
                                        "65<= years", "18<= to <65 years")
  output$TRTA <- forcats::fct_relevel(forcats::as_factor(output$TRTA),
                                      "IGlar", "I287")

  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0)                                      %>%
    addGroupedColumns("AEBODSYS", "AEDECOD")               %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30")           %>%
    addOrder(N = -1)

  file <- "adae_wide_two_level_03.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  #expect_equal(read_encoded(file.path(output_path, "expected", file)),
  #             read_encoded(file.path(output_path, "got",      file)))


  # switching will test the case where a column is smaller than the nested
  .NNTable <- .NNTable %>% addTransWide(TRTA = list(AGEGR2 = c("N", "(%)", "E")))

  file <- "adae_wide_two_level_04.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  #expect_equal(read_encoded(file.path(output_path, "expected", file)),
  #             read_encoded(file.path(output_path, "got",      file)))


  # test the spanning rows for group columns
  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0)                                      %>%
    addGroupedColumns("AEBODSYS", "AEDECOD")               %>%
    addTruncation( AEDECOD = "30")           %>%
    addOrder(N = -1)

  file <- "adae_wide_two_level_05.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})


test_that("NNTable trapose to wide with two levels and split header", {

  # create the NNTable
  output <- adae %>%
    mutate(TRTA = paste0(TRTA, "|with a long|name added"))        %>%
    group_by(AGEGR2, AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl) * 100,
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30") %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore()

  file <- "adae_wide_two_level_splithead_01.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))



  # change the order of the transpose
  .NNTable <- .NNTable %>% addTransWide(TRTA = list(AGEGR2 = c("N", "(%)", "E")))

  file <- "adae_wide_two_level_splithead_02.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))



  output$AGEGR2 <- forcats::fct_relevel(forcats::as_factor(output$AGEGR2),
                                        "65<= years", "18<= to <65 years")
  output$TRTA <- forcats::fct_relevel(forcats::as_factor(output$TRTA),
                                      "IGlar|with a long|name added", "I287|with a long|name added")

  .NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addTransWide(AGEGR2 = list(TRTA = c("N", "(%)", "E"))) %>%
    addFilling(N = 0) %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTruncation(AEBODSYS = 32, AEDECOD = "30") %>%
    addOrder(N = -1) %>%
    addCellSplit() %>%
    addUnderScore()

  file <- "adae_wide_two_level_splithead_03.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))


  .NNTable <- .NNTable %>%
    addTransWide(TRTA = list(AGEGR2 = c("N", "(%)", "E")))

  file <- "adae_wide_two_level_splithead_04.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("NNTable transpose to wide table on multiple pages with spaces in name", {

  output <- adae %>% group_by(AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output, c("Adverse event" = "AEDECOD", "Treat ment" = "TRTA", "N", "( % )"= "(P)", "E")) %>%
    addTransWide("Treat ment" = c("N", "( % )", "E")) %>% addFormat(dec = 1)

  file <- "adae_wide_mutiple_pages_space_name.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})



test_that("NNTable addFilling before addFormat", {

  output <- adae %>% group_by(AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output) %>%
    addTransWide(TRTA = c("N", "P", "E")) %>%
    addFilling(N =  0) %>%
    addFormat(dec = 1)


  file <- "adae_wide_filling_format.txt"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})



test_that("NNTable grouup transpose to wide on multiple pages", {

  output <- adae %>% group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output) %>% addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTransWide(TRTA = c("N", "P", "E")) %>%
    addFilling(N = 0) %>%
    addFormat(dec = 1)


  file <- "adae_wide_grouped.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))

})

test_that("NNTable grouup transpose to wide on multiple pages", {

  output <- adae %>% mutate(TRTA = "Total") %>%
    bind_rows(adae) %>%
    group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl) * 100,
                     P_order = -P,
                     E = n())

  # create the NNTable
  .NNTable <- NNTable(output, "AEBODSYS", "AEDECOD", "TRTA", "N", "P", "E") %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTransWide(TRTA = c("N", "P", "E")) %>%
    addFilling(N = 0)  %>%
    addFormat(dec = 1) %>%
    addOrder(P_order = 1)


  file <- "adae_wide_grouped_order_nintable.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})

test_that("NNTable grouup transpose to wide on multiple pages", {

  output <- adae %>% mutate(TRTA = "Total") %>%
    bind_rows(adae) %>%
    group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl) * 100,
                     E = n())


  # create the NNTable
  .NNTable <- NNTable(output, "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addGroupedColumns("AEBODSYS", "AEDECOD") %>%
    addTransWide(TRTA = c("N", "(%)", "E")) %>%
    addTruncation(AEDECOD = 30) %>%
    addFilling(N = 0) %>%
    addFormat(dec = 1) %>%
    addOrder(P = -1, E = -1)

  file <- "adae_wide_grouped_morder_spancol.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))
})



test_that("NNTable add exposure table", {

  adsl_f <- adsl %>%
    filter(FASFL == "Y") %>%
    mutate(TRTA = TRT01P)

  totals <- adsl_f %>%
    group_by(TRTA) %>%
    summarize("Number of subjects"     = n_distinct(USUBJID),
              "Total exposure (years)" = sum(TR01DURY))


  # format_data <- data.frame(AEBODSYS = c("Number of subjects", "Total exposure (years)"),
  #                           N = c("%.0f", "%.2f"))


  output <- adae %>% group_by(AEBODSYS, AEDECOD, TRTA) %>%
    dplyr::summarise(N = n_distinct(USUBJID),
                     P = N / nrow(adsl),
                     E = n())

  .NNTable <- NNTable(output, "AEBODSYS", "AEDECOD", "TRTA", "N", "(%)" = "(P)", "E") %>%
    addFormat(format_data = c(N = "%.0f", P = "%.2f", E = "%.0f"))

  # check that we get a warning when the table is too wide
  expect_warning({
    a <- print(.NNTable, verbose = FALSE)
  })


  .NNTable <- addExposure(.NNTable, exposure = totals,
                          format_alone = FALSE,
                          format_data  = c("Number of subjects"     = "%.0f",
                                           "Total exposure (years)" = "%.2f"))

  file <- "adae_wide_grouped_exposure_01.txt"

  # print(.NNTable, file = file.path(output_path, "expected", file))

  expect_warning({
    print(.NNTable, file = file.path(output_path, "got", file))
  })

  expect_equal(read_encoded(file.path(output_path, "expected", file)),
               read_encoded(file.path(output_path, "got",      file)))



  # add stuf to the table and check that it still works
  .NNTable <- addTransWide(.NNTable, TRTA = c("N", "(%)", "E"))
  .NNTable <- addGroupedColumns(.NNTable, "AEBODSYS", "AEDECOD" , name = "some name", span_row = FALSE)
  .NNTable <- addFilling(.NNTable, N = 0)
  .NNTable <- .NNTable %>% addTruncation(AEBODSYS = 32, AEDECOD = "30") %>% addOrder(N = -1)

  file <- "adae_wide_grouped_exposure_02.txt"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  # fix in new version
  #expect_equal(read_encoded(file.path(output_path, "expected", file)),
  #             read_encoded(file.path(output_path, "got",      file)))
})




