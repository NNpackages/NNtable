context("Test NNTable summary")


nnr_dir <- gsub("/NNR-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nnr_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)

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
            Mean = mean(AVAL, na.rm = TRUE),
            SD   = sd(  AVAL, na.rm = TRUE),
            Min  = min( AVAL, na.rm = TRUE),
            Max  = max( AVAL, na.rm = TRUE)
  )


test_that("Make simple summary table on one page", {

  # The NNTable is generated in small steps to view what happens

  # Initiate the table
  .NNTable <- NNTable(output, "PARAM", "TRTP", "N", "Mean (SD)", "Min ; Max")

  
  file <- "adlb_simple_summary.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
  
  
})
  
test_that("Make summary with tran_to_long one page", {
  
  .NNTable <- NNTable(output, "PARAM", "TRTP", "N", "Mean (SD)", "Min ; Max")

  # Transpose the summary addTransLong and addFormat
  .NNTable <- .NNTable                                    %>%
    addTransLong(SUM = c("N", "Mean (SD)" , "Min ; Max"),
                 var_name = "Called var")                 %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2)

  
  file <- "adlb_summary_tranToLong_format.txt"
  
  #print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
  
})
  
test_that("Make summary with tran_to_long tran_to_wide one page", {
  
  .NNTable <- NNTable(output, "PARAM", "TRTP", "N", "Mean (SD)", "Min ; Max")
  
  # Transpose the summary addTransLong and addFormat
  .NNTable <- .NNTable                                    %>%
    addTransLong(SUM = c("N", "Mean (SD)" , "Min ; Max"),
                 var_name = "Called var")                 %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2)
  
  
  # Add exposure, transpose to wide and ad grouping variables
  .NNTable <- addExposure(.NNTable, exposure = totals,
                          format_alone = TRUE,
                          format_data  = c("Number of subjects" = "%.0f")) %>%
              addTransWide(TRTP = "SUM")  %>%
              addGroupedColumns("PARAM", "Called var")

  
  file <- "adlb_summary_Long_wide_format.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Make summary with tran_to_long tran_to_wide group one page", {
  
  .NNTable <- NNTable(output, "PARAM", "TRTP", "N", "Mean (SD)", "Min ; Max")
  
  # Transpose the summary addTransLong and addFormat
  .NNTable <- .NNTable                                    %>%
    addTransLong(SUM = c("N", "Mean (SD)" , "Min ; Max"),
                 var_name = "Called var")                 %>%
    addFormat(format_data = c(N = "%.0f"), dec = 2)
  
  
  # Add exposure, transpose to wide and ad grouping variables
  .NNTable <- addExposure(.NNTable, exposure = totals,
                          format_alone = TRUE,
                          format_data  = c("Number of subjects" = "%.0f")) %>%
    addTransWide(TRTP = "SUM")  %>%
    addGroupedColumns("PARAM", "Called var")

  
  file <- "adlb_summary_Long_wide_group_format.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


test_that("Make summary from adsl", {
  
  cols <- c("AGE", "HGTCBL", "WGTBL",  "BMIBL", "HBA1CBL", "FPGCBL")
  by_vars <- rlang::syms(c("COUNTRY2", "AGEGR2", "TRT01P", "LABEL"))
  
  adsl_long <- adsl %>% filter(FASFL == "Y") %>% 
    pivot_longer(col = cols)
  
  adsl_long$LABEL <- 
    unlist(labelled::var_label(adsl))[adsl_long$name] 
  
  output <- adsl_long %>% group_by(!!! by_vars) %>% 
    summarise(N    = n_distinct(USUBJID),
              Mean = mean(value),
              SD   = sd(value),
              Min  = min(value),
              Max  = max(value))
  
  output$SD[is.na(output$SD)] <- NaN
  
  NNTable <- NNTable(output)
  
  NNTable %>% addGroupedColumns("LABEL", "COUNTRY2")
  
  .NNTable <- NNTable %>% 
    addTransLong(sum = c(c("N", "Mean", "SD", "Min", "Max"))) %>% 
    addTransWide(TRT01P = "sum") %>% 
    addGroupedColumns("COUNTRY2", "LABEL", "AGEGR2", "name")
  
  
  file <- "adsl_summary_no_breakes_on_first_level.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})




test_that("Make summary from adsl 2", {
  
  cols <- c("AGE", "HGTCBL", "WGTBL",  "BMIBL", "HBA1CBL", "FPGCBL")
  by_vars <- rlang::syms(c("COUNTRY2", "AGEGR2", "TRT01P", "LABEL"))
  
  adsl_long <- adsl %>% filter(FASFL == "Y") %>% 
    pivot_longer(col = cols)
  
  adsl_long$LABEL <- 
    unlist(labelled::var_label(adsl))[adsl_long$name] 
  
  output <- adsl_long %>% group_by(!!! by_vars) %>% 
    summarise(N    = n_distinct(USUBJID),
              Mean = mean(value, na.rm = TRUE),
              SD   = sd(value, na.rm = TRUE),
              Min  = min(value, na.rm = TRUE),
              Max  = max(value, na.rm = TRUE))
  
  output$SD[is.na(output$SD)] <- NaN
  
  NNTable <- NNTable(output, "COUNTRY2", "LABEL", "AGEGR2", "TRT01P", "N", 
          "Mean (SD)" = "Mean (SD)", "Min ; Max" = "Min ; Max")
  
  .NNTable <-  NNTable %>% 
    addTransLong(sum = c("N", "Mean (SD)", "Min ; Max")) %>% 
    addTransWide(TRT01P = "sum") %>% 
    addGroupedColumns("COUNTRY2", "LABEL", "AGEGR2", "name")
  
  
  file <- "adsl_summary_no_breakes_on_first_level_2.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Make summary from adsl 3", {
  
  cols <- c("AGE", "HGTCBL", "HBA1CBL", "FPGCBL")
  by_vars <- rlang::syms(c("COUNTRY2", "AGEGR2", "TRT01P", "LABEL"))
  
  adsl_long <- adsl %>% filter(FASFL == "Y") %>% 
    pivot_longer(col = cols)
  
  adsl_long$LABEL <- 
    unlist(labelled::var_label(adsl))[adsl_long$name] 
  
  output <- adsl_long %>% group_by(!!! by_vars) %>% 
    summarise(N    = n_distinct(USUBJID),
              Mean = mean(value, na.rm = TRUE),
              SD   = sd(value, na.rm = TRUE),
              Min  = min(value, na.rm = TRUE),
              Max  = max(value, na.rm = TRUE))
  
  
  output$SD[is.na(output$SD)] <- NaN
  
  NNTable <- NNTable(output, "COUNTRY2", "LABEL", "AGEGR2", "TRT01P", "N", 
                     "Mean (SD)" = "Mean (SD)", "Min ; Max" = "Min ; Max")
  
  .NNTable <-  NNTable %>% 
    addTransLong(sum = c("N", "Mean (SD)", "Min ; Max")) %>% 
    addTransWide(TRT01P = "sum") %>% 
    addGroupedColumns("COUNTRY2", "LABEL", "AGEGR2", "name")
  
  
  file <- "adsl_summary_all_breakes_on_first_level.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})

test_that("Make summary from adsl 4", {
  
  cols <- c("AGE", "HGTCBL", "WGTBL",  "BMIBL", "HBA1CBL", "FPGCBL")
  by_vars <- rlang::syms(c("COUNTRY2", "AGEGR2", "TRT01P", "LABEL"))
  
  adsl_long <- adsl %>% filter(FASFL == "Y") %>% 
    pivot_longer(cols = all_of(cols))
  
  adsl_long$LABEL <- 
    unlist(labelled::var_label(adsl))[adsl_long$name] 
  
  output <- adsl_long %>% group_by(!!! by_vars) %>% 
    summarise(N    = n_distinct(USUBJID),
              Mean = mean(value, na.rm = TRUE),
              SD   = sd(value, na.rm = TRUE),
              Min  = min(value, na.rm = TRUE),
              Max  = max(value, na.rm = TRUE))
  
  output$SD[is.na(output$SD)] <- NaN
  
  .NNTable <- NNTable(output) %>% addFormat(dec = 2) %>% 
    addGroupedColumns("COUNTRY2", "LABEL", "name") %>% 
    addTransLong(sum = c("N", "Mean", "SD", "Min", "Max")) %>% 
    addTransWide(AGEGR2 = list(TRT01P = "sum")) 
  
  
  file <- "adsl_summary_all_breakes_on_first_level_2.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
  
})

test_that("Make summary from adsl with wrapping 5", {
  
  cols <- c("AGE", "HGTCBL", "WGTBL",  "BMIBL", "HBA1CBL", "FPGCBL")
  by_vars <- rlang::syms(c("COUNTRY2", "AGEGR2", "TRT01P", "LABEL"))
  
  adsl_long <- adsl %>% filter(FASFL == "Y") %>% 
    pivot_longer(col = cols)
  
  adsl_long$LABEL <- 
    unlist(labelled::var_label(adsl))[adsl_long$name] 
  
  output <- adsl_long %>% group_by(!!! by_vars) %>% 
    summarise(N    = n_distinct(USUBJID),
              Mean = mean(value, na.rm = TRUE),
              SD   = sd(value, na.rm = TRUE),
              Min  = min(value, na.rm = TRUE),
              Max  = max(value, na.rm = TRUE))
  
  output$SD[is.na(output$SD)] <- NaN
  
  NNTable <- NNTable(output, "COUNTRY2", "LABEL", "AGEGR2", "TRT01P", "N", 
                     "Mean (SD)" = "Mean (SD)", "Min ; Max" = "Min ; Max")
  
  
  .NNTable <- NNTable %>% 
    addTransLong(sum = c("N", "Mean (SD)", "Min ; Max")) %>% 
    addTransWide(AGEGR2 = list(TRT01P = "sum")) %>% 
    addGroupedColumns("LABEL", "COUNTRY2", "name", span_row = TRUE)
  
  
  .NNTable <- .NNTable %>% addWrapping(title = "I need a title", footnote = "And a small footnote",
                                       sys_footnote = "hej")
  
  
  file <- "adsl_summary_all_breakes_on_first_level_wrapping.txt"
  
  # print(.NNTable, file = file.path(output_path, "expected", file))
  
  print(.NNTable, file = file.path(output_path, "got", file))
  
  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})




