

# test created for bug 431616

# When addFilling is used together with addExposure and the filled column is NA for all it should not be shown

context("Test NNTable bug 431616")

nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)


db <- nnaccess("0002", root = "~/training")

data_f <- db$adam("adhypo") %>% filter( SAFFL == "Y")

adsl_f <- db$adam("adsl") %>%
  filter(SAFFL == "Y") %>%
  pivot_longer(cols = c("TRT01P"), values_to = "TRTA")

#Total exposure for table
totals <- adsl_f %>%
  mutate(TRTA = "Total", TRTAN = 9999) %>%
  bind_rows(filter(adsl_f, )) %>%
  group_by(TRTA) %>%
  summarise("Number of subjects" = n_distinct(USUBJID))

#For tables with only some of the treatment categories
totals <- filter(totals, TRTA %in% c(data_f$TRTA, "Total"))

sumcontlab <- data_f %>%
  mutate(TRTA = "Total", TRTAN = 9999) %>%
  bind_rows(filter(data_f, )) %>%
  group_by(ACAT4, TRTA, TRTAN) %>%
  summarise("Number of episodes" = n(),
            Mean   = mean(GLUCLOW, na.rm = TRUE),
            SD     = sd(GLUCLOW, na.rm = TRUE),
            Median = median(GLUCLOW, na.rm = TRUE),
            Min    = min(GLUCLOW, na.rm = TRUE),
            Max    = max(GLUCLOW, na.rm = TRUE), .groups="drop") %>%
  mutate(ord = factor(ACAT4, levels = c("HYPOGLYCAEMIA ALERT VALUE (LEVEL 1)", "CLINICALLY SIGNIFICANT HYPOGLYCAEMIA (LEVEL 2)",
                                        "SEVERE HYPOGLYCAEMIA (LEVEL 3)", "UNCLASSIFIABLE"))) %>%
  mutate(ACAT4 = stringr::str_to_sentence(ACAT4))

# Generate NNTable ----------------------------------------------------------
.NNTable <- NNTable(sumcontlab, "ACAT4", "TRTA", "Number of episodes",
                    "Mean (SD)", "Median", "Min ; Max") %>%
  addTransLong(SUM = c("Number of episodes", "Mean (SD)", "Median", "Min ; Max"),
               var_name = "Called var") %>%
  addExposure(exposure = totals,
              format_alone = F,
              format_data = c("Number of subjects" = "%.0f")) %>%
  addFormat(format_data = c("Number of episodes" = "%.0f", "Mean" = "%.2f", "SD" = "%.3f", Min = "%.2f", Max = "%.2f"),
            dec = 2) %>%
  addTransWide(TRTA = "SUM") %>%
  addGroupedColumns("ACAT4","Called var") %>%
  addFilling("Number of episodes" = 0) %>%
  addOrder(ord = 1)


test_that("Bug 431616 test 1", {


  file <- "table_431616_1"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


