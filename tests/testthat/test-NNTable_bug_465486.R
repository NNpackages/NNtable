
# test created for bug 465486

# Issue with multiple treatment arms when the SOC-PTs are not
# coming from all treatment arms.

context("Test NNTable bug 465486")

nntable_dir <- gsub("/NNtable-tests/testthat|/tests/testthat", "", getwd())
output_path <- file.path(nntable_dir, "tests",  "output")

dir.create(file.path(output_path, "got"), showWarnings = FALSE)


(db <- nnaccess(project = "nn1234", trial = "0002", root="~/training"))


## *****************************************************************************
## # Example of transpose from wide to long                                 ----
##
## Calculate the statistics for an AE table
## *****************************************************************************

library(dplyr)


# Filter data according Safety analysis set
adsl_f <-db$adam("adsl") %>%
  filter(SAFFL == "Y")   %>%
  rename(TRTA=TRT01A)

# Calculate total exposure
totals1 <- adsl_f   %>%
  group_by(TRTA)        %>%
  summarize("Number of subjects"     = n_distinct(USUBJID),
            "Total exposure (years)" = sum(TR01DURY)) %>%
  mutate(TRT=ifelse(TRTA=="Twix","J","IGLAR"))


totals2 <- adsl_f   %>%
  group_by(TRTA)        %>%
  summarize("Number of subjects"     = n_distinct(USUBJID),
            "Total exposure (years)" = sum(TR01DURY)) %>%
  mutate(TRT=ifelse(TRTA=="Twix","ICOIGLAR","IGLARICO"))

totals=rbind(totals1,totals2)

adae <- db$adam("adae")  %>% filter(SAFFL=="Y" &
                                      TRTA !="" & (AREL1 %in% c("PROBABLE","POSSIBLE") |
                                                     AREL2 %in% c("PROBABLE","POSSIBLE") ))


f=mutate(adae,TRT=ifelse(adae$TRTA=="Twix" & (adae$AREL1 %in% c("POSSIBLE",
                                                                "PROBABLE")),"J",
                         ifelse(adae$TRTA=="Mars" & (adae$AREL2 %in% c("POSSIBLE",
                                                                       "PROBABLE")),"IGLAR",
                                ifelse(adae$TRTA=="Twix" & (adae$AREL2 %in% c("POSSIBLE",
                                                                              "PROBABLE")) ,"ICOIGLAR",
                                       ifelse(adae$TRTA=="Mars" & (adae$AREL1 %in% c("POSSIBLE",
                                                                                     "PROBABLE")) ,"IGLARICO","")))))

f <- f %>% filter(TRT %in% c("J" , "IGLAR"))

# Calculate adverse event statistics
output <-  f %>%
  group_by(AEBODSYS, AEDECOD, TRT,TRTA) %>%
  dplyr::summarise(N = n_distinct(USUBJID),
                   E = n())

output1 <- output %>% merge(totals,by="TRT")

output2 <- output1 %>%
  select("AEBODSYS","AEDECOD","TRT","N","E","Number of subjects",
         "Total exposure (years)") %>%
  group_by(AEBODSYS, AEDECOD, TRT) %>%
  dplyr::summarise(P = N/`Number of subjects`*100,
                   R=E/`Total exposure (years)`*100)

out11 <- output %>% merge(output2,by=c("AEBODSYS","AEDECOD","TRT")) %>%
  select(-c("TRTA"))


totals_fin <- totals %>%
  select(-c("TRTA"))

# Generate the NNTable ------------------------------------------------------

# Initiate by defining the columns based on ADAE
.NNTable <- NNTable(out11, "AEBODSYS", "AEDECOD", "TRT", "N", "(%)" = "(P)", "E","R")

# Add exposure calculated from ADSL
.NNTable <- .NNTable %>%
  addExposure(exposure = totals_fin,
              format_alone = FALSE,
              format_data  = c("Number of subjects"     = "%.0f",
                               "Total exposure (years)" = "%.2f"))

.NNTable
# Transpose to wide
.NNTable <- .NNTable %>% addTransWide(TRT = c("N", "(%)", "E","R"), .remove_empty_columns = TRUE)

.NNTable <- .NNTable                       %>%
  addGroupedColumns("AEBODSYS", "AEDECOD") %>%
  addUnderScore() %>%
  addFilling(N = 0)



test_that("Bug 465486 test 1", {


  file <- "table_465486_1"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})



test_that("Bug 465486 test 2", {

  .NNTable <- .NNTable %>% addTransWide(TRT = c("N", "(%)", "E","R"), .remove_empty_columns = FALSE) %>%
    addTruncation(AEDECOD = 20)

  file <- "table_465486_2"

  #print(.NNTable, file = file.path(output_path, "expected", file))

  print(.NNTable, file = file.path(output_path, "got", file))

  expect_equal(readLines(file.path(output_path, "expected", file)),
               readLines(file.path(output_path, "got",      file)))
})


