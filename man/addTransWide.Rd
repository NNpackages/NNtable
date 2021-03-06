% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addTransWide.R
\name{addTransWide}
\alias{addTransWide}
\title{Add columns that needs to be transposed from long to wide}
\usage{
addTransWide(
  .NNTable,
  ...,
  .remove_last_header_row = TRUE,
  .remove_empty_columns = TRUE,
  .remove_empty_level = 1,
  add_cat_space = TRUE,
  fun.aggregate = function(x) gsub("[0-9]", "x", x[1])
)
}
\arguments{
\item{.NNTable}{An \code{NNTable} generated by \code{\link{NNTable}}}

\item{...}{Nested \code{list} of columns that should be transposed. For one
level it can be defined as TRTA = c("N", "P", "E")xx. Nested levels are
added in lists (sex = list(TRTA = c("N", "P", "E"))}

\item{.remove_last_header_row}{\code{logical} indicating if the last header
row should be removed in case only one sublevel is present for each column}

\item{.remove_empty_columns}{\code{logical} indicating if created columns
that are completely empty should be removed.}

\item{.remove_empty_level}{\code{integer} indicating the level from which
empty nested columns are removed. A value of 1 indicates that all empty
columns should be removed. A value of 2 indicates that all columns in level
1 should be empty before the columns are removed.}

\item{add_cat_space}{\code{logical} indicating if a category spacer should be
added}

\item{fun.aggregate}{\code{function} if more than one value is present in
data for each grouping this function will be used to summarise}
}
\value{
An object of class \code{NNTable} with the transpose specified
}
\description{
Add columns that needs to be transposed from long to wide
}
\examples{

## *****************************************************************************
## # Example of transpose from wide to long                                 ----
##
## Calculate the statistics for an AE table
## *****************************************************************************

library(dplyr)
#library(NNR)

# Filter data according Safety analysis set
adsl_f <- adsl            \%>\%
  filter(SAFFL == "Y")    \%>\%
    rename(TRTA = TRT01P)

# Calculate total exposure
totals <- adsl_f        \%>\%
  group_by(TRTA)        \%>\%
  summarize("Number of subjects"     = n_distinct(USUBJID),
            "Total exposure (years)" = sum(TR01DURY))

# Calculate adverse event statistics
output <- adae \%>\% group_by(AEBODSYS, AEDECOD, TRTA) \%>\%
  dplyr::summarise(N = n_distinct(USUBJID),
                   P = N / nrow(adsl),
                   E = n())

# Generate the NNTable ------------------------------------------------------

# Initiate by defining the columns based on ADAE
.NNTable <- NNTable(output, "AEBODSYS", "AEDECOD", "TRTA", "N", "(\%)" = "(P)", "E")

# Add exposure calculated from ADSL
.NNTable <- .NNTable \%>\%
  addExposure(exposure = totals,
              format_alone = FALSE,
              format_data  = c("Number of subjects"     = "\%.0f",
                               "Total exposure (years)" = "\%.2f"))

# Transpose to wide
.NNTable <- .NNTable \%>\% addTransWide(TRTA = c("N", "(\%)", "E"))

# Add Filling such that non-observed events is assigned 0
.NNTable <- .NNTable \%>\% addFilling(N = 0)

# Add formatting information for the table
.NNTable <- .NNTable \%>\% addFormat(format_data = c(N = "\%.0f", P = "\%.2f", E = "\%.0f"))

# Group the columns AEBODSYS and AEDECOD
.NNTable <- .NNTable \%>\% addGroupedColumns("AEBODSYS", "AEDECOD" , name = "some name")

# Truncate columns at a certain width such that long names appear on multiple lines
.NNTable <- .NNTable \%>\% addTruncation(AEBODSYS = 50, AEDECOD = 48)

# View the final result
.NNTable

## *****************************************************************************
## # Example of transpose from with newsted columns                         ----
##
## Calculate the statistics for an AE table grouped by age group
## *****************************************************************************
# create summary
output <- adae \%>\% group_by(AGEGR2, AEBODSYS, AEDECOD, TRTA) \%>\%
  dplyr::summarise(N = n_distinct(USUBJID),
                   P = N / nrow(adsl) * 100,
                   E = n())

# create the NNTable
.NNTable <- NNTable(output, "AGEGR2", "AEBODSYS", "AEDECOD", "TRTA", "N", "(\%)" = "(P)", "E") \%>\%
  addTransWide(AGEGR2 = list(TRTA = c("N", "(\%)", "E"))) \%>\%
  addFilling(N = 0) \%>\%
  addGroupedColumns("AEBODSYS", "AEDECOD") \%>\%
  addTruncation(AEBODSYS = 32, AEDECOD = "30") \%>\%
  addOrder(N = -1)

.NNTable

# change the order of the transpose
.NNTable \%>\% addTransWide(TRTA = list(AGEGR2 = c("N", "(\%)", "E")))

}
