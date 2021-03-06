% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addGroupedColumns.R
\name{addGroupedColumns}
\alias{addGroupedColumns}
\title{Add grouped columns to an NNTable to form the table STUB}
\usage{
addGroupedColumns(
  .NNTable,
  ...,
  name = "",
  invisible = character(0),
  span_row = TRUE,
  add_blank_row = TRUE,
  add_blank_row_when = c("missing", "always"),
  add_blank_row_invisible = FALSE,
  add_header_rows_invisible = TRUE,
  remove_duplicated_stub_row = TRUE
)
}
\arguments{
\item{.NNTable}{An \code{NNTable} generated by \code{\link{NNTable}}}

\item{...}{elements with the names of the columns to group}

\item{name}{\code{character} with the name of grouping column}

\item{invisible}{\code{character} Grouped columns that should not be
displayed}

\item{span_row}{\code{logical} should group columns span the entire row}

\item{add_blank_row}{\code{logical} Add a blank line between every grouping.
Alternative \code{character} with the column names of the group columns for
which a blank should be added. Alternatively, it can be a \code{character}
specifying which grouping columns should have a blank added, the grouping
name can be assigned a value of either \code{"missing"} or \code{"always"}
which overrules \code{add_blank_row_when}.}

\item{add_blank_row_when}{\code{character}, if set equal to \code{"missing"}
blank rows are only added when an group header is missing, i.e. if the
header is supplied in the data no blank row is added. When set equal to
\code{always} it is added regardless of the group header was supplied in
the data.}

\item{add_blank_row_invisible}{\code{logical} should blank rows be added for
invisible columns. Note, that this is an additional empty row compared to
the header row that is blank. Ignored if \code{add_blank_row} is a
character.}

\item{add_header_rows_invisible}{\code{logical} Should a blank header row be
added to invisible grouped columns. Alternative, \code{character} with
invisible groups that should have a blank header.}

\item{remove_duplicated_stub_row}{\code{logical} indicating whether or not
duplicated rows should be displayed or be left blank. Defaults to TRUE as
this is usually the wanted behavior, where the repeats are considered noise}
}
\value{
An object of class \code{NNTable} with the column grouping specified
}
\description{
Add grouped columns to an NNTable to form the table STUB
}
\examples{

## *****************************************************************************
## # Example of transpose from wide to long                                 ----
##
## Calculate the statistics for an AE table
## *****************************************************************************

library(dplyr)
# library(NNR)

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

}
