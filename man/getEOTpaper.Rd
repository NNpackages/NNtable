% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEOTpaper.R
\name{getEOTpaper}
\alias{getEOTpaper}
\title{Get EOT paper size}
\usage{
getEOTpaper(
  orientation = c("port", "land"),
  font_size = 8,
  figure_size = c("defeault", "full"),
  type = c("table", "listing", "figure"),
  format = "txt"
)
}
\arguments{
\item{orientation}{The orientation of the page}

\item{font_size}{the font size. Only applicable for figure}

\item{figure_size}{Should the size be default or full}

\item{type}{Should the output be character widths or figure width in cm.}

\item{format}{\code{character} the type of output, currently \code{"txt"} or \code{"flextable"}}
}
\value{
list with paper size
}
\description{
Get EOT paper size
}
\examples{
getEOTpaper("Port", 8, type = "Table")
}
