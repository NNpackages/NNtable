% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addPageSplit.R
\name{balanced.split}
\alias{balanced.split}
\title{Main function that splits values into n.pages
chunks, minimizing sd(sum(chunk)) + I(p_width>sum(chunk)) * Inf}
\usage{
balanced.split(values, n.pages, p_width = Inf)
}
\description{
Main function that splits values into n.pages
chunks, minimizing sd(sum(chunk)) + I(p_width>sum(chunk)) * Inf
}
\keyword{internal}
