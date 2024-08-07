% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fac2seg.R
\name{fac2seg}
\alias{fac2seg}
\title{Factor to Segments}
\usage{
fac2seg(fac)
}
\arguments{
\item{fac}{A factor where each level represents a segment}
}
\value{
A list of vectors, each vector contains the indices of the elements
of the corresponding segment
}
\description{
Factor to Segments
}
\examples{
fac <- factor(c("a", "b", "a", "b", "c", "c"))
fac2seg(fac)
}
