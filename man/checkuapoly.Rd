% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_summaries.R
\name{checkuapoly}
\alias{checkuapoly}
\title{check if an urban area sf is multipolygon formatted}
\usage{
checkuapoly(ua)
}
\arguments{
\item{ua}{a sf}
}
\value{
an error if not a multipolygon formatted sf
}
\description{
check if an urban area sf is multipolygon formatted
}
\examples{
data("ua2000"); class(ua2000)
#will produce error (not run) `checkuapoly(ua2000)`
}
