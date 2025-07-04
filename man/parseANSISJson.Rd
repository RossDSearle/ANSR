% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseANSISJson.R
\name{parseANSISJson}
\alias{parseANSISJson}
\title{Parses an ANSIS JSON response into an R ANSIS object}
\usage{
parseANSISJson(ansisResponse, numCPUs = NULL)
}
\arguments{
\item{ansisResponse}{The path to the ANSIS JSON file to parse}
}
\value{
list
}
\description{
Parses an ANSIS JSON response into an R ANSIS object
}
\details{
This function parses an ANSIS JSON response into a form that is more amenable to further massage
}
\examples{
parseANSISJson()
}
\author{
Ross Searle
}
