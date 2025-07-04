% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API.R
\name{getSingleSite}
\alias{getSingleSite}
\title{Get a single site}
\usage{
getSingleSite(providerID, siteID, format = "ANSISDataObject")
}
\arguments{
\item{providerID}{ANSIS Data Provider ID}

\item{siteID}{ANSIS site ID}
}
\value{
logical
}
\description{
Get a single site
}
\details{
Returns all the data for a single site in a number of formats. These are, raw json (JSON), a parsed object for easier access to data (ANSISDataObject), a flat csv file (CSV) or a HTML site description (HTML)
}
\author{
Ross Searle
}
