% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_hhs_behaviour_functions.R
\name{hhs_detentions}
\alias{hhs_detentions}
\title{Get clean school detention data.}
\usage{
hhs_detentions(academicYear, goDateStart, goDateEnd)
}
\arguments{
\item{academicYear}{academic year as integer.}

\item{goDateStart}{start date of range as string in the form yyyy-mm-dd.}

\item{goDateEnd}{end date of range as string in the form yyyy-mm-dd.}
}
\description{
Returns clean school detention data for chosen academic year and date range,
along with a line plot.
}
\examples{
hhs_detentions(2022, "2021-09-01", "2021-09-30")
}
