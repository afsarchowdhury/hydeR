% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_hhs_class_list_functions.R
\name{hhs_class_list_teacher}
\alias{hhs_class_list_teacher}
\title{Get teacher-focused class list.}
\usage{
hhs_class_list_teacher(
  academicYear,
  staffCode = NULL,
  yearGroupFrom = "7",
  yearGroupTo = "11"
)
}
\arguments{
\item{academicYear}{academic year as integer.}

\item{staffCode}{case-sensitive staff code as string.}

\item{yearGroupFrom}{starting year group as string.  Defaults to "7".}

\item{yearGroupTo}{ending year group as string.  Defaults to "11".}
}
\description{
Returns clean details of all classes in the chosen academic year for chosen
staff.  If staff code is not provided, class list for all staff are returned.
}
\examples{
hhs_class_list_teacher(2020, "ACH")
hhs_class_list_teacher(2020)
}
