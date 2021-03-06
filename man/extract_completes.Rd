% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_completes}
\alias{extract_completes}
\title{Extract Completion Status}
\usage{
extract_completes(report_data, record_id_col = "record_id")
}
\arguments{
\item{report_data}{Raw report data from get_report or importing from .csv}

\item{record_id_col}{column name for the record id, defaults to the lab's record_id}
}
\value{
A tibble containing the completion status value for each instrument
for every participant in the given report
}
\description{
Extract the completion status for each participant and each instrument in
the given raw report data. This isn't terribly useful, but could be helpful
when trying to exclude any data entry that isn't finished.
}
