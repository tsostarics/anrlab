% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_cinderella_chat.R
\name{process_cinderella_chat}
\alias{process_cinderella_chat}
\title{Process Narrative Metrics from Raw CHAT Output}
\usage{
process_cinderella_chat(
  filepath,
  record_id = NA,
  instance = NA,
  entry = NA,
  administrator = NA,
  date = NA,
  study = NA,
  timepoint = NA,
  record_id_col = "demo_record_id",
  inst_name = "cinderella_narrative_summary"
)
}
\arguments{
\item{filepath}{Character, Filepath to a .csv or .xlsx file created by CHAT}

\item{record_id}{Character or Numeric, Record ID for this participant}

\item{instance}{Character or Numeric, Instance number for this participant's
narrative sample}

\item{entry}{Person doing data entry}

\item{administrator}{Character, researcher NetID}

\item{date}{Character, Date of the sample in MM-DD-YYYY format}

\item{study}{Character, Which study is this sample for?}

\item{timepoint}{Character, Which timepoint of the study is this sample for?}

\item{record_id_col}{Defaults to record_id}

\item{inst_name}{Defaults to cinderella_narrative_summary}
}
\value{
A dataframe ready to be exported to redcap
}
\description{
Takes the output from CHAT for a narrative sample and converts it into a form
that can be directly read into Redcap. This can be done through an API write
call or through uploading it manually.
}
