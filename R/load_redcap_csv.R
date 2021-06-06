#' Load redcap csv data
#'
#' Helper function to do some prepreocessing on the data to avoid mismatching
#' error types due to user error.
#'
#' @param filepath csv filepath
#'
#' @export
load_redcap_csv <- function(filepath) {
  dplyr::mutate(
    readr::read_csv(filepath),
    dplyr::across(tidyselect::contains("timepoint"), as.character)
  )
}
