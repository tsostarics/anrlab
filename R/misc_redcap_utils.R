#' Get Redcap Report
#'
#' @param rep_id Report ID on Redcap
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#' @param redcap_username Redcap username (NetID for Northwestern). This is used
#' to verify data export privileges.
#'
#' @return A tibble representation of the specified redcap report
#' @export
#'
get_report <- function(rep_id,
                       redcap_uri,
                       redcap_token,
                       redcap_username) {
  # Get user privileges from redcap API, for some reason it always throws
  # a warning that isn't relevant. I think it's something on REDcapR's end.
  user_info <-
    suppressWarnings(
      REDCapR::redcap_users_export(redcap_uri, redcap_token)[["data_user"]]
    )

  # Extract Data Export privilege for specified username
  user_privileges <- user_info[
    user_info["username"] == redcap_username,
    "data_export"
  ][[1L]]

  # Key: 0=No Access; 1=Full Dataset; 2=De-Identified; 3=Remove tagged fields
  if (user_privileges == "1") {
    stop("You have Full Data Set privileges, which will export identifying information.
  Please change your Data Export Privileges to De-Identified or Remove all tagged Identifier Fields.")
  }


  tibble::as_tibble(
    RcppSimdJson::fparse(
      RCurl::postForm(
        uri = redcap_uri,
        token = redcap_token,
        content = "report",
        format = "json",
        report_id = rep_id,
        csvDelimiter = "",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        returnFormat = "json"
      )
    )
  )
}


#' Get Repeating Instruments
#'
#' Returns a vector with all the repeating instrument names.
#' Used when generating a new lookup table.
#'
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#'
#' @return A vector with repeating instrument names
#' @export
#'
get_repeating <- function(redcap_uri, redcap_token) {
  httr::POST(
    url = redcap_uri,body = list(
      token = redcap_token,
      content = "repeatingFormsEvents",
      format = "json",
      returnFormat = "json"), encode = "form"
  ) |>
    httr::content() |>
    vapply(function(x) x[['form_name']], "Char")
}
