#' Get Redcap Report
#'
#' @param rep_id Report ID on Redcap
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#'
#' @return A tibble representation of the specified redcap report
#' @export
#'
#' @examples
get_report <- function(rep_id, redcap_uri, redcap_token){
  tibble::as_tibble(
    RcppSimdJson::fparse(
      postForm(
        uri=redcap_uri,
        token=redcap_token,
        content='report',
        format='json',
        report_id=rep_id,
        csvDelimiter='',
        rawOrLabel='raw',
        rawOrLabelHeaders='raw',
        exportCheckboxLabel='false',
        returnFormat='json'
      )
    )
  )
}
