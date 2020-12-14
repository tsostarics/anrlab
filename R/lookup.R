
#' Generate Instrument Name-Prefix Lookup Table
#'
#' Must have API privileges and be connected to the Northwestern VPN. Creates
#' a lookup table based on the RedcapR::redcap_metadata_read() function. Note:
#' I need to include an offline object within the package for people to use.
#'
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#'
#' @return
#' @export
#'
#' @examples
generate_lookup <- function(redcap_uri, redcap_token){
  # Get data dictionary
  metas <- REDCapR::redcap_metadata_read(redcap_uri, redcap_token)
  # Get instrument names
  redcap_repeat_instrument <- unique(metas$data$form_name)
  # Extract prefixes for each instrument
  instrument_prefix <- unique(str_extract(metas$data$field_name,
                                          "^[[:alnum:]]+"))
  # Combine instrument info into lookup table
  inst_lookup <- data.frame(redcap_repeat_instrument, instrument_prefix)
  inst_lookup
}
