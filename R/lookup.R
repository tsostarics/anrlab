
#' Generate Instrument Name-Prefix Lookup Table
#'
#' Must have API privileges and be connected to the Northwestern VPN. Creates
#' a lookup table based on the RedcapR::redcap_metadata_read() function. Note:
#' I need to include an offline object within the package for people to use.
#'
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#' @param ignore Any prefixes to ignore, really just here to ignore 'record_id'
#' in the list of prefixes.
#'
#' @return A dataframe with all the instrument names and their prefixes
#' @export
#'
#' @examples TODO
generate_lookup <- function(redcap_uri, redcap_token, ignore='record'){
  # Get data dictionary
  metas <- REDCapR::redcap_metadata_read(redcap_uri, redcap_token)
  # Get instrument names
  redcap_repeat_instrument <- unique(metas$data$form_name)
  # Extract prefixes for each instrument
  instrument_prefix <- unique(stringr::str_extract(metas$data$field_name,
                                                   "^[[:alnum:]]+"))
  instrument_prefix <- instrument_prefix[instrument_prefix != ignore]

  # Combine instrument info into lookup table
  inst_lookup <- data.frame(redcap_repeat_instrument, instrument_prefix)
  inst_lookup
}

