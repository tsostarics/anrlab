
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
generate_new_lookup <- function(redcap_uri, redcap_token, ignore='record'){
  # Get data dictionary
  inst_lookup <-
    dplyr::select(
      REDCapR::redcap_metadata_read(redcap_uri, redcap_token)$data,
      instrument_name = form_name,
      field_name
    )

  repeating_insts <- get_repeating(uri, tkn)$form_name


  inst_lookup <-
    dplyr::group_by(inst_lookup, instrument_name) %>%
    dplyr::summarize(instrument_prefix = stringr::str_extract(dplyr::last(field_name), "^[[:alnum:]]+"),
                     fields = list(field_name))

  inst_lookup$is_repeating <- inst_lookup$instrument_name %in% repeating_insts
  inst_lookup
}

