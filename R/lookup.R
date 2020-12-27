
#' Generate Instrument Name-Prefix Lookup Table
#'
#' Must have API privileges and be connected to the Northwestern VPN. This
#' will return a new version of the anrlab_instruments objects. Future
#' maintainers should run this and update that object whenever changes are made
#' to instruments or new instruments are added. This shouldn't happen frequently
#' once a project is moved to production mode.
#'
#' @param redcap_uri Redcap user API URI
#' @param redcap_token Redcap user API token
#'
#' @return A dataframe with all the instrument names and their prefixes
#' @export
#'
#' @examples
#' #TODO
generate_new_lookup <- function(redcap_uri, redcap_token) {
  # Get data dictionary
  inst_lookup <-
    dplyr::select(
      REDCapR::redcap_metadata_read(redcap_uri, redcap_token)$data,
      instrument_name = form_name,
      field_name
    )

  # Get instrument names, prefixes, and field names
  inst_lookup <-
    dplyr::group_by(inst_lookup, instrument_name) %>%
    dplyr::summarize(
      instrument_prefix =
        # Using last will ignore the record id
      stringr::str_extract(
        dplyr::last(field_name),
        "^[[:alnum:]]+"
      ),
      fields = list(field_name)
    )

  # Set which instruments are repeating instruments
  repeating_insts <- get_repeating(uri, tkn)$form_name
  inst_lookup$is_repeating <- inst_lookup$instrument_name %in% repeating_insts
  inst_lookup
}
