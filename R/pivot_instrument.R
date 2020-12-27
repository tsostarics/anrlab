#' Pivot instrument
#'
#' Helper for trim_instrument, handles the actual pivot across particular
#' columns. NOTE: The instrument_regex parameter is what our lab uses across
#' most instruments. If this needs to be changed later or generalizable for
#' other labs, then it's there. For now things work for us just fine.
#'
#' @param instrument One instrument's data
#' @param instrument_prefix Instrument prefix
#' @param instrument_regex The regex associated with the metadata in the field
#' name
#' @param output_names Output names for the capture groups of the regex
#' @param ... Other options for pivot_longer()
#'
#' @return A pivoted instrument
#'
#' @examples
#' #TODO
.pivot_instrument <- function(instrument,
                              instrument_prefix,
                              instrument_regex = "_(.+)_(.+)_(.+)$",
                              output_names = c("subtest", "number", ".value"),
                              ...) {
  inst_pattern <- paste0(instrument_prefix, instrument_regex)
  instrument <- instrument %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches(inst_pattern),
      names_to = output_names,
      names_pattern = inst_pattern,
      ...
    )

  # Set an attribute with the prefix so that we can reference it later
  # when we set the name of each instrument in the list
  attr(instrument, "redcap_instrument") <- instrument_prefix
  instrument
}
