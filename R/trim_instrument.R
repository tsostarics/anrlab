#' Trim instrument
#'
#' Helper for split_instruments, actually does all of the hard work of paring
#' down each instrument's data and passing it off to the pre and post
#' processer helpers.
#'
#' @param inst Instrument data
#' @param lookup Instrument name-prefix lookup table from generate_lookup
#' @param record_id_col Defaults to record_id
#' @param verbose Logical, inherits from split_instrument, prints the name
#' of each instrument as it's processing it. Useful for debugging.
#'
#' @return Smaller dataframe that's pivoted to long form and postprocessed
#' @examples
#' TODO
.trim_instrument <- function(inst,
                             lookup = anrlab_instruments,
                             record_id_col = "record_id",
                             verbose = T) {
  # Get the name and prefix of the instrument
  inst_name <- inst$redcap_repeat_instrument[1L]
  redcap_cols <- c(
    record_id_col,
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )

  if (verbose) {
    ifelse(is.na(inst_name) | inst_name == "",
      print("non-repeat instruments"),
      print(inst_name)
    )
  }

  # All non repeating lists are lumped together
  if (inst_name == "" | is.na(inst_name)) {
    # Access our list of non repeating instruments
    non_repeating_insts <-
      dplyr::group_by(lookup[!lookup$is_repeating, ], instrument_name) %>%
      dplyr::group_split()

    # For each non repeating instrument, get the relevant columns
    # This results in multiple dataframes, so we return the list of inst data
    return(
      purrr::map(non_repeating_insts, function(x) {
        select_cols <- x$fields[[1L]]
        select_cols <- c(redcap_cols, select_cols)
        nr_inst <- dplyr::select(inst, tidyselect::contains(select_cols))
        nr_inst <- .postprocess(nr_inst, x$instrument_prefix[[1L]], verbose)
        attr(nr_inst, "redcap_instrument") <- x[["instrument_prefix"]]
        nr_inst
      })
    )
  }

  inst_info <- lookup[lookup$instrument_name == inst_name, ]
  inst_pref <- inst_info[["instrument_prefix"]]

  select_cols <- c(redcap_cols, inst_info$fields[[1L]])
  inst <- dplyr::select(inst, c(redcap_cols, tidyselect::contains(select_cols)))
  attr(inst, "redcap_instrument") <- inst_pref

  # Revisit this later and find a nicer way to incorporate the consent forms
  # into the preprocessing step
  if (grepl("^consent", inst_name)) {
    return(inst)
  }

  # Preprocess data
  inst <- .preprocess(inst, inst_pref, inst_name)

  # If this is a summary score instrument then we don't need to pivot it
  if (grepl("_summary", inst_name)) {
    attr(inst, "redcap_instrument") <- inst_pref
    return(.postprocess(inst, inst_pref, verbose))
  }
  # All other instruments need to be converted to long form
  .postprocess(.pivot_instrument(inst, inst_pref), inst_pref, verbose)
}
