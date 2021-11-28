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
.trim_instrument <- function(inst,
                             lookup = anrlab_instruments,
                             record_id_col = "record_id",
                             verbose = TRUE) {
  # Get the name and prefix of the instrument
  inst_name <- inst[["redcap_repeat_instrument"]][1L]
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
    return(.get_nonrepeating_insts(inst, lookup, redcap_cols, verbose))
  }

  inst_info <- lookup[lookup[["instrument_name"]] == inst_name, ]
  inst_pref <- inst_info[["instrument_prefix"]]

  select_cols <- c(redcap_cols, inst_info[["fields"]][[1L]])
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

#' Process non repeating instruments
#'
#' Used to handle the demographic and medical info, mostly
#'
#' @param inst Dataframe of a single instrument, this will typically be the
#' output of filtering the report where redcap_repeating_instrument is NA
#' @param lookup Lookup table, passed from split_instruments
#' @param redcap_cols Redcap metadata column names, passed from split_instruments
#' @param verbose Logical, whether readr type conversion messsages will be printed
#'
#' @return List of tibbles for each non repeating instrument found in inst
.get_nonrepeating_insts <- function(inst, lookup, redcap_cols, verbose) {
  which_not_repeating <- !lookup[["is_repeating"]]
  non_repeating_prefixes <- lookup[which_not_repeating,][['instrument_prefix']]

  # Not all reports use all non-repeating instruments
  which_nr_in_report <- .has_instrument(inst, non_repeating_prefixes)
  non_repeating_insts <- split(lookup[which_not_repeating, ][which_nr_in_report,], ~instrument_name)

  # For each non repeating instrument, get the relevant columns
  # This results in multiple dataframes, so we return the list of inst data
  return(
    purrr::map(non_repeating_insts, function(x) {
      select_cols <- x[["fields"]][[1L]]
      select_cols <- c(redcap_cols, select_cols)
      nr_inst <- dplyr::select(inst, tidyselect::contains(select_cols))
      nr_inst <- .postprocess(nr_inst, x[["instrument_prefix"]][[1L]], verbose)
      attr(nr_inst, "redcap_instrument") <- x[["instrument_prefix"]]
      nr_inst
    })
  )
}

#' Check if instrument in report df
#'
#' Given a character vector of instrument prefixes (typically non-repeating
#' instruments like demo and med), return whether a variable from those instruments
#' is included. This is equivalent to something like
#' grepl(colnames(inst), "^demo_") but will return as soon as it finds one. So,
#' best case O(3) worst case O(n)
#'
#' @param inst Dataframe of a single instrument, this will typically be the
#' output of filtering the report where redcap_repeating_instrument is NA
#' @param check_prefixes Which prefixes to check, typically c('demo','med')
#' @param USE.NAMES Whether to return prefixes as names in the logical vector,
#' defaults to FALSE
#'
#' @return Logical vector, TRUE if prefix is found in the colnames of the instrument
#' df, FALSE if not
.has_instrument <- function(inst, check_prefixes, USE.NAMES=FALSE) {
  vapply(check_prefixes,
         function(check_prefix) {
           lookup_regex <- paste0("^",check_prefix,"_")
           for (column_name in colnames(inst)) {
             if (grepl(lookup_regex, column_name))
               return(TRUE)
           }
           return(FALSE)
         },
         TRUE,
         USE.NAMES = USE.NAMES)
}
