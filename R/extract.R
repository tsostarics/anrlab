#' Extract Completion Status
#'
#' Extract the completion status for each participant and each instrument in
#' the given raw report data. This isn't terribly useful, but could be helpful
#' when trying to exclude any data entry that isn't finished.
#'
#' @param report_data Raw report data from get_report or importing from .csv
#' @param record_id_col column name for the record id, defaults to the lab's record_id
#'
#' @return A tibble containing the completion status value for each instrument
#' for every participant in the given report
#' @export
#' @examples TODO
extract_completes <- function(report_data, record_id_col='record_id'){
  complete_regex <- paste0(record_id_col, "|(_instance|_complete$)")
  complete_vars <- report_data[grep(complete_regex, colnames(report_data))]

  dplyr::filter(tidyr::pivot_longer(complete_vars,
                                    cols = contains('_complete'),
                                    names_to = 'instrument_complete'),
                !is.na(value))
}



#' Extract Info Variables from Instrument
#'
#' Extract the information from the top of each instrument such as age & tsonset.
#' This is valuable to filter which records you actually want, then you
#' can filter each instrument via joining operations
#'
#' @param report_data Dataframe, the raw report data from get_report or importing from .csv
#' @param lookup Dataframe, instrument Name-Prefix lookup table from generate_table or read in from a .csv
#' @param make_uid Logical, should a unique id from the record id, instrument, and instance number be made?
#' @param record_id_col String, name of the record id column, defaults to record_id
#' @param filter_by String, name of the column to filter out NAs, defaults to user
#'
#' @return A dataframe with all of the "info" data from each instrument
#' @export
#' @examples TODO
extract_info <- function(report_data,
                         lookup,
                         make_uid = T,
                         record_id_col = 'record_id',
                         filter_by = 'user'){
  # get only the record id, instance number, and info columns
  info_regex <- paste0(record_id_col, "|(_instance$)|(_info_)")
  info_cols <- colnames(report_data)[grep(info_regex, colnames(report_data))]
  info_vars <- report_data[info_cols]

  # shorten column names for readability
  info_cols <- gsub('_info_','_',info_cols)
  colnames(info_vars) <- info_cols

  # pivot the instrument information. There will be a lot of NAs so we filter
  # out any that don't have age information since it's an automatic calculation
  # that's only computed when that instrument is added to a record
  pivot_cols <- info_cols[grep(info_regex, info_cols, invert = T)]
  output <-
    info_vars %>%
    tidyr::pivot_longer(cols = all_of(pivot_cols),
                        names_to = c('instrument_prefix','.value'),
                        names_pattern = "(.+)_(.+)$") %>%
    dplyr::filter(!is.na(!!dplyr::sym(filter_by)), !!dplyr::sym(filter_by) != '')


  if(NA %in% output$date){
    warning("There are missing dates, please verify dates are entered in redcap.")
  }

  output <- dplyr::inner_join(output, lookup, by='instrument_prefix')
  if(make_uid){
    output <- dplyr::mutate(output,
                            uid = paste(!!as.name(record_id_col),
                                        redcap_repeat_instance,
                                        instrument_prefix,
                                        sep="_"),
                            .before = 1)
  }
  if(is.character(output$age)){
    output <- dplyr::mutate(output,
                            age = as.numeric(age),
                            tsonset = as.numeric(age),
                            date = as.Date(date))
  }

  output
}
