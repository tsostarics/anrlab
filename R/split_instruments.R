#' Split Instruments from Raw Report
#'
#' Given a dataframe from a raw report, convert the dataframe into a list of
#' dataframes. Each dataframe is 1 Redcap instrument's worth of data in tidy
#' format. Instrument data can be accessed through $ notation such as
#' output$nnb to get the data from the Northwestern Naming Battery. The name
#' of each dataframe is set to be the instrument prefix to keep things short.
#'
#' @param report_data Dataframe, the raw exported data from a report
#' @param lookup Dataframe, instrument name-prefix lookup table.
#' @param record_id_col Defaults to record_id
#' @param verbose Logical, if True will print each instrument as it's processing
#'
#' @return List of dataframes, named according to instrument prefix
#' @export
split_instruments <- function(report_data,
                              lookup = anrlab_instruments,
                              record_id_col = "record_id",
                              verbose = T) {

  # Throw error if the repeat instrument isn't in the provided data
  if (!"redcap_repeat_instrument" %in% colnames(report_data)) {
    stop("Provided data frame must contain the redcap_repeat_instrument field")
  }
  if (!"redcap_repeat_instance" %in% colnames(report_data)) {
    stop("Provided data frame must contain the redcap_repeat_instance field")
  }

  # Group by instrument, split, then trim each instrument
  report_data <- dplyr::group_by(report_data, redcap_repeat_instrument)
  inst_data_list <- dplyr::group_split(report_data)
  inst_data_list <-
    rlang::squash(
      purrr::map(
        inst_data_list,
        function(x) .trim_instrument(x, lookup, record_id_col, verbose)
      )
    )

  # Extract names stored in the attribute of each instrument data frame
  inst_names <- vapply(
    inst_data_list,
    FUN = function(x) attributes(x)$redcap_instrument,
    FUN.VALUE = 'character'
  )

  # Return list of all the data for each instrument, named according to prefix
  purrr::set_names(inst_data_list, inst_names)
}
