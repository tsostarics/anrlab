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
#' @examples
#' TODO
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
  inst_data_list <- purrr::map(
    inst_data_list,
    function(x) {
      .trim_instrument(
        x,
        lookup,
        record_id_col,
        verbose
      )
    }
  )
  # Currently we have a structure of list(list(x,y),z) and i need list(x,y,z)
  # Even though this is using a for loop, I'm not growing or copying the main
  # list, I'm just adding pointers to the end of the list to data that already
  # exists, then removing any pointers that need to be removed.
  last_inst <- length(inst_data_list)
  to_remove <- c()
  for (index in 1:last_inst) {
    # print(paste0(index,": ", lobstr::obj_addr(inst_data_list[[index]])))

    if (class(inst_data_list[[index]])[1L] == "list") {
      # This is a list within the list, so we'll have to remove that later
      to_remove <- c(index, to_remove)
      for (sublist in inst_data_list[[index]]) {
        # Add a pointer to the end of the list
        last_inst <- last_inst + 1
        inst_data_list[[last_inst]] <- sublist
      }
    }
  }

  for (index in to_remove) {
    inst_data_list[[index]] <- NULL
  }

  # Extract names stored in the attribute of each instrument data frame
  inst_names <- sapply(
    inst_data_list,
    function(x) attributes(x)$redcap_instrument
  )

  # Return list of all the data for each instrument, named according to prefix
  purrr::set_names(inst_data_list, inst_names)
}
