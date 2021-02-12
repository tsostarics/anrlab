#' Postprocess instrument data
#'
#' After pivoting an instrument, some postprocessing needs to be done. Usually
#' this means converting datatypes from character to numeric.
#'
#' @param inst_data One instrument's data
#' @param inst_prefix Character, instrument's prefix
#' @param verbose Logical, defaults to True, inherits from split_instruments
#'
#' @return A dataframe with some cleaned up names and datatypes
#'
#' @examples
#' #TODO
.postprocess <- function(inst_data, inst_prefix, verbose = T) {
  # Common .postprocessing that should be done on all instruments

  # Change item number and accuracy scores to integers
  if ("number" %in% colnames(inst_data)) {
    inst_data$number <- as.integer(inst_data$number)
  }
  if ("acc" %in% colnames(inst_data)) {
    inst_data$acc <- as.integer(inst_data$acc)
  }

  # Remove instrument prefix from column names for readability
  colnames(inst_data) <- gsub(
    paste0(inst_prefix, "_"),
    "",
    colnames(inst_data)
  )

  # Compose a function call of the form .postprocess_[prefix]()
  # this may need to come after the type conversion? maybe not though
  # TODO: actually implement this for the non repeatable instruments
  # fx_name <- paste0('.preprocess_',inst_prefix)
  # if(methods::existsFunction(fx_name)){
  #   inst_data <- do.call(fx_name, args = list(inst_data))
  # }

  # Turns out readr CAN operate on existing data with type_convert so it can
  # generalize a whole lot better than hard coding everything
  if (verbose) {
    readr::type_convert(inst_data)
  } else {
    suppressMessages(readr::type_convert(inst_data))
  }
}

.postprocess_navs <- function(inst_data) {
  dplyr::relocate(inst_data, z, .before = A)
}

.postprocess_demo <- function(inst_data) {
  # rename checkboxes
}

.postprocess_med <- function(inst_data) {
  # rename checkboxes
}

.postprocess_nat <- function(inst_data) {
  dplyr::select(inst_data, -subtest)
}
