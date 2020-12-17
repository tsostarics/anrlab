#' Postprocess instrument data
#'
#' After pivoting an instrument, some postprocessing needs to be done. Usually
#' this means converting datatypes from character to numeric.
#'
#' @param inst_data One instrument's data
#' @param inst_prefix Instrument's prefix
#'
#' @return A dataframe with some cleaned up names and datatypes
#'
#' @examples TODO
.postprocess <- function(inst_data, inst_prefix, verbose=T){
  # Common .postprocessing that should be done on all instruments

  # Change item number and accuracy scores to integers
  if('number' %in% colnames(inst_data))
    inst_data$number <- as.integer(inst_data$number)
  if('acc' %in% colnames(inst_data))
    inst_data$acc <- as.integer(inst_data$acc)

  # Remove instrument prefix from column names for readability
  colnames(inst_data) <- gsub(paste0(inst_prefix,'_'),
                              '',
                              colnames(inst_data))

  # Turns out readr can operate on existing data with type_convert so it can
  # generalize a whole lot better than hard coding everything
  if(verbose)
    readr::type_convert(inst_data)
  else
    suppressMessages(readr::type_convert(inst_data))
}
