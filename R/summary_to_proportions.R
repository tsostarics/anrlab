#' Convert Summary Scores to Proportions
#'
#' Summary score instruments provide the raw # of items correct depending on
#' the section and item type. This function will convert those into a percentage
#' correct, utilizing instrument-specific helpers to determine what the
#' denominator should be for each score (eg sometimes out of 5, sometimes out
#' of 74).
#'
#' @param inst_data Instrument data, must be a summary instrument
#' @param inst_prefix Instrument prefix
#'
#' @return The instrument but with raw # correct converted to the appropriate
#' % correct
#' @export
#'
#' @examples TODO
summary_to_proportions <- function(inst_data, inst_prefix){
  # Convert summary scores to proportion correct (varies depending on section)

  fx_name <- paste0('.',inst_prefix,'_to_proportions')
  if(methods::existsFunction(fx_name)){
    do.call(fx_name, args = list(inst_data))
  } else{
    return(inst_data)
  }
}

.nnbsbtl_to_proportions <- function(inst_data){
}

.nnbtotal_to_proportions <- function(inst_data){
}
