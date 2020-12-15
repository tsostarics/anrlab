#' Prettify instrument
#'
#' Renames columns and applies labels as needed to make the data easier to read.
#' This is not called automatically like .preprocess() and .postprocess(), and
#' so is done on an as-needed basis.
#'
#' @param inst_data Instrument data
#' @param inst_prefix Instrument prefix
#'
#' @return
#' @export
#'
#' @examples
prettify_instrument <- function(inst_data, inst_prefix){
  fx_name <- paste0('.prettify_',inst_prefix)
  if(existsFunction(fx_name)){
    do.call(fx_name, args = list(inst_data))
  } else{
    return(inst_data)
  }
}

.prettify_nnbsbtl <- function(inst_data){
  # Rename columns for nnb subtotal column
}
