#' Postprocess instrument data
#'
#' After pivoting an instrument, some postprocessing needs to be done. Usually
#' this means converting datatypes from character to numeric.
#'
#' @param inst_data One instrument's data
#' @param inst_prefix Instrument's prefix
#'
#' @return
#' @export
#'
#' @examples
.postprocess <- function(inst_data, inst_prefix){
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

  # Compose a function call of the form preprocess_[prefix]()
  fx_name <- paste0('.postprocess_',inst_prefix)
  if(existsFunction(fx_name)){
    do.call(fx_name, args = list(inst_data))
  } else{
    return(inst_data)
  }
}

.postprocess_wab <- function(inst_data){
  mutate(inst_data,
         across(verbal:phonemic, as.integer))
}

.postprocess_navi <- function(inst_data){
  mutate(inst_data,
         across(substitution:other, as.integer))
}

.postprocess_navipre <- function(inst_data){
  mutate(inst_data,
         across(match:read, as.integer))
}


.postprocess_navipresum <- function(inst_data){
  mutate(inst_data,
         across(phon_num:npcomp_rsi_num, as.numeric))
}

.postprocess_navisum <- function(inst_data){
  mutate(inst_data,
         across(inf_num:psi_num, as.numeric))
}

.postprocess_nnb <- function(inst_data){
  mutate(inst_data,
         across(av, as.integer))
}

.postprocess_nnberr <- function(inst_data){
  mutate(inst_data,
         across(SR:Un, as.integer))
}

.postprocess_nnbsbtl <- function(inst_data){
  mutate(inst_data,
         across(ad_na_1_na:wr_verb_1_v, as.numeric))
}

.postprocess_nnbtotal <- function(inst_data){
  mutate(inst_data,
         across(ad_all:ac_verbs, as.numeric))
}

.postprocess_wabsum <- function(inst_data){
  mutate(inst_data,
         across(ss_total:aq, as.numeric))
}
