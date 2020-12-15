#' Trim instrument
#'
#' Helper for split_instruments, actually does all of the hard work of paring
#' down each instrument's data and passing it off to the pre and post
#' processer helpers.
#'
#' @param inst Instrument data
#' @param lookup Instrument name-prefix lookup table from generate_lookup
#' @param record_id_col
#' @param verbose Logical, inherits from split_instrument, prints the name
#' of each instrument as it's processing it. Useful for debugging.
#'
#' @return
#' @examples
.trim_instrument <- function(inst,
                             lookup,
                             record_id_col='demo_record_id',
                             verbose = T){
  # print(inst)
  # Get the name and prefix of the instrument
  inst_name <- inst[[1L, 'redcap_repeat_instrument']]

  if(verbose) ifelse(is.na(inst_name)|inst_name=='',
                     print('non-repeat instruments'),
                     print(inst_name))

  # Demographics and medical history aren't repeated instruments
  # (name=='') so we can easily just return the relevant columns
  if(inst_name=="" | is.na(inst_name)){
    prefix_regex <- paste0('^(',record_id_col,'|demo|med)')
    inst <- inst[grep(prefix_regex,colnames(inst))]
    attr(inst, 'redcap_instrument') <- 'demographics'
    return(inst)
  }

  inst_pref <- lookup[lookup['redcap_repeat_instrument']==inst_name, 'instrument_prefix']

  # Get the columns corresponding to the instrument using the correct prefix
  prefix_regex <- paste0('^(',record_id_col,'|redcap_|',inst_pref,'_)')
  inst <- inst[grep(prefix_regex, colnames(inst))]

  # Preprocess data
  inst <- .preprocess(inst, inst_pref, inst_name)

  # If this is a summary score instrument then we don't need to pivot it
  if(grepl('_summary', inst_name)){
    attr(inst, 'redcap_instrument') <- inst_pref
    return(.postprocess(inst, inst_pref))
  }
  # All other instruments need to be converted to long form
  .postprocess(.pivot_instrument(inst, inst_pref), inst_pref)
}
