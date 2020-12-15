#' Query Subjects Across Instruments by External Dataframe
#'
#' This function utilizes the output of extract_infos, which can then be
#' filtered according to some criteria, to filter the list of dataframes from
#' split_instruments. This lets us avoid having to utilize apply or map
#' functions to apply a particular filter like "Only the Project A patients
#' at baseline."
#'
#' @param instrument_db List of dataframes, the output from split_instruments
#' @param filter_specification Dataframe, the output from extract_infos which
#' can be filtered as desired
#' @param join_info Logical, defaults to True, should this function add the info
#' from the filter specification dataframe such as age and time since onset?
#' @param limit_cols Logical, assuming join_info is True, if this is False, then
#' include all additional columns from the filter specification. If this is True,
#' then include only the columns given by include_cols
#' @param include_cols Character Vector, vector of which columns to include.
#' Must be present in filter_specification.
#' @param record_id_col
#'
#' @return
#' @export
#'
#' @examples
query_subjects <- function(instrument_db,
                           filter_specification,
                           join_info = T,
                           limit_cols = F,
                           include_cols = NA,
                           record_id_col = 'record_id'){

  if(!'redcap_repeat_instrument' %in% colnames(filter_specification))
    stop('Column redcap_repeat_instrument not found in filter specification')

  # The key columns that we'll join by
  key_cols <- c(record_id_col,
                'redcap_repeat_instrument',
                'redcap_repeat_instance')

  # Check how much info to retain from the filter specification
  if(join_info){
    if(limit_cols){
      # If the user wants to only include particular columns,
      # they have to specify which ones, and they must exist.
      # key_cols always has to be included no matter what.
      if(anyNA(include_cols)) # anyNA(NA) is TRUE
        stop("Must provide columns to include, or one of the columns provided is NA")
      else if(!all(include_cols %in% colnames(filter_specification)))
        stop("Column(s) provided are not in the filter specification")
      else{
        keep_cols <- c(key_cols, include_cols) # Keep specified cols
      }
    } else{
      keep_cols <- colnames(filter_specification) # Keep all cols
    }
  } else{
    keep_cols <- key_cols # Keep no additional information
  }

  # Not all instruments in the db will be in the filter specification,
  # so we get the instrument names to join by, and the prefixes give
  # the named indices for the db
  spec_names <- unique(filter_specification$redcap_repeat_instrument)
  spec_prefixes <- unique(filter_specification$instrument_prefix)

  output <- purrr::map2(spec_names, spec_prefixes, function(x,y){
    this_inst <-
      dplyr::filter(filter_specification,
                    redcap_repeat_instrument == x)[keep_cols] %>%
      .verify_datatypes(instrument_db[[y]], key_cols)

    dplyr::inner_join(instrument_db[[y]], this_inst, by=key_cols)
  })

  # The map results in an unnamed list, so we add the names back in
  purrr::set_names(output, spec_prefixes)

}

#' Verify datatypes before joining info
#'
#' Helper for query_subjects(), the column datatypes for the dataset from
#' an api call and importing from a file can differ, which prevents the inner
#' join from working.
#'
#' @param filter_spec Filter specification
#' @param inst_data Data of one instrument
#' @param cols Columns to verify
#'
#' @return
#'
#' @examples
.verify_datatypes <- function(filter_spec,
                              inst_data,
                              cols){
  for(col in cols){
    type_data <- typeof(inst_data[[col]])
    type_spec <- typeof(filter_spec[[col]])
    if(type_data != type_spec){
      filter_spec[col] <- do.call(paste0('as.',type_data),
                                  args=list(this_inst[col]))
    }
  }
  filter_spec
}
