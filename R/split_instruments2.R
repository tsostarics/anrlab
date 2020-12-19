#' Title
#'
#' This is the new way I'm testing out to do it...
#'
#' @param report_data
#' @param lookup
#' @param record_id_col
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
split_instruments2 <- function(report_data,
                              lookup = acnrlab_instruments,
                              record_id_col = "record_id",
                              verbose = T){
  # instruments <-
  #   dplyr::group_by(report_data, redcap_repeat_instrument)


  lookup <- split(lookup, factor(lookup$instrument_name))
  # print(lookup)
  purrr::lmap(lookup, function(x){
    # print(x[[1]][['fields']])
    fields <- x[[1L]][['fields']][[1L]]
    # print(fields)
    fields <- fields[fields %in% colnames(report_data)]
    fields <- c('record_id',
                'redcap_repeat_instrument',
                'redcap_repeat_instance',
                fields)
    # print(fields)
    print(dplyr::select(report_data, tidyselect::all_of(fields)))
  })
}
