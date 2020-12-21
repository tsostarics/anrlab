#' Add Demographics Information to Info Table
#'
#' This is a wrapper for left_join to make it a bit easier for people without
#' knowledge of SQL-esque joins to simply take information from the demographics
#' table (such as subject ID or aphasia status) and add it to the info table.
#'
#' @param extracted_info Output of extract_info
#' @param demographics Demographics table from split_instruments
#' @param include_cols Which columns to add to the info table
#' @param record_id_col
#'
#' @return The extracted_info dataframe with the columns from demographics
#' specified in include_cols
#' @export
#'
#' @examples
#' # TODO
add_from_demographics <- function(extracted_info,
                                  demographics,
                                  include_cols,
                                  record_id_col = "demo_record_id") {
  dplyr::left_join(extracted_info,
    select(demographics, all_of(c(include_cols, record_id_col))),
    by = record_id_col
  )
}
