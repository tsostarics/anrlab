#' Export to SQLite Database
#'
#' Add a list of instrument dataframes from split_instruments() to an RSQLite
#' database, if you want to use SQL commands that way. This is really just a
#' wrapper for lapply to add all the instruments at once.
#'
#' @param conn A database connection from RSQLite::dbConnect()
#' @param instrument_list A list of instruments, either from split_instruments()
#' or query_subjects() (but you probably want the full data from the former)
#' @param ... Other options to pass to dbWriteTable
#'
#' @return Nothing, will display a message if successful
#' @export
#'
#' @examples
#'
#' # Set up instruments and database
#' # all_instruments <- split_instruments(report, lookup)
#' # db_con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Send all instruments to database connection
#' # export_sqlite(db_con, all_instruments)
#'
#' # Run a simple query
#' # RSQLite::dbGetQuery(dbcon, "SELECT
#' #                              wab.*,
#' #                              wabsum.aq
#' #                            FROM wab
#' #                            LEFT JOIN wabsum
#' #                            USING (record_id, redcap_repeat_instance)")
export_sqlite <- function(conn, instrument_list, ...) {
  results <-
    vapply(
      report_db,
      function(x) {
        RSQLite::dbWriteTable(
          conn = conn,
          name = attributes(x)$redcap_instrument,
          value = x,
          ...
        )
      },
      TRUE
    )
  if (!all(results)) {
    warning("There was an issue sending one or more instruments to the database, please review.")
    results
  } else {
    message("All instruments were added to database successfully.")
  }
}
