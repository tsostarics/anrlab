#' Export instruments to excel workbook
#'
#' Export a list of instruments (the output of `split_instruments` or
#' `query_instruments`) to an excel workbook. Each instrument is saved in a
#' separate sheet.
#'
#' Be VERY CAREFUL with dates in Excel.
#'
#' This function requires the xlsx package to be installed.
#'
#' @param instrument_db List of instruments
#' @param filepath Filepath for a new .xlsx file. Must be .xlsx.
#'
#' @return Returns nothing, saves an excel workbook
#' @export
export_to_excel <- function(instrument_db, filepath) {
  if (!grepl("\\.xlsx$", filepath))
    stop("Filepath must end in .xlsx")

  requireNamespace('xlsx', quietly = TRUE)
  workbook <- xlsx::createWorkbook()

  for (inst_name in names(instrument_db)) {
    cur_sheet <- xlsx::createSheet(workbook, sheetName = inst_name)
    xlsx::addDataFrame(instrument_db[[inst_name]], cur_sheet)
  }

  warning("Sometimes datatypes, ESPECIALLY DATES, can be changed by Excel. Please be careful and double check things.")
  xlsx::saveWorkbook(workbook, filepath)
}
