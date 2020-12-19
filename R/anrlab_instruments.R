#' Instrument Lookup Table
#'
#' Instrument information for our database for use with various functions
#'
#' @format A data frame with 18 rows and 4 variables:
#' \describe{
#'   \item{instrument_name}{Name of the instrument}
#'   \item{instrument_prefix}{Instrument prefix in variable names}
#'   \item{fields}{list of variables/field names for this instrument}
#'   \item{is_repeating}{True if it's a repeating instrument, else False}
#' }
"anrlab_instruments"
