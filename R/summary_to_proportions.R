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
#' @examples
#' # TODO
summary_to_proportions <- function(inst_data, inst_prefix) {
  # Convert summary scores to proportion correct (varies depending on section)

  fx_name <- paste0(".", inst_prefix, "_to_proportions")
  if (methods::existsFunction(fx_name)) {
    do.call(fx_name, args = list(inst_data))
  } else {
    return(inst_data)
  }
}

.nnbsbtl_to_proportions <- function(inst_data) {
  # AD /10
  # ALD / 10
  # CN /74
    # Animals, fruits/veg, tools, clothing all /8
    # other /10
    # total obj /42
    # body parts /8
    # total nouns /50
    # colors /8
    # intrans verbs /6
    # trans /10
    # total verbs /16
  # AC /50
    # Animal, fruits/veg, tools, clothing, other obj all /5
    # Total objs /25
    # Body parts /5
    # Total nouns /30
    # colors /5
    # Intrans verbs /5
    # Trans verbs /15
    # total verbs /15
  # SA /16
    # Animals and tools each /8
  # NW Rep /10
  # Word rep /21
}

.nnbtotal_to_proportions <- function(inst_data) {
}

.nat_to_proportions <- function(inst_data) {
  # All out of 5
  # Canonical: active, subject wh, subject relative (/15)
  # Non-canonical: passive, obj wh, obj relative (/15)
  # Total: /30
}
