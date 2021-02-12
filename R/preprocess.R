
#' Preprocess data
#'
#' Our field names in Redcap have a lot of information in them, but some of them
#' don't need to be used by the researcher at all. This removes some redundant
#' information that's common to all instruments and also calls additional
#' instrument-specific functions as needed. For example, checkbox matrices
#' in redcap append ___X to the field name, but what X means will vary from
#' instrument to instrument.
#'
#' @param inst_data A single instrument's data
#' @param inst_prefix The prefix for this instrument
#' @param inst_name The name for this instrument
#'
#' @return A smaller dataframe with some columns renamed depending on the
#' instrument provided
#'
#' @examples
#' # TODO
.preprocess <- function(inst_data, inst_prefix, inst_name) {
  # Compose a function call of the form .preprocess_[prefix]()
  fx_name <- paste0(".preprocess_", inst_prefix)

  if (!"_summary" %in% inst_name) {
    inline_sums <- "_subtotals|_totals|_score|_scores|_analysis"
    inst_data <- inst_data[grep(inline_sums, colnames(inst_data), invert = T)]
  }
  unneeded_cols <- "(^consent)|(_complete$)|_info_|_override"
  inst_data <- inst_data[grep(unneeded_cols, colnames(inst_data), invert = T)]

  if (methods::existsFunction(fx_name)) {
    do.call(fx_name, args = list(inst_data))
  } else {
    return(inst_data)
  }
}

# .preprocess raw WAB data, rename checkbox columns and remove scores
.preprocess_wab <- function(inst_data) {
  # wab_obn_x_cues___1/2/3 should be Tactile, phonemic, Semantic
  # wab_ynq_x_acc___1/2/3/4/5 should be acc, verbal, gestural, eye blink, NR
  new_cols <- colnames(inst_data)

  rename_checkboxes <- function(column) {
    is_ynq_item <- grepl("wab_ynq_\\d+", column)
    is_obncue <- grepl("wab_obn_\\d+_cues", column)
    col_item <- stringr::str_extract(column, "wab_..._\\d+")
    last_num <- stringr::str_extract(column, ".$")
    if (is_ynq_item) {
      dplyr::case_when(
        last_num == "1" ~ paste0(col_item, "_acc"),
        last_num == "2" ~ paste0(col_item, "_verbal"),
        last_num == "3" ~ paste0(col_item, "_gestural"),
        last_num == "4" ~ paste0(col_item, "_eyeblink"),
        last_num == "5" ~ paste0(col_item, "_noresponse"),
      )
    } else if (is_obncue) {
      dplyr::case_when(
        last_num == "1" ~ paste0(col_item, "_tactile"),
        last_num == "2" ~ paste0(col_item, "_phonemic"),
        last_num == "3" ~ paste0(col_item, "_semantic")
      )
    } else {
      column
    }
  }
  # Rename checkbox columns
  colnames(inst_data) <- sapply(new_cols, rename_checkboxes, USE.NAMES = F)

  # Omit wf_resp and any scores (they're entered by the experimenter but
  # really only used in the wab summary scores instrument)
  item_cols <- grep("wf_resp", colnames(inst_data), invert = T)
  inst_data[item_cols]
}

# .preprocess raw NAVI data, rename checkbox columns
.preprocess_navi <- function(inst_data) {
  # navi_main_x_err___1/2/3 should be substitution, ommission, other
  new_cols <- colnames(inst_data)
  # Convert Inf (numeric) to 'Inf' (character) due to a mistaken readr guess
  inst_data <-
    dplyr::mutate(
      inst_data,
      dplyr::across(
        tidyselect::contains("_type") & where(is.numeric),
        as.character
      )
    )

  rename_checkboxes <- function(column) {
    if (grepl("_err", column)) {
      col_item <- stringr::str_extract(column, "navi_main_\\d+")
      last_num <- stringr::str_extract(column, ".$")
      dplyr::case_when(
        last_num == "1" ~ paste0(col_item, "_substitution"),
        last_num == "2" ~ paste0(col_item, "_ommission"),
        last_num == "3" ~ paste0(col_item, "_other")
      )
    } else {
      column
    }
  }
  # Rename checkbox columns
  colnames(inst_data) <- sapply(new_cols, rename_checkboxes, USE.NAMES = F)
  inst_data
}

.preprocess_navipre <- function(inst_data) {
  # navipre_npcomp_x_acc___1/2 should be match, read
  new_cols <- colnames(inst_data)

  rename_checkboxes <- function(column) {
    if (grepl("___", column)) {
      col_item <- stringr::str_extract(column, "navipre_[[:alnum:]]+_\\d+")
      last_num <- stringr::str_extract(column, "\\d+$")
      dplyr::case_when(
        last_num == "1" ~ paste0(col_item, "_match"),
        last_num == "2" ~ paste0(col_item, "_read")
      )
    } else {
      column
    }
  }
  # Rename checkbox columns
  colnames(inst_data) <- sapply(new_cols, rename_checkboxes, USE.NAMES = F)
  inst_data
}

# .preprocess nnb error analysis since it's entirely a checkbox matrix
# note that the type of the word will end up in the 'subtest' column
# this may change later if i figure out a good way to maintain a list
# of field regexes and output columns, but it's okay for now
.preprocess_nnberr <- function(inst_data) {
  # nnberr_s3_x_type___1/2/3/4/5/6/7/8/9/10/11/12 should be:
  # SR, SU, Pn, Pr, N, WC, ESR, Paragrammatic, EM, Pe, Un, DK
  new_cols <- colnames(inst_data)

  rename_checkboxes <- function(column) {
    if (grepl("___", column)) {
      col_num <- stringr::str_extract(column, "_(\\d+)")
      col_type <- stringr::str_extract(column, "_(noun|verb)")
      col_item <- paste0("nnberr", col_type, col_num)
      last_num <- stringr::str_extract(column, "\\d+$")
      dplyr::case_when(
        last_num == "1" ~ paste0(col_item, "_SR"),
        last_num == "2" ~ paste0(col_item, "_SU"),
        last_num == "3" ~ paste0(col_item, "_Pn"),
        last_num == "4" ~ paste0(col_item, "_Pr"),
        last_num == "5" ~ paste0(col_item, "_N"),
        last_num == "6" ~ paste0(col_item, "_WC"),
        last_num == "7" ~ paste0(col_item, "_ESR"),
        last_num == "8" ~ paste0(col_item, "_Paragrammatic"),
        last_num == "9" ~ paste0(col_item, "_EM"),
        last_num == "10" ~ paste0(col_item, "_Pe"),
        last_num == "11" ~ paste0(col_item, "_Un"),
        last_num == "12" ~ paste0(col_item, "_DK")
      )
    } else {
      column
    }
  }
  # Rename checkbox columns
  colnames(inst_data) <- sapply(new_cols, rename_checkboxes, USE.NAMES = F)
  inst_data
}

.preprocess_navs <- function(inst_data) {
  # wab_obn_x_cues___1/2/3 should be Tactile, phonemic, Semantic
  # wab_ynq_x_acc___1/2/3/4/5 should be acc, verbal, gestural, eye blink, NR
  new_cols <- colnames(inst_data)

  rename_checkboxes <- function(column) {
    if (grepl("___", column)){
      last_num <- stringr::str_extract(column, ".$")
      if (grepl("args_", column))
        dplyr::case_when(
          last_num == "1" ~ gsub("___.$", "_x", column),
          last_num == "2" ~ gsub("___.$", "_V", column),
          last_num == "3" ~ gsub("___.$", "_y", column),
          last_num == "4" ~ gsub("___.$", "_z", column),
        )
      else
        dplyr::case_when(
          last_num == "1" ~ gsub("___.$", "_A", column),
          last_num == "2" ~ gsub("___.$", "_W", column)
        )
    }
    else{
      column
    }
  }
  # Rename checkbox columns
  colnames(inst_data) <- sapply(new_cols, rename_checkboxes, USE.NAMES = F)
  inst_data
}
