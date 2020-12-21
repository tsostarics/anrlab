#' Process Narrative Metrics from Raw CHAT Output
#'
#' Takes the output from CHAT for a narrative sample and converts it into a form
#' that can be directly read into Redcap. This can be done through an API write
#' call or through uploading it manually.
#'
#' @param filepath Character, Filepath to a .csv or .xlsx file created by CHAT
#' @param record_id Character or Numeric, Record ID for this participant
#' @param instance Character or Numeric, Instance number for this participant's
#' narrative sample
#' @param researcher Character, Researcher NetID
#' @param date Character, Date of the sample in MM-DD-YYYY format
#' @param study Character, Which study is this sample for?
#' @param timepoint Character, Which timepoint of the study is this sample for?
#' @param record_id_col Defaults to record_id
#' @param inst_name Defaults to cinderella_narrative_summary
#'
#' @return A dataframe ready to be exported to redcap
#' @export
#'
#' @examples
#' TODO
process_cinderella_chat <- function(filepath,
                                    record_id = NA,
                                    instance = NA,
                                    researcher = NA,
                                    date = NA,
                                    study = NA,
                                    timepoint = NA,
                                    record_id_col = "demo_record_id",
                                    inst_name = "cinderella_narrative_summary") {

  # Error handling for missing parameters
  if (is.na(record_id)) stop("Error: Must provide a record_id")
  if (is.na(instance)) stop("Error: Must provide an instance number (an integer)")
  if (!is.na(date)) {
    if (grepl("\\d{2}-\\d{2}-\\d{4}")) {
      stop("Error: Date provided must be in MM-DD-YYYY format.")
    }
  }

  # Read in file, handle invalid file extensions
  extension <- stringr::str_extract(filepath, "\\.[[:alnum:]]+$")
  if (is.na(extension)) {
    stop("Error: Filepath has no file extension")
  } else if (extension == ".csv") {
    raw_chat <- read.csv(filepath)
  } else if (extension == ".xlsx") {
    raw_chat <- readxl::read_xlsx(filepath)
  } else {
    stop("Error: File must be a .csv or .xlsx file")
  }

  # Hard code the raw column names and the redcap column names
  raw_cols <-
    c(
      "File",
      "Language",
      "Corpus",
      "Code",
      "Duration (sec)",
      "Words/Min",
      "Total Utts",
      "Total Words",
      "MLU Words",
      "open-class",
      "% open-class/all words",
      "closed-class",
      "% closed-class/all words",
      "open/closed",
      "Nouns",
      "% Nouns/all words",
      "Verbs",
      "% Verbs/all words",
      "noun/verb",
      "adj|",
      "adv|",
      "det|",
      "pro|",
      "aux|",
      "conj|",
      "complementizers",
      "modals",
      "prep|",
      "negation markers",
      "infinitival markers",
      "quantifiers",
      "wh-words",
      "comparative suffixes",
      "superlative suffixes",
      "possessive markers",
      "regular plural markers",
      "irregular plural forms",
      "3rd person present tense markers",
      "regular past tense markers",
      "irregular past tense markers",
      "regular perfect aspect markers",
      "irregular perfect participles",
      "progressive aspect markers",
      "% correct regular verb inflection",
      "% correct irregular verb inflection",
      "% sentences produced",
      "% sentences with correct syntax, semantics*",
      "% sentences with flawed syntax",
      "% sentences with flawed semantics*",
      "sentence complexity ratio",
      "# embedded clauses/sentence"
    )
  fix_cols <-
    c(
      "narr_meta_path",
      "narr_meta_lang",
      "narr_meta_corpus",
      "narr_meta_code",
      "narr_stat_dur",
      "narr_stat_wpm_ratio",
      "nrr_stat_utts_count",
      "nrr_stat_word_count",
      "narr_stat_mluwords_ratio",
      "narr_stat_open_count",
      "narr_stat_open_pct",
      "narr_stat_closed_count",
      "narr_stat_closed_pct",
      "narr_stat_opcl_ratio",
      "narr_stat_noun_count",
      "narr_stat_noun_pct",
      "narr_stat_verb_count",
      "narr_stat_verb_pct",
      "narr_nv_ratio",
      "narr_stat_adj_count",
      "narr_stat_adv_count",
      "narr_stat_det_count",
      "narr_stat_pro_count",
      "narr_stat_aux_count",
      "narr_stat_conj_count",
      "narr_stat_comp_count",
      "narr_stat_modal_count",
      "narr_stat_prep_count",
      "narr_stat_negmrk_count",
      "narr_stat_infmrk_count",
      "narr_stat_quant_count",
      "narr_stat_wh_count",
      "narr_stat_compar_count",
      "narr_stat_super_count",
      "narr_stat_poss_count",
      "narr_stat_regpl_count",
      "narr_stat_irrpl_count",
      "narr_stat_3ppt_count",
      "narr_stat_regpt_count",
      "narr_stat_irrpt_count",
      "narr_stat_regperf_count",
      "narr_stat_irrperf_count",
      "narr_stat_prog_count",
      "narr_regv_pct",
      "narr_irrv_pct",
      "narr_stat_sent_pct",
      "narr_corsynsem_pct",
      "narr_flawsyn_pct",
      "narr_flawsem_pct",
      "narr_stat_sentcmplx_ratio",
      "narr_stat_embcl_ratio"
    )
  # Keep only the relevant fields
  output <- raw_chat[, raw_cols]

  # Rename fields to redcap's variable names
  colnames(output) <- fix_cols

  # Set the patient and meta data if provided (record_id and instance required)
  output[record_id_col] <- record_id
  output$redcap_repeat_instance <- instance
  output$redcap_repeat_instrument <- inst_name
  output$narr_info_user <- researcher
  output$narr_info_date <- date
  output$narr_info_study <- study
  output$narr_info_timepoint <- timepoint

  return(output)
}

#' Process Multiple Cinderella Narratives
#'
#' Given a dataframe with filepaths and metadata, use it to process multiple
#' narrative samples. This will convert all files into Redcap-readable form.
#'
#' @param pathdf A dataframe with the filepaths and relevant metadata
#' @param researcher Character, researcher NetID
#' @param record_id_col Defaults to record_id
#'
#' @return A dataframe with multiple narrative samples ready to be imported
#' into redcap
#' @export
#'
#' @examples
#' TODO
process_multi_cinderella <- function(pathdf,
                                     researcher = NA,
                                     record_id_col = "record_id") {
  if (colnames(pathdf) != c("filepath", "record_id", "instance")) {
    stop("Error: Malformed path data frame. Must have columns filepath, record_id, instance")
  }
  data.table::rbindlist(
    purrr::pmap(
      pathdf,
      function(filepath, record_id, instance) {
        process_cinderella_chat(filepath,
          record_id = record_id,
          instance = instance,
          researcher = researcher,
          record_id_col = record_id_col
        )
      }
    )
  )
}
