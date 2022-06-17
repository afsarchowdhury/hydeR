# Clean readability functions ---------------------------

## Clean readability data
#' Get clean readability data.
#'
#' Returns clean readability data for texts in a given directory.
#' @param text_dir path to directory containing texts as string.
#' @param return_full return full readability statistics as logical.  Default FALSE.
#' @examples
#' hhs_readability("C:/Users//User/Texts", TRUE)
#' @export
hhs_readability <- function(text_dir, return_full = FALSE) {
  ## Check directory
  message(cat(crayon::cyan("Check directory status")))

  if (dir.exists(text_dir) == FALSE) {
    stop(call. = FALSE, "Directory does not exist")
  } else{
    ## Create corpus
    message(cat(crayon::silver("Create corpus")))

    my_data <- readtext::readtext(paste0(text_dir, "*"), encoding = "ISO-8859-1", verbosity = 3)
    my_corpus <- quanteda::corpus(my_data)

    ## Create docvars df
    df_my_corpus <- summary(my_corpus, n = length(my_corpus))

    ## Compute statistics
    message(cat(crayon::silver("Computing text statistics")))

    df_numberline <- data.frame(Flesch = seq(100, 0, -1))

    df_readability_details <- data.frame(
      Flesch = c(90, 80, 70, 60, 50, 30, 10, 0),
      Age = c("10-11", "11-12", "12-13", "13-15", "15-18", "Undergraduate", "Graduate", "Professional"),
      Notes = c("Very easy to read. Easily understood by an average 11-year-old student.",
                "Easy to read. Conversational English for consumers.",
                "Fairly easy to read.",
                "Plain English. Easily understood by 13- to 15-year-old students.",
                "Fairly difficult to read.",
                "Difficult to read.",
                "Very difficult to read. Best understood by university graduates.",
                "Extremely difficult to read. Best understood by university graduates.")
    )

    df_readability_details_long <- dplyr::left_join(df_numberline, df_readability_details, by = c("Flesch"))
    df_readability_details_long <- tidyr::fill(df_readability_details_long, c(Age, Notes), .direction = "up")

    rm(df_numberline)

    message(cat(crayon::silver("Calculate statistics")))

    df_stat_readability <- quanteda.textstats::textstat_readability(
      x = my_corpus,
      measure = c("meanSentenceLength","meanWordSyllables", "Flesch")
    )
    df_stat_readability$Flesch <- round(df_stat_readability$Flesch, 0)

    df_stat <- dplyr::left_join(df_my_corpus, df_stat_readability, by = c("Text" = "document"))
    df_stat <- dplyr::left_join(df_stat, df_readability_details_long, by = c("Flesch"))
    df_stat <- dplyr::mutate(df_stat, Words = round(Sentences * meanSentenceLength, 0))

    ## Clean df_stat
    message(cat(crayon::silver("Clean final output")))

    df_stat <- dplyr::rename(df_stat, Mean.WordsPS = meanSentenceLength, Mean.SyllablesPW = meanWordSyllables)
    df_stat <- dplyr::mutate(df_stat, across(.cols = c(Mean.WordsPS, Mean.SyllablesPW), .fns = ~round(., 1)))
    df_stat <- dplyr::select(df_stat, c(Text, Types, Tokens, Words, everything()))

    ## Final return
    if (return_full == FALSE) {
      df <- dplyr::select(df_stat, c(Text, Flesch, Age, Notes))
      return(df)
    } else {
      message(cat(crayon::silver("Return full output")))
      return(df_stat)
    }
  }
}
