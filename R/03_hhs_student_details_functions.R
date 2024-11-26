# Clean student details functions ---------------------------

## Clean general student details
#' Get clean general student details.
#'
#' Returns clean general student details for chosen academic year.
#' @param academicYear academic year as integer.
#' @examples
#' hhs_student_details_general(2021)
#' @export
hhs_student_details_general <- function(academicYear) {
  ## Message
  message(cat(crayon::cyan("Generating clean student details")))

  ## Import data
  df_students <- g4sr::gfs_student_details(academicYear = academicYear)
  df_students_general <- g4sr::gfs_student_general(academicYear = academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear = academicYear)
  df_students_sensitive <- g4sr::gfs_student_sensitive(academicYear = academicYear)

  message(cat(crayon::silver("Tidy datasets")))

  ## Tidy general
  df_students_general_02 <- dplyr::select(df_students_general, c(student_id, name, value))
  df_students_general_02 <- dplyr::filter(
    df_students_general_02,
    grepl(pattern = "admission|uci|age|hml|sen|key|permission",
          x = name, ignore.case = TRUE)
  )
  df_students_general_02 <- tidyr::pivot_wider(df_students_general_02, names_from = name, values_from = value)
  df_students_general_02 <- data.frame(df_students_general_02, check.names = TRUE)
  df_students_general_02 <- dplyr::select(df_students_general_02,c(
    student_id, "Ad.No" = Admission.number, UCI, HML.Band,
    #"Age.Reading" = X1..Reading.Age, "Age.Spelling" = X2..Spelling.Age,
    "SEN" = X3...SEN.Code, "SEN.Notes" = X4..SEN.Notes,
    "Keyworker" = X5..Keyworker.Name,
    "Date.Admission" = Admission.date
  ))
  df_students_general_02 <- dplyr::distinct(df_students_general_02)

  ## Tidy sensitive
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive, c(student_id, name, value))
  df_students_sensitive_02 <- tidyr::pivot_wider(df_students_sensitive_02, names_from = name, values_from = value)
  df_students_sensitive_02 <- data.frame(df_students_sensitive_02, check.names = TRUE)
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive_02, c(
    student_id, "LAC" = Looked.after, Ethnicity, EAL, FSM, "FSM6" = FSMEver6,
    "PP" = Pupil.Premium.Indicator
  ))
  df_students_sensitive_02 <- dplyr::distinct(df_students_sensitive_02)

  ## Tidy details
  df_students_details_02 <- dplyr::select(df_students_details, c(student_id, upn, national_curriculum_year, registration_group))

  message(cat(crayon::silver("Merge datasets")))

  ## Merge datasets
  df <- dplyr::left_join(df_students, df_students_details_02, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_students_general_02, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_students_sensitive_02, by = c("id" = "student_id"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$id, ")")
  df <- dplyr::mutate(df, WBr.PP = ifelse(grepl(pattern = "english|scottish|welsh", x = Ethnicity, ignore.case = TRUE) & PP == "True", "True", "False"))

  message(cat(crayon::silver("Clean final output")))

  ## Clean and filter
  df <- dplyr::select(df, c(
    "Year.Group" = national_curriculum_year, "UPN" = upn, "GFSID" = id,
    Surname.Forename.Reg, Surname.Forename.ID,
    "Surname" = preferred_last_name, "Forename" = preferred_first_name, "Reg" = registration_group,
    "Gender" = sex, Ethnicity, EAL, FSM, FSM6, PP, WBr.PP, HML.Band, Ad.No, UCI,
    #Age.Reading, Age.Spelling,
    SEN, SEN.Notes, Keyworker, LAC, Date.Admission
  ))
  df <- dplyr::mutate_all(df, .funs = as.character)
  # df <- dplyr::mutate_at(df, .vars = c("Date.Admission", "Date.Leaving"), .funs = lubridate::mdy_hms)
  # df$Stay <- lubridate::as.duration(df$Date.Leaving - df$Date.Admission)
  df <- dplyr::distinct(df)

  message(cat(crayon::silver("Impute missing data")))

  ## Impute missing data
  df$HML.Band <- ifelse(is.na(df$HML.Band), "Unknown.HML", df$HML.Band)
  df$Ethnicity <- ifelse(is.na(df$Ethnicity), "Unknown.Ethnicity", df$Ethnicity)
  df$EAL <- ifelse(is.na(df$EAL), "False", df$EAL)

  ## Return
  return(df)
}

## Clean SEND notes search
#' Get clean SEND notes search.
#'
#' Returns clean student details based on SEND notes search for chosen
#' academic year.
#' @param academicYear academic year as integer.
#' @param academicYear notesSearch as string.
#' @examples
#' hhs_student_send_search(2021, "extra time")
#' hhs_student_send_search(2021, "irlen")
#' @export
hhs_student_send_search <- function(academicYear, notesSearch) {
  ## Import
  df <- hhs_student_details_general(academicYear = academicYear)

  ## Search
  message(cat(crayon::silver("Search SEND notes for", notesSearch)))
  df <- dplyr::filter(df, grepl(pattern = notesSearch, x = SEN.Notes, ignore.case = TRUE))

  message(cat(crayon::silver("Clean final output")))

  ## Clean and filter
  df <- dplyr::select(df, c(
    Year.Group, UPN, GFSID, Surname.Forename.Reg,
    Surname.Forename.ID, Gender, EAL, FSM, FSM6, PP, WBr.PP,
    HML.Band, UCI,
    #Age.Reading, Age.Spelling,
    SEN, SEN.Notes, Keyworker, LAC, CP.CAF
  ))
  df <- dplyr::arrange(df, as.numeric(Year.Group))

  ## Return
  return(df)
}

## Raw general student details
#' Get raw general student details.
#'
#' Returns raw general student details for chosen academic year.
#' @param academicYear academic year as integer.
#' @examples
#' hhs_student_details_general(2021)
#' @export
hhs_raw_student_details_general <- function(academicYear) {
  ## Message
  message(cat(crayon::cyan("Requesting raw student details")))

  ## Import data
  df_students_general <- g4sr::gfs_student_general(academicYear = academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear = academicYear)

  message(cat(crayon::silver("Tidy datasets")))

  ## Tidy general
  df_students_general <- dplyr::select_if(df_students_general, function(x) !(all(is.na(x)) | all(x == "")))
  df_students_general <- dplyr::select(df_students_general, c(student_id, name, value, is_system))

  ## Tidy details
  df_students_details <- dplyr::select(df_students_details, c(student_id, upn))

  message(cat(crayon::silver("Merge datasets")))

  ## Merge datasets
  df <- dplyr::left_join(df_students_details, df_students_general, by = c("student_id" = "student_id"))

  message(cat(crayon::silver("Clean final output")))

  ## Clean and filter
  df <- dplyr::select(df, c(
    "UPN" = upn, "GFSID" = student_id,
    "Key" = name, "Value" = value
  ))

  ## Return
  return(df)
}
