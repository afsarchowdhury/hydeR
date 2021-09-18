# Clean attendance functions ---------------------------

## Clean student session marks
#' Get clean student session marks.
#'
#' Returns clean details of student session marks for chosen
#' academic year, date, and session.
#' @param academicYear academic year as integer.
#' @param goDate date as string in the form yyyy-mm-dd.
#' @param session session as string.
#' @examples
#' hhs_attendance_student_session(2020, "2020-07-01")
#' hhs_attendance_student_session(2020, "2020-07-01", "am")
#' @export
hhs_attendance_student_session <- function(academicYear, goDate, session = NULL) {
  ## Message
  message(cat(crayon::cyan("Generating clean student session marks for", goDate)))

  ## Import data
  df_attendance_codes <- g4sr::gfs_attendance_codes(academicYear)
  df_attendance_student_session_marks <- g4sr::gfs_attendance_student_session_marks(academicYear, goDate)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_students, df_attendance_student_session_marks, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_attendance_codes, by = c("session_mark_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("id" = "student_id"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df <- dplyr::filter(df, !is.na(date))
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("UPN" = upn, "GFSID" = id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Year.Group" = national_curriculum_year, "Reg" = registration_group, "Gender" = sex,
                            "Date" = date, "Session" = session, "Session.Mark" = code, "Session.Label" = label,
                            "Session.Late.Minutes" = session_minutes_late, "Session.Notes" = session_notes))
  df <- dplyr::distinct(df)

  if (is.null(session)) {
    message(cat(crayon::magenta("No session preference provided.  Returning AM and PM.")))
    df <- df
  } else {
    df <- dplyr::filter(df, Session == toupper(session))
  }

  ## Return
  return(df)
}

## Clean student lesson marks
#' Get clean student lesson marks.
#'
#' Returns clean details of student lesson marks for chosen
#' academic year and date.
#' @param academicYear academic year as integer.
#' @param goDate date as string in the form yyyy-mm-dd.
#' @examples
#' hhs_attendance_student_lesson(2020, "2020-07-01")
#' @export
hhs_attendance_student_lesson <- function(academicYear, goDate) {
  ## Message
  message(cat(crayon::cyan("Generating clean student lesson marks for", goDate)))

  ## Import data
  df_attendance_codes <- g4sr::gfs_attendance_codes(academicYear)
  df_attendance_student_lesson_marks <- g4sr::gfs_attendance_student_lesson_marks(academicYear = academicYear, goDate)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_classes <- g4sr::gfs_classes(academicYear)
  df_timetables <- g4sr::gfs_timetables(academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_students, df_attendance_student_lesson_marks, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_attendance_codes, by = c("lesson_mark_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("id" = "student_id"))
  df_02 <- tidyr::unnest(df_classes, cols = teacher_ids)
  df_02 <- dplyr::left_join(df_02, df_timetables, by = c("period_id" = "id...2"))
  df_02 <- dplyr::left_join(df_02, df_teachers, by = c("teacher_ids" = "id"))
  df <- dplyr::left_join(df, df_02, by = c("class_id" = "id"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df <- dplyr::filter(df, !is.na(date))
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$registration_group, ")")
  df$Teacher = paste0(df$title, " ", df$preferred_last_name.y)

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("Lesson.ID" = name, "Lesson" = display_name, Teacher, "Room" = rooms,
                            "UPN" = upn, "GFSID" = id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name.x, "Forename" = preferred_first_name.x, "Gender" = sex,
                            "Year.Group" = year_group, "Reg" = registration_group,
                            "Date" = date, "Subject" = subject_code, "Class" = group_code, "Lesson.Mark" = code.x,
                            "Lesson.Label" = label,
                            "Lesson.Late.Minutes" = lesson_minutes_late, "Lesson.Notes" = lesson_notes))
  df <- dplyr::distinct(df)
  df <- dplyr::arrange(df, Lesson)

  ## Return
  return(df)
}

## Clean student attendance summary
#' Get clean student attendance summary.
#'
#' Returns clean details of student attendance summary for chosen
#' academic year.
#' @param academicYear academic year as integer.
#' @examples
#' hhs_attendance_student_summary(2020)
#' @export
hhs_attendance_student_summary <- function(academicYear) {
  ## Message
  message(cat(crayon::cyan("Generating clean student attendance summary")))

  ## Import data
  df_attendance_codes <- g4sr::gfs_attendance_codes(academicYear)
  df_attendance_student_summary <- g4sr::gfs_attendance_student_summary(academicYear)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_students, df_attendance_student_summary, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_students_details, by = c("id" = "student_id"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("UPN" = upn, "GFSID" = id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name, "Gender" = sex,
                            "Year.Group" = national_curriculum_year, "Reg" = registration_group,
                            "Possible.Session" = possible_sessions, "Present" = present, "Approved.EA" = approved_educational_activity,
                            "Late" = late, "Authorised.Absence" = authorised_absence, "Unauthorised.Absence" = unauthorised_absence,
                            "Attendance.No.Required" = attendance_not_required, "Missing.Mark" = missing_mark))
  df <- dplyr::distinct(df)

  ## Return
  return(df)
}

## Clean student session marks date range
#' Get clean student session marks over a range of dates.
#'
#' Returns clean student session marks over a range of dates for chosen
#' academic year.
#' @param academicYear academic year as integer.
#' @param goDateStart start date of range as string in the form yyyy-mm-dd.
#' @param goDateEnd end date of range as string in the form yyyy-mm-dd.
#' @param session session as string.
#' @examples
#' hhs_attendance_student_session_range(2021, "2021-07-12", "2021-07-16")
#' hhs_attendance_student_session_range(2021, "2021-07-12", "2021-07-16", session = "am")
#' @export
hhs_attendance_student_session_range <- function(academicYear, goDateStart, goDateEnd, session = NULL) {
  ## Message
  message(cat(crayon::cyan("Generating clean student session marks for", goDateStart, "to", goDateEnd)))

  ## Import data
  df_attendance_codes <- g4sr::gfs_attendance_codes(academicYear)
  # Create date range sequence
  goDate <- seq(as.Date(goDateStart), as.Date(goDateEnd), 1)
  # Iterate over date range
  df_attendance_student_session_marks <- lapply(1:length(goDate),
                                                function(i) g4sr::gfs_attendance_student_session_marks(academicYear = academicYear,
                                                                                                       goDate = goDate[i]))
  # Bind list
  df_attendance_student_session_marks <- data.table::rbindlist(df_attendance_student_session_marks)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_students, df_attendance_student_session_marks, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_attendance_codes, by = c("session_mark_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("id" = "student_id"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df <- dplyr::filter(df, !is.na(date))
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("UPN" = upn, "GFSID" = id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Year.Group" = national_curriculum_year, "Reg" = registration_group, "Gender" = sex,
                            "Date" = date, "Session" = session, "Session.Mark" = code, "Session.Label" = label,
                            "Session.Late.Minutes" = session_minutes_late, "Session.Notes" = session_notes))
  df <- dplyr::distinct(df)

  if (is.null(session)) {
    message(cat(crayon::magenta("No session preference provided.  Returning AM and PM.")))
    df <- df
  } else {
    df <- dplyr::filter(df, Session == toupper(session))
  }

  ## Return
  return(df)
}

## Clean student lesson marks date range
#' Get clean student lesson marks over a range of dates.
#'
#' Returns clean student lesson marks over a range of dates for chosen
#' academic year.
#' @param academicYear academic year as integer.
#' @param goDateStart start date of range as string in the form yyyy-mm-dd.
#' @param goDateEnd end date of range as string in the form yyyy-mm-dd.
#' @examples
#' hhs_attendance_student_lesson_range(2021, "2021-07-12", "2021-07-16")
#' @export
hhs_attendance_student_lesson_range <- function(academicYear, goDateStart, goDateEnd) {
  ## Message
  message(cat(crayon::cyan("Generating clean student lesson marks for", goDateStart, "to", goDateEnd)))

  ## Import data
  df_attendance_codes <- g4sr::gfs_attendance_codes(academicYear)
  # Create date range sequence
  goDate <- seq(as.Date(goDateStart), as.Date(goDateEnd), 1)
  # Iterate over date range
  df_attendance_student_lesson_marks <- lapply(1:length(goDate),
                                               function(i) g4sr::gfs_attendance_student_lesson_marks(academicYear = academicYear,
                                                                                                     goDate = goDate[i]))
  # Bind list
  df_attendance_student_lesson_marks <- data.table::rbindlist(df_attendance_student_lesson_marks)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_classes <- g4sr::gfs_classes(academicYear)
  df_timetables <- g4sr::gfs_timetables(academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_students, df_attendance_student_lesson_marks, by = c("id" = "student_id"))
  df <- dplyr::left_join(df, df_attendance_codes, by = c("lesson_mark_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("id" = "student_id"))
  df_02 <- tidyr::unnest(df_classes, cols = teacher_ids)
  df_02 <- dplyr::left_join(df_02, df_timetables, by = c("period_id" = "id...2"))
  df_02 <- dplyr::left_join(df_02, df_teachers, by = c("teacher_ids" = "id"))
  df <- dplyr::left_join(df, df_02, by = c("class_id" = "id"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df <- dplyr::filter(df, !is.na(date))
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$registration_group, ")")
  df$Teacher = paste0(df$title, " ", df$preferred_last_name.y)

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("Lesson.ID" = name, "Lesson" = display_name, Teacher, "Room" = rooms,
                            "UPN" = upn, "GFSID" = id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name.x, "Forename" = preferred_first_name.x, "Gender" = sex,
                            "Year.Group" = year_group, "Reg" = registration_group,
                            "Date" = date, "Subject" = subject_code, "Class" = group_code, "Lesson.Mark" = code.x,
                            "Lesson.Label" = label,
                            "Lesson.Late.Minutes" = lesson_minutes_late, "Lesson.Notes" = lesson_notes))
  df <- dplyr::distinct(df)
  df <- dplyr::arrange(df, Date, Lesson, Lesson.ID)

  ## Return
  return(df)
}
