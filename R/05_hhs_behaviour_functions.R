# Clean behaviour functions ---------------------------

## Clean behaviour events
#' Get clean behaviour events.
#'
#' Returns clean behaviour events over a range of dates within in the chosen
#' academic year.
#' @param academicYear academic year as integer.
#' @param goDateStart start date of range as string in the form yyyy-mm-dd.
#' @param goDateEnd end date of range as string in the form yyyy-mm-dd.
#' @examples
#' hhs_behaviour_events_range(2021, "2021-07-12", "2021-07-16")
#' @export
hhs_behaviour_events_range <- function(academicYear, goDateStart, goDateEnd) {
  ## Mesaage
  message(cat(crayon::cyan("Generating clean behaviour events for", goDateStart, "to", goDateEnd)))

  ## Import data
  df_behaviour_events <- g4sr::gfs_behaviour_events_range(academicYear, goDateStart, goDateEnd)
  df_behaviour_event_types <- g4sr::gfs_behaviour_event_types(academicYear)
  df_behaviour_classification <- g4sr::gfs_behaviour_classification(academicYear)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_subjects <- g4sr::gfs_teaching_subjects(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear)
  df_staff <- g4sr::gfs_users_staff()
  df_cal <- g4sr::gfs_calendar(academicYear)

  message(cat(crayon::magenta("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_behaviour_events, df_behaviour_event_types, by = c("event_type_id" = "id"))
  df <- dplyr::left_join(df, df_behaviour_classification, by = c("event_classification_id" = "id"))
  df <- dplyr::left_join(df, df_students, by = c("student_ids" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_staff, by = c("created_by" = "id"))
  df_02 <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_subjects, by = c("subject_id" = "id"))
  df_02 <- dplyr::left_join(df_02, df_students, by = c("student_ids" = "id"))
  df_02 <- dplyr::select(df_02, c("Class" = name.x, "subject_code" = code.y, student_ids))
  df_02 <- dplyr::distinct(df_02)
  df <- dplyr::left_join(df, df_02, by = c("subject_code", "student_ids"))
  df_cal <- dplyr::select(df_cal, c("Date" = date, "School.Week" = week, "Day.Type" = day_type_code))
  df <- dplyr::left_join(df, df_cal, by = c("event_date" = "Date"))

  message(cat(crayon::magenta("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")

  message(cat(crayon::magenta("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c("Event.ID" = id, "Staff" = display_name, "Email.Staff" = email_address,
                            "Year.Group" = national_curriculum_year, "Subject" = subject_code, Class, "Room" = room_name,
                            "UPN" = upn, "GFSID" = student_ids, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Reg" = registration_group, "Gender" = sex,
                            "Date" = event_date, "Timestamp" = created_timestamp, School.Week, Day.Type,
                            "Event.Code" = code, "Event.Name" = name.x, "Event.Classification" = name.y,
                            "Polarity" = significance, "Score" = score, "School.Notes" = school_notes))
  df <- dplyr::mutate_all(df, .funs = as.character)
  df <- dplyr::distinct(df)
  df$Email.Staff <- tolower(df$Email.Staff)

  message(cat(crayon::magenta("Lookup student demographic data")))

  ## Student demographics
  df_stu <- hhs_student_details_general(academicYear)
  df_stu <- dplyr::select(df_stu, c(GFSID, Ethnicity:ncol(df_stu)))
  df <- dplyr::left_join(df, df_stu, by = c("GFSID"))

  ## Return
  return(df)
}
