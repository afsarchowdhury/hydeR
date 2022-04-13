# Clean class list functions ---------------------------

## Class list by teacher
#' Get teacher-focused class list.
#'
#' Returns clean details of all classes in the chosen academic year for chosen
#' staff.  If staff code is not provided, class list for all staff are returned.
#' @param academicYear academic year as integer.
#' @param staffCode case-sensitive staff code as string.
#' @param yearGroupFrom starting year group as string.  Defaults to "7".
#' @param yearGroupTo ending year group as string.  Defaults to "11".
#' @examples
#' hhs_class_list_teacher(2020, "ACH")
#' hhs_class_list_teacher(2020)
#' @export
hhs_class_list_teacher <- function(academicYear, staffCode = NULL, yearGroupFrom = "7", yearGroupTo = "11") {
  ## Message
  message(cat(crayon::cyan("Generating clean teacher-focused class list")))

  ## Import data
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear = academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear = academicYear)
  df_teaching_groups_teachers <- g4sr::gfs_teaching_groups_teachers(academicYear = academicYear)
  df_teaching_subjects <- g4sr::gfs_teaching_subjects(academicYear = academicYear)
  df_students <- g4sr::gfs_student_details(academicYear = academicYear)
  df_students_general <- g4sr::gfs_student_general(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear = academicYear)
  df_students_sensitive <- g4sr::gfs_student_sensitive(academicYear = academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear = academicYear)
  # Loop attainment grades
  df_att_grades <- lapply(seq(yearGroupFrom, yearGroupTo, 1), function(i) g4sr::gfs_attainment_grades(academicYear, yearGroup = i))
  df_att_grades <- dplyr::bind_rows(df_att_grades)
  df_att_grades <- dplyr::distinct(df_att_grades)
  # Loop attainment grade types
  df_att_grade_types <- lapply(seq(yearGroupFrom, yearGroupTo, 1), function(i) g4sr::gfs_attainment_grade_types(academicYear, yearGroup = i))
  df_att_grade_types <- dplyr::bind_rows(df_att_grade_types)
  df_att_grade_types <- dplyr::distinct(df_att_grade_types)

  message(cat(crayon::silver("Tidy datasets")))

  ## Tidy general
  df_students_general_02 <- dplyr::select(df_students_general, c(student_id, name, value))
  df_students_general_02 <- dplyr::filter(df_students_general_02,
                                          grepl(pattern = "uci|hml|sen|key|caf",
                                                x = name, ignore.case = TRUE))
  df_students_general_02 <- tidyr::pivot_wider(df_students_general_02, names_from = name, values_from = value)
  df_students_general_02 <- data.frame(df_students_general_02, check.names = TRUE)
  df_students_general_02 <- dplyr::select(df_students_general_02,
                                          c(student_id, UCI, HML.Band, "SEN" = X3...SEN.Code,
                                            "SEN.Notes" = X4..SEN.Notes, "Keyworker" = X5..Keyworker.Name, CP.CAF))
  df_students_general_02 <- dplyr::distinct(df_students_general_02)

  ## Tidy sensitive
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive, c(student_id, name, value))
  df_students_sensitive_02 <- tidyr::pivot_wider(df_students_sensitive_02, names_from = name, values_from = value)
  df_students_sensitive_02 <- data.frame(df_students_sensitive_02, check.names = TRUE)
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive_02, c(student_id, "LAC" = Looked.after, Ethnicity, EAL, FSM,
                                                                        "PP" = Pupil.Premium.Indicator))

  ## Tidy attainment
  df_att <- dplyr::left_join(df_att_grades, df_att_grade_types, by = c("grades.grade_type_id" = "id"))
  df_att <- dplyr::filter(df_att, name == "Target")
  df_att <- dplyr::left_join(df_att, df_teaching_subjects, by = c("grades.subject_id" = "id"))
  df_att <- dplyr::left_join(df_att, df_students, by = c("grades.student_id" = "id"))
  df_att <- dplyr::left_join(df_att, df_students_details, by = c("grades.student_id" = "student_id"))
  df_att <- dplyr::select(df_att, c("GFSID" = grades.student_id, "Subject" = name.y, "Target" = grades.name))
  df_att <- dplyr::distinct(df_att)

  message(cat(crayon::silver("Merge datasets")))

  ## Merge datasets
  df <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df <- dplyr::left_join(df, df_students, by = c("student_ids" = "id"))
  df <- dplyr::left_join(df, df_teaching_groups_teachers, by = c("id" = "group_id"))
  df <- dplyr::left_join(df, df_teachers, by = c("teacher_ids" = "id"))
  df <- dplyr::left_join(df, df_teaching_subjects, by = c("subject_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_students_sensitive_02, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_students_general_02, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_att, by = c("student_ids" = "GFSID", "name.y" = "Subject"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$student_ids, ")")
  df <- dplyr::mutate(df, WBr.PP = ifelse(grepl(pattern = "english|scottish|welsh", x = Ethnicity, ignore.case = TRUE) & PP == "True", "True", "False"))

  message(cat(crayon::silver("Clean final output")))

  ## Clean and filter
  df <- dplyr::select(df, c("Staff.Code" = code.y, "Year.Group" = year_group, "Reg" = registration_group, "Subject" = name.y,
                            "Class" = name.x, "UPN" = upn, "GFSID" = student_ids, UCI, Surname.Forename.Reg, Surname.Forename.ID,
                            "Surname" = preferred_last_name.x, "Forename" = preferred_first_name.x,
                            "Gender" = sex, LAC, Ethnicity, EAL, FSM, PP, WBr.PP, HML.Band, Target,
                            SEN, SEN.Notes, Keyworker, CP.CAF))

  if (is.null(staffCode)) {
    message(cat(crayon::silver("No staffCode provided.  Return all.")))
    df <- df
  } else {
    message(cat(crayon::silver("Returning class list for", staffCode)))
    df <- dplyr::filter(df, Staff.Code %in% staffCode)
  }

  df <- dplyr::distinct(df)

  message(cat(crayon::silver("Impute missing data")))

  ## Impute missing data
  df$HML.Band <- ifelse(is.na(df$HML.Band), "Unknown.HML", df$HML.Band)
  df$Ethnicity <- ifelse(is.na(df$Ethnicity), "Unknown.Ethnicity", df$Ethnicity)
  df$EAL <- ifelse(is.na(df$EAL), "False", df$EAL)

  ## Return
  return(df)
}

## Class list by student
#' Get student-focused class list.
#'
#' Returns clean details of all classes in the chosen academic year for chosen
#' student.
#' @param academicYear academic year as integer.
#' @param student student search string.
#' @examples
#' hhs_class_list_student(2020, "smith")
#' @export
hhs_class_list_student <- function(academicYear, student) {
  ## Message
  message(cat(crayon::cyan("Generating clean student-focused class list")))

  ## Import data
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear = academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear = academicYear)
  df_teaching_groups_teachers <- g4sr::gfs_teaching_groups_teachers(academicYear = academicYear)
  df_teaching_subjects <- g4sr::gfs_teaching_subjects(academicYear = academicYear)
  df_students <- g4sr::gfs_student_details(academicYear = academicYear)
  df_students_general <- g4sr::gfs_student_general(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear = academicYear)
  df_students_sensitive <- g4sr::gfs_student_sensitive(academicYear = academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear = academicYear)

  message(cat(crayon::silver("Tidy datasets")))

  ## Tidy general
  df_students_general_02 <- dplyr::select(df_students_general, c(student_id, name, value))
  df_students_general_02 <- dplyr::filter(df_students_general_02,
                                          grepl(pattern = "uci|hml|sen|key|caf",
                                                x = name, ignore.case = TRUE))
  df_students_general_02 <- tidyr::pivot_wider(df_students_general_02, names_from = name, values_from = value)
  df_students_general_02 <- data.frame(df_students_general_02, check.names = TRUE)
  df_students_general_02 <- dplyr::select(df_students_general_02,
                                          c(student_id, UCI, HML.Band, "SEN" = X3...SEN.Code,
                                            "SEN.Notes" = X4..SEN.Notes, "Keyworker" = X5..Keyworker.Name, CP.CAF))
  df_students_general_02 <- dplyr::distinct(df_students_general_02)

  ## Tidy sensitive
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive, c(student_id, name, value))
  df_students_sensitive_02 <- tidyr::pivot_wider(df_students_sensitive_02, names_from = name, values_from = value)
  df_students_sensitive_02 <- data.frame(df_students_sensitive_02, check.names = TRUE)
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive_02, c(student_id, "LAC" = Looked.after, Ethnicity, EAL, FSM,
                                                                        "PP" = Pupil.Premium.Indicator))

  message(cat(crayon::silver("Merge datasets")))

  ## Merge datasets
  df <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df <- dplyr::left_join(df, df_students, by = c("student_ids" = "id"))
  df <- dplyr::left_join(df, df_teaching_groups_teachers, by = c("id" = "group_id"))
  df <- dplyr::left_join(df, df_teachers, by = c("teacher_ids" = "id"))
  df <- dplyr::left_join(df, df_teaching_subjects, by = c("subject_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_students_sensitive_02, by = c("student_ids" = "student_id"))
  df <- dplyr::left_join(df, df_students_general_02, by = c("student_ids" = "student_id"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name.x), " ", df$preferred_first_name.x, " (", df$student_ids, ")")
  df <- dplyr::mutate(df, WBr.PP = ifelse(grepl(pattern = "english|scottish|welsh", x = Ethnicity, ignore.case = TRUE) & PP == "True", "True", "False"))

  message(cat(crayon::silver("Clean final output")))

  ## Clean
  df <- dplyr::select(df, c("UPN" = upn, "GFSID" = student_ids, UCI, Surname.Forename.Reg, Surname.Forename.ID,
                            "Surname" = preferred_last_name.x, "Forename" = preferred_first_name.x, "Gender" = sex,
                            LAC, Ethnicity, EAL, FSM, PP, WBr.PP, HML.Band, SEN, SEN.Notes, Keyworker, CP.CAF,
                            "Year.Group" = year_group, "Reg" = registration_group, "Subject" = name.y, "Class" = name.x,
                            "Staff.Code" = code.y))

  df <- dplyr::filter(df, grepl(pattern = student, x = df$Surname.Forename, ignore.case = TRUE))
  df <- dplyr::distinct(df)

  message(cat(crayon::silver("Impute missing data")))

  ## Impute missing data
  df$HML.Band <- ifelse(is.na(df$HML.Band), "Unknown.HML", df$HML.Band)
  df$Ethnicity <- ifelse(is.na(df$Ethnicity), "Unknown.Ethnicity", df$Ethnicity)
  df$EAL <- ifelse(is.na(df$EAL), "False", df$EAL)

  ## Return
  return(df)
}
