# Clean attainment functions ---------------------------

## Clean exam results
#' Get clean external examination results.
#'
#' Returns clean details of external examination results in the chosen
#' academic year for chosen
#' year group.
#' @param academicYear academic year as integer.
#' @param yearGroup year group as string.
#' @param type qualification type as string.
#' @examples
#' hhs_exam_results(2020, "11")
#' hhs_exam_results(2020, "11", type = "gcse")
#' hhs_exam_results(2020, "11", type = c("gcse", "btec"))
#' @export
hhs_exam_results <- function(academicYear, yearGroup, type = NULL) {
  ## Message
  message(cat(crayon::cyan("Generating clean external examination results for", academicYear, "year", yearGroup)))

  ## Import data
  df_att_exam_results <- g4sr::gfs_attainment_exam_results(academicYear, yearGroup)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_students_general <- g4sr::gfs_student_general(academicYear)
  df_students_sensitive <- g4sr::gfs_student_sensitive(academicYear)
  df_subjects <- g4sr::gfs_teaching_subjects(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear)
  df_teaching_groups_teachers <- g4sr::gfs_teaching_groups_teachers(academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear)

  message(cat(crayon::silver("Tidy datasets")))

  ## Tidy general
  df_students_general_02 <- dplyr::select(df_students_general, c(student_id, name, value))
  df_students_general_02 <- dplyr::filter(df_students_general_02,
                                          grepl(pattern = "hml", x = name, ignore.case = TRUE))
  df_students_general_02 <- tidyr::pivot_wider(df_students_general_02, names_from = name, values_from = value)
  df_students_general_02 <- data.frame(df_students_general_02, check.names = TRUE)
  df_students_general_02 <- dplyr::select(df_students_general_02, c(student_id, HML.Band))
  df_students_general_02 <- dplyr::distinct(df_students_general_02)

  ## Tidy sensitive
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive, c(student_id, name, value))
  df_students_sensitive_02 <- dplyr::filter(df_students_sensitive_02,
                                            grepl(pattern = "premium", x = name, ignore.case = TRUE))
  df_students_sensitive_02 <- tidyr::pivot_wider(df_students_sensitive_02, names_from = name, values_from = value)
  df_students_sensitive_02 <- data.frame(df_students_sensitive_02, check.names = TRUE)
  df_students_sensitive_02 <- dplyr::select(df_students_sensitive_02, c(student_id, "PP" = Pupil.Premium.Indicator))
  df_students_sensitive_02 <- dplyr::distinct(df_students_sensitive_02)

  message(cat(crayon::silver("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_att_exam_results, df_subjects, by = c("subject_id" = "id"))
  df <- dplyr::left_join(df, df_students, by = c("student_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("student_id" = "student_id"))
  df <- dplyr::left_join(df, df_students_general_02, by = c("student_id" = "student_id"))
  df <- dplyr::left_join(df, df_students_sensitive_02, by = c("student_id" = "student_id"))
  df_02 <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_subjects, by = c("subject_id" = "id"))
  df_02 <- dplyr::left_join(df_02, df_teaching_groups_teachers, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_teachers, by = c("teacher_ids" = "id"))
  df_02 <- dplyr::select(df_02, c("Class" = name.x, student_ids, "Subject.Code" = code.y, "Staff.Code" = code))
  df_02 <- dplyr::distinct(df_02)
  df <- dplyr::left_join(df, df_02, by = c("student_id" = "student_ids", "code" = "Subject.Code"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$student_id, ")")
  df$Grade.Type <- paste0("External examination")

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group.y, "Qualification.Title" = qualification_title.x,
                            "Subject" = name, Class, "UPN" = upn, "GFSID" = student_id, Surname.Forename.Reg,
                            Surname.Forename.ID, "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Reg" = registration_group, "Gender" = sex, PP, HML.Band, Grade.Type, "Grade" = grade))
  df <- dplyr::distinct(df)

  message(cat(crayon::silver("Impute missing data")))

  ## Impute missing data
  df$HML.Band <- ifelse(is.na(df$HML.Band), "Unknown.HML", df$HML.Band)

  ## Qualification type
  if (is.null(type)) {
    df <- df
  } else {
    my_query <- c(type)
    my_query <- glue::glue_collapse(x = my_query, sep = "|", last = "")
    df <- dplyr::filter(df, grepl(pattern = my_query, x = Qualification.Title, ignore.case = TRUE))
  }

  ## Return
  return(df)
}

## Clean attainment
#' Get clean attainment data.
#'
#' Returns clean details of attainment in the chosen academic year for chosen
#' year group.
#' @param academicYear academic year as integer.
#' @param yearGroup year group as string.
#' @examples
#' hhs_attainment(2020, "10")
#' @export
hhs_attainment <- function(academicYear, yearGroup) {
  ## Message
  message(cat(crayon::cyan("Generating clean attainment data for", academicYear, "year", yearGroup)))

  ## Import data
  df_att_grades <- g4sr::gfs_attainment_grades(academicYear, yearGroup)
  df_att_grade_types <- g4sr::gfs_attainment_grade_types(academicYear, yearGroup)
  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_teaching_subjects <- g4sr::gfs_teaching_subjects(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear)
  df_teaching_groups_teachers <- g4sr::gfs_teaching_groups_teachers(academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear)

  message(cat(crayon::silver("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_att_grades, df_att_grade_types, by = c("grades.grade_type_id" = "id"))
  df <- dplyr::left_join(df, df_teaching_subjects, by = c("grades.subject_id" = "id"))
  df <- dplyr::left_join(df, df_students, by = c("grades.student_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("grades.student_id" = "student_id"))
  df_02 <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_teaching_subjects, by = c("subject_id" = "id"))
  df_02 <- dplyr::left_join(df_02, df_teaching_groups_teachers, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_teachers, by = c("teacher_ids" = "id"))
  df_02 <- dplyr::select(df_02, c("Class" = name.x, student_ids, "Subject.Code" = code.y, "Staff.Code" = code))
  df_02 <- dplyr::distinct(df_02)
  df <- dplyr::left_join(df, df_02, by = c("grades.student_id" = "student_ids", "code" = "Subject.Code"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$grades.student_id, ")")

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group.y, "Subject" = name.y, Class,
                            "UPN" = upn, "GFSID" = grades.student_id, Surname.Forename.Reg, Surname.Forename.ID,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Reg" = registration_group, "Gender" = sex, "Grade.Type" = name.x, "Grade" = grades.name))
  df <- dplyr::distinct(df)

  ## Return
  return(df)
}

## Clean attainment multiple
#' Get clean attainment data for multiple year groups.
#'
#' Returns clean details of attainment in the chosen academic year for multiple
#' year groups.
#' @param academicYear academic year as integer.
#' @param yearGroupFrom starting year group as string.  Defaults to "7".
#' @param yearGroupTo ending year group as string.  Defaults to "11".
#' @examples
#' hhs_attainment_multiple(2021, "7", "11")
#' @export
hhs_attainment_multiple <- function(academicYear, yearGroupFrom = "7", yearGroupTo = "11") {
  ## Message
  message(cat(crayon::cyan("Generating clean attainment data for year groups", yearGroupFrom, "to", yearGroupTo)))

  ## Import data
  # Loop attainment grades
  df_att_grades <- lapply(seq(yearGroupFrom, yearGroupTo, 1), function(i) g4sr::gfs_attainment_grades(academicYear, yearGroup = i))
  df_att_grades <- dplyr::bind_rows(df_att_grades)
  df_att_grades <- dplyr::distinct(df_att_grades)
  # Loop attainment grade types
  df_att_grade_types <- lapply(seq(yearGroupFrom, yearGroupTo, 1), function(i) g4sr::gfs_attainment_grade_types(academicYear, yearGroup = i))
  df_att_grade_types <- dplyr::bind_rows(df_att_grade_types)
  df_att_grade_types <- dplyr::distinct(df_att_grade_types)

  df_students <- g4sr::gfs_student_details(academicYear)
  df_students_details <- g4sr::gfs_student_edu_details(academicYear)
  df_teaching_subjects <- g4sr::gfs_teaching_subjects(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_teaching_groups_students <- g4sr::gfs_teaching_groups_students(academicYear)
  df_teaching_groups_teachers <- g4sr::gfs_teaching_groups_teachers(academicYear)
  df_teachers <- g4sr::gfs_teaching_teachers(academicYear)

  message(cat(crayon::silver("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_att_grades, df_att_grade_types, by = c("grades.grade_type_id" = "id"))
  df <- dplyr::left_join(df, df_teaching_subjects, by = c("grades.subject_id" = "id"))
  df <- dplyr::left_join(df, df_students, by = c("grades.student_id" = "id"))
  df <- dplyr::left_join(df, df_students_details, by = c("grades.student_id" = "student_id"))
  df_02 <- dplyr::left_join(df_teaching_groups, df_teaching_groups_students, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_teaching_subjects, by = c("subject_id" = "id"))
  df_02 <- dplyr::left_join(df_02, df_teaching_groups_teachers, by = c("id" = "group_id"))
  df_02 <- dplyr::left_join(df_02, df_teachers, by = c("teacher_ids" = "id"))
  df_02 <- dplyr::select(df_02, c("Class" = name.x, student_ids, "Subject.Code" = code.y, "Staff.Code" = code))
  df_02 <- dplyr::distinct(df_02)
  df <- dplyr::left_join(df, df_02, by = c("grades.student_id" = "student_ids", "code" = "Subject.Code"))

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")
  df$Surname.Forename.ID <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$grades.student_id, ")")

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group.y, "Subject" = name.y, Class,
                            "UPN" = upn, "GFSID" = grades.student_id, Surname.Forename.Reg, Surname.Forename.ID,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Reg" = registration_group, "Gender" = sex, "Grade.Type" = name.x, "Grade" = grades.name))
  df <- dplyr::distinct(df)

  ## Return
  return(df)
}

## Clean targets
#' Get clean targets.
#'
#' Returns clean details of targets in the chosen academic year and subject for all
#' year groups.
#' @importFrom magrittr "%>%"
#' @param academicYear academic year as integer.
#' @param subject subject as string.  .  Defaults to null.
#' @examples
#' hhs_targets(2022, "Combined Science 9-1")
#' @export
hhs_targets <- function(academicYear, subject = NULL) {
  ## Message
  message(cat(crayon::cyan("Generating clean targets for", academicYear)))

  ## Import data
  df_class_list <- hhs_class_list_teacher(academicYear)

  if (is.null(subject)) {
    message(cat(crayon::silver("Subject not specified.  Return all.")))
    df <- df_class_list %>%
      dplyr::select(c(-Target, Target))
  } else if (subject == "Combined Science 9-1") {
    message(cat(crayon::silver("Subset data for", tolower(subject))))
    message(cat(crayon::silver("Transforming double-grade separator")))
    df <- df_class_list %>%
      dplyr::filter(Subject == subject) %>%
      tidyr::separate(col = Target, into = c("Combined.Science.1", "Combined.Science.2"), sep = "/") %>%
      dplyr::mutate_at(.vars = c("Combined.Science.1",
                                 "Combined.Science.2"), list(as.integer))

    df <- df %>%
      dplyr::mutate(Target = (Combined.Science.1 + Combined.Science.2) / 2) %>%
      dplyr::select(-c(Combined.Science.1, Combined.Science.2)) %>%
      dplyr::mutate_at(.vars = c("Target"), list(as.character))
  } else {
    message(cat(crayon::silver("Subset data for", tolower(subject))))
    df <- df_class_list %>%
      dplyr::filter(Subject == subject) %>%
      dplyr::select(c(-Target, Target))
  }

  message(cat(crayon::silver("Compute metadata")))

  ## Encode academic year
  df <- df %>%
    dplyr::mutate(
      Ac.Year = ifelse(Year.Group == "11", paste0("c", academicYear),
                       ifelse(Year.Group == "10", paste0("c", academicYear + 1),
                              ifelse(Year.Group == "9", paste0("c", academicYear + 2),
                                     ifelse(Year.Group == "8", paste0("c", academicYear + 3),
                                            ifelse(Year.Group == "7", paste0("c", academicYear + 4), NA)))))
    )

  message(cat(crayon::silver("Clean final output")))

  ## Final selection
  df <- df %>%
    dplyr::select(c(Staff.Code, Ac.Year, Year.Group, Subject, Class, UPN, GFSID,
                    Surname.Forename.Reg, Surname.Forename.ID, Gender, Reg, Ethnicity, PP, WBr.PP,
                    HML.Band, SEN, SEN.Notes, Target))
  df <- dplyr::distinct(df)
  df$Year.Group <- factor(df$Year.Group, levels = c("7", "8", "9", "10", "11"))
  df <- df %>% dplyr::arrange(Year.Group, Class, Surname.Forename.Reg)
  df <- df %>% dplyr::mutate_at(.vars = c("Year.Group"), list(as.character))

  ## Return
  return(df)
}

## Clean markbook attainment
#' Get clean markbook attainment.
#'
#' Returns clean details of attainment in the chosen academic year for all
#' markbooks.
#' @importFrom magrittr "%>%"
#' @param academicYear academic year as integer.
#' @examples
#' hhs_markbook_attainment(2022)
#' @export
hhs_markbook_attainment <- function(academicYear) {
  ## Message
  message(cat(crayon::cyan("Generating clean markbook attainment for", academicYear)))

  ## Import data
  df_class_list <- hhs_class_list_teacher(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_mb <- g4sr::gfs_assessment_markbooks(academicYear)
  df_marks <- g4sr::gfs_assessment_marks(academicYear)

  message(cat(crayon::silver("Clean data")))

  ## Clean data
  df_class_list_02 <- df_class_list %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct()

  df_teaching_groups_02 <- df_teaching_groups %>%
    dplyr::select(c("Class" = name, subject_id)) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::distinct()

  df_mb_02 <- df_mb %>% dplyr::mutate_all(as.character)
  df_marks_02 <- df_marks %>% dplyr::mutate_all(as.character)

  message(cat(crayon::silver("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_marks_02, df_mb_02, by = c("id" = "id...4"))
  df <- dplyr::left_join(df, df_teaching_groups_02, by = c("id...1" = "subject_id"))

  df <- df %>%
    dplyr::select(c("GFSID" = student_id, Class, "Markbook" = name...3,
                    "Strand" = name...5, "Mark" = mark, "Max" = max, "Grade" = grade)) %>%
    dplyr::distinct()

  df <- dplyr::left_join(df_class_list_02, df, by = c("Class", "GFSID"))

  ## Return
  return(df)
}
