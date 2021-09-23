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
  df$Grade.Type <- paste0("External examination")

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group, "Qualification.Title" = qualification_title.x,
                            "Subject" = name, Class, "UPN" = upn, "GFSID" = student_id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
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

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group, "Subject" = name.y, Class,
                            "UPN" = upn, "GFSID" = grades.student_id, Surname.Forename.Reg,
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

  message(cat(crayon::silver("Clean final output")))

  ## Tidy
  df <- dplyr::select(df, c(Staff.Code, "Year.Group" = year_group, "Subject" = name.y, Class,
                            "UPN" = upn, "GFSID" = grades.student_id, Surname.Forename.Reg,
                            "Surname" = preferred_last_name, "Forename" = preferred_first_name,
                            "Reg" = registration_group, "Gender" = sex, "Grade.Type" = name.x, "Grade" = grades.name))
  df <- dplyr::distinct(df)

  ## Return
  return(df)
}

## Clean science targets
#' Get clean science targets.
#'
#' Returns clean details of science targets in the chosen academic year for all
#' year groups.
#' @importFrom magrittr "%>%"
#' @param academicYear academic year as integer.
#' @param layout layout as string.  Choice of "wide" or "long".  Defaults is "wide".
#' @examples
#' hhs_targets_science(2022, layout = "wide")
#' @export
hhs_targets_science <- function(academicYear, layout = "wide") {
  ## Message
  message(cat(crayon::cyan("Generating clean science targets for", academicYear)))

  ## Import data
  df_attainment <- hhs_attainment_multiple(academicYear = academicYear)
  df_student_details <- hhs_student_details_general(academicYear = academicYear)

  message(cat(crayon::silver("Subset data")))

  ## Filter data
  # for target
  df_target <- df_attainment %>%
    dplyr::filter(Grade.Type == "Target")
  # for science
  df_target_sci <- df_target %>%
    dplyr::filter(Subject %in% c("Science", "Combined Science 9-1", "Biology", "Chemistry", "Physics"))

  ## Rename triple science classes
  df_target_sci <- df_target_sci %>%
    dplyr::mutate(Class = ifelse(grepl(pattern = "Yr10X/Bi|Yr10X/Ch|Yr10X/Ph", x = Class, ignore.case = TRUE), "Yr10ab/TrX",
                                 ifelse(grepl(pattern = "Yr10Y/Bi|Yr10Y/Ch|Yr10Y/Ph", x = Class, ignore.case = TRUE), "Yr10ab/TrY",
                                        ifelse(grepl(pattern = "Yr10Z/Bi|Yr10Z/Ch|Yr10Z/Ph", x = Class, ignore.case = TRUE), "Yr10ab/TrZ", Class))))

  df_target_sci <- df_target_sci %>%
    dplyr::mutate(Class = ifelse(grepl(pattern = "Yr11X/Bi|Yr11X/Ch|Yr11X/Ph", x = Class, ignore.case = TRUE), "Yr11ab/TrX",
                                 ifelse(grepl(pattern = "Yr11Y/Bi|Yr11Y/Ch|Yr11Y/Ph", x = Class, ignore.case = TRUE), "Yr11ab/TrY",
                                        ifelse(grepl(pattern = "Yr11Z/Bi|Yr11Z/Ch|Yr11Z/Ph", x = Class, ignore.case = TRUE), "Yr11ab/TrZ", Class))))

  message(cat(crayon::silver("Reshape dataset")))

  ## Pivot wide
  df_target_sci_wide <- df_target_sci %>%
    dplyr::select(-c(Staff.Code, Grade.Type)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = Subject, values_from = Grade, values_fill = NA) %>%
    data.frame(check.names = TRUE)

  ## Convert combined science
  df_target_sci_wide <- df_target_sci_wide %>%
    tidyr::separate(col = Combined.Science.9.1, into = c("Combined.Science.1", "Combined.Science.2"), sep = "/") %>%
    dplyr::mutate_at(.vars = c("Combined.Science.1",
                               "Combined.Science.2"), list(as.integer))

  df_target_sci_wide <- df_target_sci_wide %>%
    dplyr::mutate(Combined.Science = (Combined.Science.1 + Combined.Science.2) / 2) %>%
    dplyr::select(-c(Combined.Science.1, Combined.Science.2)) %>%
    dplyr::mutate_at(.vars = c("Combined.Science"), list(as.character))

  # df_target_sci_wide <- df_target_sci_wide %>%
  #   mutate(Science = ifelse(is.na(Science), paste0(Combined.Science), Science)) %>%
  #   mutate(Science = ifelse(Science == "NA", NA, Science)) %>%
  #   select(-c(Combined.Science))

  message(cat(crayon::silver("Merge datasets")))

  ## Retain PP and HML data from student details and merge
  df_target_sci_wide <- df_target_sci_wide %>%
    dplyr::select(c(UPN, Class, Science:Combined.Science))

  df <- dplyr::left_join(df_student_details, df_target_sci_wide, by = c("UPN"))

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
    dplyr::select(c(Ac.Year, Year.Group, Class, UPN, GFSID, Surname.Forename.Reg,
                    Gender, Reg, Ethnicity, PP, WBr.PP, HML.Band, SEN, SEN.Notes,
                    Science, Combined.Science, Biology, Chemistry, Physics))
  df <- dplyr::distinct(df)
  df$Year.Group <- factor(df$Year.Group, levels = c("7", "8", "9", "10", "11"))
  df <- df %>% dplyr::arrange(Year.Group, Class, Surname.Forename.Reg)
  df <- df %>% dplyr::mutate_at(.vars = c("Year.Group"), list(as.character))

  message(cat(crayon::silver("Reshaping dataset")))

  if (layout == "long") {
    message(cat(crayon::silver("Returning long-form")))
    message(cat(crayon::red("Warning: dropped students with missing targets")))
    df <- df %>%
      tidyr::pivot_longer(cols = c(Science:Physics), names_to = "Subject", values_to = "Target", values_drop_na = TRUE)
  } else {
    message(cat(crayon::silver("Returning wide-form")))
    df <- df
  }

  ## Return
  return(df)
}
