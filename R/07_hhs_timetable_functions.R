# Clean timetable functions ---------------------------

## Clean timetable data
#' Get clean timetable data.
#'
#' Returns clean timetable data for chosen academic year.
#' @param academicYear academic year as integer.
#' @examples
#' hhs_timetable(2021)
#' @export
hhs_timetable <- function(academicYear) {
  ## Message
  message(cat(crayon::cyan("Generating clean timetable data for", academicYear)))
  
  ## Import data
  df_timetable <- g4sr::gfs_timetables(academicYear)
  df_calendar <- g4sr::gfs_calendar(academicYear)
  df_classes <- g4sr::gfs_classes(academicYear)
  df_student_classes <- g4sr::gfs_student_classes(academicYear)
  df_teaching_teachers <- g4sr::gfs_teaching_teachers(academicYear)
  df_teaching_groups <- g4sr::gfs_teaching_groups(academicYear)
  df_teaching_subject <- g4sr::gfs_teaching_subjects(academicYear)
  df_teaching_department <- g4sr::gfs_teaching_departments(academicYear)
  df_student_clean <- g4sr::gfs_clean_student_details_general(academicYear)
  
  message(cat(crayon::silver("Clean datasets")))
  
  ## Clean datasets
  # Clean timetable
  df_timetable_02 <- dplyr::select(df_timetable, -c(id...1))
  df_timetable_02 <- dplyr::rename(df_timetable_02, c("Day" = day_of_week, "Lesson.ID" = name,
                                                      "Period" = display_name, "Week" = week))
  # Clean calendar
  df_calendar_02 <- dplyr::rename(df_calendar, c("Week" = week, "Date" = date))
  df_calendar_02 <- dplyr::filter(df_calendar_02, day_type_code == "OPEN")
  df_calendar_02$Date <- lubridate::as_date(df_calendar_02$Date)
  df_calendar_02$Day <- lubridate::wday(df_calendar_02$Date, label = TRUE, abbr = FALSE)
  df_calendar_02 <- dplyr::arrange(df_calendar_02, Date)
  df_calendar_02 <- dplyr::select(df_calendar_02, -c(timetable_id, day_type_code))
  # Clean classes
  df_classes_02 <- dplyr::rename(df_classes, c("Year.Group" = year_group, "Subject.Code" = subject_code,
                                               "Class" = group_code, "Room" = rooms))
  df_classes_02 <- dplyr::filter(df_classes_02, !Room %in% c("character(0)", "NULL"))
  df_classes_02 <- tidyr::unnest(df_classes_02, cols = "Room")
  # Clean student classes
  df_student_classes_02 <- dplyr::rename(df_student_classes, c("student_start" = start, "student_end" = end))
  df_student_classes_02$student_start <- lubridate::as_date(df_student_classes_02$student_start)
  df_student_classes_02$student_end <- lubridate::as_date(df_student_classes_02$student_end)
  # Clean teaching
  df_teaching_teachers_02 <- dplyr::select(df_teaching_teachers, c(id, "Staff.Code" = code))
  df_teaching_groups_02 <- dplyr::select(df_teaching_groups, c("groups_id" = id, "Class" = name, subject_id))
  df_teaching_subject_02 <- dplyr::select(df_teaching_subject, c(id, "Subject.Name" = name, "Subject.Code" = code,
                                                                 "Year.Group" = year_group, department_id))
  df_teaching_department_02 <- dplyr::rename(df_teaching_department, "Department" = name)
  # Clean student
  df_student_clean_02 <- dplyr::select(df_student_clean, c(UPN:Surname.Forename.Reg, Gender:HML.Band, SEN))
  df_student_clean_02$GFSID <- as.integer(df_student_clean_02$GFSID)
  
  message(cat(crayon::silver("Merge datasets")))
  
  ## Merge datasets
  df <- dplyr::left_join(df_calendar_02, df_timetable_02, by = c("Week", "Day"))
  df <- dplyr::left_join(df, df_classes_02, by = c("id...2" = "period_id"))
  df <- dplyr::left_join(df, df_student_classes_02, by = c("id" = "id"))
  df <- dplyr::left_join(df, df_teaching_teachers_02, by = c("teacher_ids" = "id"))
  df <- dplyr::left_join(df, df_teaching_groups_02, by = c("Class"))
  df <- dplyr::left_join(df, df_teaching_subject_02, by = c("subject_id" = "id", "Subject.Code", "Year.Group"))
  df <- dplyr::left_join(df, df_teaching_department_02, by = c("department_id" = "id"))
  df <- dplyr::left_join(df, df_student_clean_02, by = c("student_id" = "GFSID"))
  
  message(cat(crayon::silver("Clean final output")))
  
  ## Tidy
  df <- dplyr::mutate(df, student_end = ifelse(student_end > max(Date), NA, student_end))
  df <- dplyr::select(df, -c(Date))
  df <- dplyr::filter(df, is.na(student_end))
  df <- dplyr::filter(df, !is.na(Class))
  df <- dplyr::filter(df, !is.na(Lesson.ID))
  df <- dplyr::distinct(df)
  df <- dplyr::mutate(df, Week.Colour = ifelse(Week == 1, "Red", ifelse(Week == 2, "Blue", NA)))
  # Factor days
  df$Day <- factor(df$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
  # Factor week colour
  df$Week.Colour <- factor(df$Week.Colour, levels = c("Red", "Blue"))
  # Factor year groups
  df$Year.Group <- factor(df$Year.Group, levels = c(as.character(seq(7, 11, 1))))
  # Create lesson codes for factoring
  df_lesson_code <- data.frame(
    Week.Colour = c(rep("R", 25), rep("B", 25)),
    Week.Day = c(rep("Mon", 5), rep("Tue", 5), rep("Wed", 5), rep("Thu", 5), rep("Fri", 5),
                 rep("Mon", 5), rep("Tue", 5), rep("Wed", 5), rep("Thu", 5), rep("Fri", 5)),
    Period = rep(1:5, 5)
  )
  df_lesson_code <- dplyr::mutate(df_lesson_code, Lesson.ID = paste0(Week.Colour, Week.Day, ":", Period))
  df$Lesson.ID <- factor(df$Lesson.ID, levels = df_lesson_code$Lesson.ID)
  #df <- dplyr::arrange(df, name)
  
  df <- dplyr::select(df, c(Staff.Code, Week.Colour, Day, Lesson.ID, Period, Room,
                            Department, Subject.Name, Subject.Code,
                            Year.Group, Class, "Lesson.Start" = start, "Lesson.End" = end,
                            UPN:SEN))
  
  df <- dplyr::distinct(df)
  
  # Tidy multiple rooms
  df <- dplyr::group_by(df, Staff.Code, Lesson.ID, Class)
  df <- tidyr::nest(df, Room = c(Room))
  df <- dplyr::ungroup(df)
  df <- dplyr::distinct(df)
  df <- dplyr::rowwise(df) %>% dplyr::mutate(Room = toString(Room))
  df <- dplyr::ungroup(df)
  df <- dplyr::distinct(df)
  # Tidy room names
  df <- dplyr::mutate(df, Room = stringr::str_replace_all(Room, "c\\(", ""))
  df <- dplyr::mutate(df, Room = stringr::str_replace_all(Room, "\\)", ""))
  df <- dplyr::mutate(df, Room = stringr::str_replace_all(Room, "\"", ""))
  df <- dplyr::mutate(df, Room = stringr::str_replace_all(Room, "\\\\", ""))
  
  ## Return
  return(df)
}