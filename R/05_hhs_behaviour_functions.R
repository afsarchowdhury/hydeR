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

  message(cat(crayon::silver("Merge datasets")))

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

  message(cat(crayon::silver("Compute metadata")))

  ## Create
  df$Surname.Forename.Reg <- paste0(toupper(df$preferred_last_name), " ", df$preferred_first_name, " (", df$registration_group, ")")

  message(cat(crayon::silver("Clean final output")))

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

  message(cat(crayon::silver("Lookup student demographic data")))

  ## Student demographics
  df_stu <- hhs_student_details_general(academicYear)
  df_stu <- dplyr::select(df_stu, c(GFSID, Ethnicity:ncol(df_stu)))
  df <- dplyr::left_join(df, df_stu, by = c("GFSID"))

  ## Return
  return(df)
}


## Clean school exclusion data
#' Get clean school exclusion data.
#'
#' Returns clean school exclusion data for chosen academic year and date range,
#' along with a line plot.
#' @importFrom magrittr "%>%"
#' @examples
#' hhs_exclusions(2022, "2021-09-01", "2021-09-30")
#' @export
hhs_exclusions <- function(academicYear, goDateStart, goDateEnd) {
  ## Mesaage
  message(cat(crayon::cyan("Generating clean exclusion data for", goDateStart, "to", goDateEnd)))

  ## Import school information
  my_school <- g4sr::gfs_school()
  my_school_name <- my_school$name
  my_school_years <- my_school$academic_years
  my_school_years_current <- my_school$current_academic_year

  ## Import student data
  df_student_details <- hhs_student_details_general(academicYear = academicYear)
  df_student_details$Year.Group <- factor(df_student_details$Year.Group,
                                          levels = c("7", "8", "9", "10", "11"))

  ## Import attendance data
  df_student_attendance_session <- hhs_attendance_student_session_range(
    academicYear = academicYear,
    goDateStart = goDateStart,
    goDateEnd = goDateEnd
  )
  df_student_attendance_session$Year.Group <- factor(df_student_attendance_session$Year.Group,
                                                     levels = c("7", "8", "9", "10", "11"))

  message(cat(crayon::silver("Clean data")))

  ## Filter exclusions
  df_exclusions <- df_student_attendance_session %>%
    dplyr::filter(Session.Mark == "E") %>%
    dplyr::mutate(Date = lubridate::as_date(Date)) %>%
    dplyr::group_by(UPN) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  message(cat(crayon::silver("Merge datasets")))

  ## Merge
  df <- dplyr::left_join(df_exclusions, df_student_details, by = c("UPN"))

  message(cat(crayon::silver("Generate plot")))

  ## Plot
  p <- df_student_attendance_session %>%
    dplyr::filter(Session.Mark == "E") %>%
    dplyr::mutate(Date = lubridate::as_date(Date)) %>%
    dplyr::group_by(Year.Group, Date, UPN, Surname.Forename.Reg) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Date) %>%
    dplyr::group_by(Year.Group) %>%
    dplyr::mutate(Cum.Sum = cumsum(n)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = Date, y = Cum.Sum)) +
    ggplot2::geom_step(lwd = 1) +
    ggplot2::geom_point(ggplot2::aes(colour = Year.Group), size = 4) +
    #geom_point(col = "olivedrab3", size = 4) +
    #scale_y_continuous(breaks = seq(0, 100, 2)) +
    ggplot2::scale_x_date(
      expand = c(0, 0.5),
      labels = scales::label_date_short(format = c("%Y", "%b", "%d"))#,
      #breaks = scales::breaks_width("2 days")
    ) +
    ggplot2::labs(x = NULL, y = "Count of sessions",
                  title = "School exclusions",
                  subtitle = paste0("Cumulative count of sessions vs. time, faceted by year group.",
                                    "\n", goDateStart, " to ", goDateEnd, "."),
                  caption = paste0(my_school_name, " | Data retrieved using R package g4sr on ", Sys.Date())) +
    ggplot2::facet_wrap(~Year.Group, nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none"#,
      #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1/3)
    )

  ## Return
  return(list(plot = p, data = df))
}
