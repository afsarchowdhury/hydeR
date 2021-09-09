# Clean school functions ---------------------------

## Clean school population
#' Get clean school population data.
#'
#' Returns clean details of school population for all available academic years,
#' along with a line plot.
#' @examples
#' hhs_school_population()
#' @export
#' @importFrom magrittr "%>%"
hhs_school_population <- function() {
  ## Mesaage
  message(cat(crayon::cyan("Generating clean school population data")))

  ## School details
  my_school <- g4sr::gfs_school()

  ## Define variables
  my_school_name <- my_school$name
  my_school_years <- my_school$academic_years
  my_school_years_current <- my_school$current_academic_year

  ## Loop student details for all available academic years
  df_student_details_loop_01 <- lapply(my_school_years, function(i) g4sr::gfs_student_details(academicYear = i))

  ## Count per year
  df_summary_academic_year <- data.frame(
    Academic.Year = my_school_years,
    n = sapply(df_student_details_loop_01, nrow)
  )

  ## Plot
  p <- ggplot2::ggplot(df_summary_academic_year, ggplot2::aes(x = Academic.Year, y = n)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Academic year", y = "School population",
                  title = paste0(my_school_name),
                  subtitle = paste0("School population vs. academic year from ", my_school_years[[length(my_school_years)]],
                                    " to ", my_school_years_current, "."))

  ## Return
  return(list(plot = p, data = df_summary_academic_year))
}
