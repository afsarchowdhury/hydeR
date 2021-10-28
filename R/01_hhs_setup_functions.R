# API documentation ---------------------------
#https://www.go4schools.com/Documentation/V1/APIDocumentation.html

# Set up ---------------------------
#' Run set up for gfs functions.
#'
#' Use this function to store your API key for use with hhs functions.
#' Authentication is required for all requests.  See your school's
#' administrator for an API key.
#' @param api_key API key as string.
#' @examples
#' hhs_setup("a96650481f0245eea396726f85ac7049")
#' @export
hhs_setup <- function(api_key = NULL) {
  if (is.null(api_key)) {
    g4sr::gfs_setup()
  } else {
    g4sr::gfs_setup(api_key = api_key)
  }
}

## School
#' Get school details.
#'
#' Returns details of the school and the available academic years.
#' @export
hhs_school <- function() {
  g4sr::gfs_school()
}
