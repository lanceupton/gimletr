#' Download Gimlet data.
#'
#' Download data from a Gimlet site.
#'
#' @param site The name of the Gimlet subdomain ("site") to access.
#' @param email The email address for logging in to the Gimlet site.
#' @param password The password for logging in to the Gimlet site.
#' @param start_date,end_date Objects of class "POSIXt" or "Date".
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing the data returned by Gimlet in the download query.
#' @examples
#' read.gimlet("mylibrary", "email@gmail.com", "mypassword")
#' read.gimlet("mylibrary", "email@gmail.com", "mypassword", "2015-01-01", "2015-01-31")

read.gimlet <- function(site, email, password, start_date, end_date) {

  # Handle arguments

  if(missing(site)) {
    stop("Please specify a Gimlet site.")
  }
  if(!is.character(site) | length(site) > 1) {
    stop("site must be character class of length 1.")
  }
  if(grepl(pattern = "^([a-zA-Z0-9-])", x = site)) {
    stop("site must have a valid name.\n",
         "Acceptable characters are: a-z, A-Z, 0-0, and -.")
  }

  if(missing(email)) {
    stop("Please specify a login email for the Gimlet site.")
  }
  if(!is.character(email) | length(email) > 1) {
    stop("email must be character class of length 1.")
  }

  if(missing(password)) {
    stop("Please specify a login password for the Gimlet site.")
  }
  if(!is.character(password) | length(password) > 1) {
    stop("password must be character class of length 1.")
  }

  # TO DO: Add handlers for start_date and end_date

}
