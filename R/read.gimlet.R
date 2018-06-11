#' Download Gimlet data.
#'
#' Download Gimlet data.
#'
#' @param site A string denoting the Gimlet subdomain 'site', e.g., YOURLIBRARY.gimlet.us.
#' @param email A string denoting the email to login to your Gimlet site.
#' @param password A string denoting the password to login to your Gimlet site.
#' @param start_date,end_date Optional. Object of class 'POSIXt' or 'Date' denoting the desired start and end date of the data query. Defaults to past 7 days.
#'
#' @return A data frame containing the contents of a Gimlet data query.
#'
#' @examples
#' read.gimlet('mysite', 'e@mail.com', 'mypassword')
#'
#' @export read.gimlet

read.gimlet <- function(site, email, password, start_date, end_date) {

  # HANDLE ARGUMENTS --------------------------------------------------------

  # Check argument classes
  assert_that(
    is.string(site),
    is.string(email),
    is.string(password)
  )

  # Handle start_date and end_date
  missing_dates <- missing(start_date) + missing(end_date)
  # One is missing
  if(missing_dates == 1) {
    stop('Please specify both start_date and end_date, or neither.')
  }
  # Both are missing
  if(missing_dates == 2) {
    # Set default values
    start_date <- Sys.Date() - 7
    end_date <- Sys.Date()
    # Message user
    message('gimletr: start_date and end_date missing. Defaulting to last 7 days.')
  }
  # Neither are missing
  if(missing_dates == 0) {
    # Format variables
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    # If unordered
    if(start_date > end_date) {
      stop('start_date occurs after end_date.')
    }
  }

  # FUNCTION ----------------------------------------------------------------

  # Generate base site URL
  base_url <- paste0('https://', site, '.gimlet.us')

  # Generate a Gimlet login URL
  login_url <- parse_url(base_url)
  login_url$path <- 'users/login'

  # Generate a Gimlet report URL
  report_url <- parse_url(base_url)
  report_url$path <- 'reports/data_dump'
  report_url$query <- list(
    `report[start_date]` = start_date,
    `report[end_date]` = end_date
  )

  # Initialize a curl handle with cookies
  curl <- getCurlHandle(cookiefile = '')

  # POST login form
  login <- suppressWarnings(
    postForm(
      uri = build_url(login_url),
      .params = list(
        email = email,
        password = password
      ),
      curl = curl
    )
  )

  # GET report data and parse it
  response <- getURL(build_url(report_url), curl = curl)
  response <- read.csv(textConnection(response), header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

  # Success message
  message('gimletr: Succesfully fetched ', nrow(response), ' observations.')

  # Return data
  return(response)

}
