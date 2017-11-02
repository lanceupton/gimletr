#' Download Gimlet data.
#'
#' Download Gimlet data.
#'
#' @param site,email,password Strings.
#' @param start_date,end_date Optional. Objects of class 'POSIXt' or 'Date'.
#'
#' @return A data frame containing the content of a Gimlet data query.
#'
#' @examples
#' read.gimlet('mysite', 'e@mail.com', 'mypassword')
#' read.gimlet('mysite', 'e@mail.com', 'mypassword', as.Date('2010-01-01'), as.Date('2010-01-31'))
#'
#' @export read.gimlet

read.gimlet <- function(site, email, password, start_date, end_date) {

# Handle Arguments --------------------------------------------------------

  # Return for missing arguments
  if(missing(site)) {
    stop('Please specify a Gimlet site.')
  }
  if(missing(email)) {
    stop('Please specify a login email.')
  }
  if(missing(password)) {
    stop('Please specify a login password.')
  }

  # Handle start_date and end_date
  dates <- missing(start_date) + missing(end_date)
  # One is missing
  if(dates == 1) {
    stop('Please specify both start_date and end_date, or neither.')
  }
  # Both are missing
  if(dates == 2) {
    # Set default values
    start_date <- Sys.Date() - 7
    end_date <- Sys.Date()
    # Message user
    message(
      'start_date and end_date missing. Defaulting to last 7 days.\n',
      start_date, ' - ', end_date
    )
  }
  # Neither are missing
  if(dates == 0) {
    # Format variables
    start_date <- format(as.Date(start_date))
    end_date <- format(as.Date(end_date))
    # If unordered
    if(start_date > end_date) {
      stop('start_date must occur before end_date.')
    }
  }

  # Check argument classes
  if(!is.character(site) | length(site) != 1) {
    stop('site must be character class of length 1.')
  }
  if(grepl(pattern = '[^[:alnum:]|-]', x = site, ignore.case = TRUE)) {
    stop('site must have a valid name.\nAcceptable characters are: a-z, A-Z, 0-9, and -.')
  }
  if(!is.character(email) | length(email) != 1) {
    stop('email must be character class of length 1.')
  }
  if(!is.character(password) | length(password) != 1) {
    stop('password must be character class of length 1.')
  }

# Function ----------------------------------------------------------------

  ## LOGIN

  # Generate a Gimlet login URL
  url <- paste0('https://', site, '.gimlet.us/users/login')
  # Start a session
  session <- html_session(url = url)
  # Get form and fill in values
  login <- session %>%
    html_node('form') %>%
    html_form() %>%
    set_values(
      email = email,
      password = password
    )
  # Submit login form
  login <- suppressMessages(
    session %>%
      submit_form(form = login)
  )

  ## GET DATA
  data_query <- suppressMessages(
    login %>%
      # Navigate to download page
      follow_link('Reports') %>%
      follow_link('Download') %>%
      # Get form and fill in values
      html_node('form') %>%
      html_form() %>%
      set_values(
        'report[start_date]' = start_date,
        'report[end_date]' = end_date
      )
  )
  # Submit data query form
  response <- suppressMessages(
    session %>% submit_form(data_query)
  )

  # Return the parsed response content
  suppressMessages(content(x = response$response, as = 'parsed'))

}
