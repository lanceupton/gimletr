#' Download Gimlet data.
#'
#' Download data from a Gimlet site.
#'
#' @param site The name of the Gimlet subdomain ("site") to access.
#' @param email The email address for logging in to the Gimlet site.
#' @param password The password for logging in to the Gimlet site.
#' @param start_date,end_date Optional. Objects of class "POSIXt" or "Date".
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing the data returned by Gimlet in the download query.
#' @examples
#' read.gimlet("mysite", "e@mail.com", "mypassword")
#' read.gimlet("mysite", "e@mail.com", "mypassword", "2015-01-01", "2015-01-31")
#'
#' @export read.gimlet

read.gimlet <- function(site, email, password, start_date, end_date) {

# Handle Arguments --------------------------------------------------------

  # Throw error if required arguments missing
  if(missing(site)) {
    stop("Please specify a Gimlet site.")
  }
  if(missing(email)) {
    stop("Please specify a login email for the Gimlet site.")
  }
  if(missing(password)) {
    stop("Please specify a login password for the Gimlet site.")
  }
  if(missing(start_date) + missing(end_date) == 1) {
    stop("Please specify both start_date and end_date, or neither.")
  }

  # Handle report dates
  # If both are missing
  if(missing(start_date) + missing(end_date) == 2) {
    # Set report to last 30 days
    start_date <- Sys.Date() - 30
    end_date <- Sys.Date()
    # Message user about default values
    message(
      "No start_date or end_date given.\n",
      "Using ", start_date, " and ", end_date
    )
  }
  # If both are defined
  if(missing(start_date) + missing(end_date) == 0) {
    # Format them as Gimlet parameters
    start_date <- format(as.Date(start_date), format = "%Y-%m-%d")
    end_date <- format(as.Date(end_date), format = "%Y-%m-%d")
  }

  # Check argument classes
  if(!is.character(site) | length(site) > 1) {
    stop("site must be character class of length 1.")
  }
  if(grepl(pattern = "[^[:alnum:]|-]", x = site)) {
    stop("site must have a valid name.\n",
         "Acceptable characters are: a-z, A-Z, 0-9, and -.")
  }
  if(!is.character(email) | length(email) > 1) {
    stop("email must be character class of length 1.")
  }
  if(!is.character(password) | length(password) > 1) {
    stop("password must be character class of length 1.")
  }


# Function ----------------------------------------------------------------

  # Generate a Gimlet URL based on site
  url <- paste0("https://", site, ".gimlet.us/users/login")

  # Define the html session
  session <- html_session(url)

  # Login algorithm
  login <- session %>%
    html_node("form") %>%
    html_form() %>%
    set_values(
      email = email,
      password = password
    )

  # Submit the form
  login <- session %>%
    submit_form(login)

  # Data query algorithm
  data_query <- login %>%
    follow_link("Reports") %>%
    follow_link("Download") %>%
    html_node("form") %>%
    html_form() %>%
    set_values(
      'report[start_date]' = start_date,
      'report[end_date]' = end_date
    )

  # Submit a data query
  response <- session %>% submit_form(data_query)

  # Return the parsed response content
  content(response$response, "parsed")

}
