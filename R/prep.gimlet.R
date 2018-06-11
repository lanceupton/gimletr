#' Pre-process Gimlet data.
#'
#' Pre-process Gimlet data.
#'
#' @param data A data frame with raw Gimlet data.
#'
#' @return A data frame with pre-processed data.
#'
#' @examples
#' prep.gimlet(data = read.gimlet('mysite', 'e@mail.com', 'mypassword'))
#'
#' @export prep.gimlet

prep.gimlet <- function(data) {

# HANDLE ARGUMENTS --------------------------------------------------------

  # Check argument classes
  assert_that(
    is.data.frame(data)
  )

  # Vector of required variables
  vars_req <- c(
    'Initials', 'Location', 'Format', 'Asked.by', 'Question.type',
    'Asked.at', 'Tags', 'Question', 'Answer'
  )

  # Determine which variables are not supplied
  vars_missing <- vars_req[!vars_req %in% names(data)]

  # Require that all variables are supplied
  assert_that(
    length(vars_missing) == 0,
    msg = paste(
      'Additional variables required:',
      paste(vars_missing, collapse = ', ')
    )
  )

# FUNCTION ----------------------------------------------------------------

  # Subset and format data
  data.frame(
    datetime     = as.POSIXct(data$Asked.at),
    initials     = factor(tolower(gsub(
      pattern = '[^[:alpha:]]', replacement = '', x = data$Initials
    ))),
    location     = factor(tolower(data$Location)),
    format       = factor(tolower(data$Format)),
    patron_group = factor(tolower(data$Asked.by)),
    category     = factor(tolower(data$Question.type)),
    tags         = tolower(data$Tags),
    question     = tolower(data$Question),
    answer       = tolower(data$Answer),
    stringsAsFactors = FALSE
  )

}
