#' Prepare Gimlet data.
#'
#' Prepare Gimlet data.
#'
#' @param data A data frame with raw Gimlet data.
#' @param promoted_tags Optional. A character vector of 'promoted' tags, i.e., tags to retain.
#'
#' @return A data frame with prepared data.
#'
#' @examples
#' prep.gimlet(read.gimlet('mysite', 'e@mail.com', 'mypassword'))
#' prep.gimlet(mydata, c('policy', 'technology', 'ill', 'internal_directions'))
#'
#' @export gimlet.prep

gimlet.prep <- function(data, promoted_tags = NULL) {

  # HANDLE ARGUMENTS --------------------------------------------------------

  # Check argument classes
  assert_that(is.data.frame(data))
  if(!is.null(promoted_tags)) {
    assert_that(is.character(promoted_tags))
  }

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
  output <- data.frame(
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

  if(!is.null(promoted_tags)) {

    # Detach parsed tags
    tags <- strsplit(x = output$tags, split = ' ')

    # Remove non-promoted tags
    output$tags <- unlist(lapply(X = tags, FUN = function(t) {
      t <- t[t %in% promoted_tags]
      if(length(t) == 0) {t <- NA}
      # Deparse tags to re-attach
      paste(t, collapse = ' ')
    }))

  }

  # Return data
  return(output)

}
