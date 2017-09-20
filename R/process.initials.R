#' Verify initials in Gimlet data.
#'
#' Verify initials in Gimlet data.
#'
#' @param initials,users Character vectors.
#'
#' @return \code{TRUE} if all elements in \code{initials} are contained in \code{users}. \code{FALSE} otherwise.
#'
#' @examples
#' process.ids(initials = data$Initials, users = users)
#'
#' @export process.initials

process.initials <- function(initials, users) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(initials)) {
    stop('Please specify initials.')
  }
  if(missing(users)) {
    stop('Please specify users.')
  }
  # Check argument classes
  if(!is.character(initials)) {
    stop('initials must be character class.')
  }
  if(!is.character(users)) {
    stop('users must be character class.')
  }

# Function ----------------------------------------------------------------

  # Index unmatched elements
  i <- which(!initials %in% users)

  # If there are any,
  if(length(i) > 0) {

    # Print them
    message(cat('Unmatched IDs!', unique(initials[i]), sep = '\n'))

    return(FALSE)

  } else {

    return(TRUE)

  }

}
