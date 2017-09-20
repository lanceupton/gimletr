#' Verify tags in Gimlet data.
#'
#' Verify tags in Gimlet data.
#'
#' @param tags,promoted,alt Character vectors.
#'
#' @return A vector of processed tags.
#'
#' @examples
#' process.tags(tags = data$Tags, promoted = c('policy', 'circulation'))
#' process.tags(tags = data$Tags, promoted = c('policy', 'circulation'), alt = list(policy = c('policies', 'hours'), circulation = c('circ', 'ill', 'borrowing')))
#'
#' @export process.tags

process.tags <- function(tags, promoted, alt) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(tags)) {
    stop('Please specify tags.')
  }
  if(missing(promoted)) {
    stop('Please specify promoted.')
  }
  # Check argument classes
  if(!is.character(tags)) {
    stop('tags must be character class.')
  }
  if(!is.character(tags)) {
    stop('promoted must be character class.')
  }
  if(!missing(alt)) {
    if(!is.list(alt)) {
      stop('alt must be a list.')
    }
  }

# Function ----------------------------------------------------------------

  # Initiate a vector for output
  output <- rep('', length(tags))

  # For each promoted tag,
  for(n in promoted) {

    # Index first-position matches
    i <- grep(x = tags, pattern = paste0('^', n), ignore.case = TRUE)

    # If there are any,
    if(length(i) > 0) {

      # Assign tag
      output[i] <- n

    }

  }

  # If alt is supplied,
  if(!missing(alt)) {

    # Loop through each element in alt
    for(n in alt) {

      # Generate the regular expression
      expr <- paste0('^(', paste0(n, collapse = '|'), ')')

      # Index first-position matches
      i <- grep(x = tags, pattern = expr, ignore.case = TRUE)

      # If there are any,
      if(length(i) > 0) {

        # Assign tag
        output[i] <- names(n)

      }

    }

  }

  # Return output
  output

}
