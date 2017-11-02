#' Verify tags in Gimlet data.
#'
#' Verify tags in Gimlet data.
#'
#' @param tags,promoted,regex Character vectors.
#'
#' @return A vector of processed tags.
#'
#' @examples
#' process.tags(tags = data$Tags, promoted = c('policy', 'circulation'))
#'
#' @export process.tags

process.tags <- function(tags, promoted, regex) {

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
  if(!missing(regex)) {
    if(!is.character(regex)) {
      stop('regex must be character class.')
    }
  }

# Function ----------------------------------------------------------------

  # Initiate a vector for output
  output <- character(length(tags))

  # If regex is not supplied,
  if(missing(regex)) {
    # Default
    regex <- paste0('^', promoted)
  }

  # For each index in promoted,
  for(n in 1:length(promoted)) {

    # Index matches
    i <- grep(x = tags, pattern = regex[n], ignore.case = TRUE)

    # If there are any,
    if(length(i) > 0) {
      # Assign tag
      output[i] <- promoted[n]
    }

  }

  # Return output
  output

}
