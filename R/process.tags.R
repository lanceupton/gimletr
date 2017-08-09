#' Verify tags in Gimlet data.
#'
#' Verify tags in Gimlet data.
#'
#' @param x,tag Character vectors.
#'
#' @return A data frame containing processed data.
#'
#' @examples
#' process.tags(data$Tags, tags)
#'
#' @export process.tags

process.tags <- function(x, tags) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(x)) {
    stop("Please specify data.")
  }
  if(missing(tags)) {
    stop("Please specify accepted tags.")
  }

  # Check argument classes
  if(!is.character(x)) {
    stop("x must be character class.")
  }
  if(!is.character(tags)) {
    stop("tags must be character class.")
  }

# Function ----------------------------------------------------------------

  # Initiate a vector for output
  output <- rep("", length(x))

  # For each element in tags,
  for(n in tags) {

    # Index elements with n in the first position
    i <- grep(x = x, pattern = paste0("^", n), ignore.case = TRUE)

    # If there are any,
    if(length(i) > 0) {

      # Assign tag
      output[i] <- n

    }

  }

  # Return output
  output

}
