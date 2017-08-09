#' Verify initials in Gimlet data.
#'
#' Verify initials in Gimlet data.
#'
#' @param x,id Character vectors.
#'
#' @return \code{TRUE} if all elements in \code{data} are contained within \code{id}. \code{FALSE} and character vector of unmatched elements in there are any.
#'
#' @examples
#' process.ids(data$Initials, idAccepted)
#'
#' @export process.ids

process.ids <- function(x, id) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(x)) {
    error("Please specify data.")
  }
  if(missing(id)) {
    error("Please specify accepted IDs.")
  }

  # Check argument classes
  if(!is.character(x)) {
    error("x must be character class.")
  }
  if(!is.character(id)) {
    error("id must be character class.")
  }

# Function ----------------------------------------------------------------

  # Index unmatched elements
  i <- which(!x %in% id)

  # If there are any,
  if(length(i) > 0) {

    # Print them
    message(cat("Unmatched IDs!", unique(x[i]), sep = "\n"))

    # View related observations
    View(data[i,])

    return(FALSE)

  } else {

    return(TRUE)

  }

}
