#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param data A data frame containing \code{Id}, \code{TagPre}, and \code{TagPost}.
#'
#' @return A data frame containing \code{NumEnt}, \code{NumErr}, \code{PerErr}, and \code{Score} for each \code{Id}.
#'
#' @examples
#' compute.errors(data)
#'
#' @export compute.errors

compute.errors <- function(data) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(data)) {
    stop("Please specify data.")
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

# Function ----------------------------------------------------------------

  # Initiate a data frame for output
  output <- data.frame(
    Id     = unique(data$Id),
    NumEnt = 0,
    NumErr = 0,
    PerErr = 0,
    Score  = "",
    stringsAsFactors = FALSE
  )

  # Index observations where tag was incorrect
  i.error <- which(data$TagPre != data$TagPost)

  # For each row in output,
  for(n in 1:nrow(output)) {

    # Index obs for Id
    i <- which(data$Id == output[n,"Id"])

    # Count entries
    ent <- length(i)

    # Count errors
    err <- length(intersect(i.error, i))

    # Calculate percent error
    per <- round((1 - err/ent) * 100, 0)

    # Generate a score
    if(ent == 0) {
      score <- "---"
    } else {
      score <- paste0(ent - err, "/", ent, " | ", per, "%")
    }

    # Write numbers
    output[n,c("NumEnt", "NumErr", "PerErr", "Score")] <- c(ent, err, per, score)

  }

  # Format and return output
  output <- data.frame(
    NumEnt = as.integer(output$NumEnt),
    NumErr = as.integer(output$NumErr),
    PerErr = as.integer(output$PerErr),
    Score  = as.character(output$SCore),
    stringsAsFactors = FALSE
  )

}
