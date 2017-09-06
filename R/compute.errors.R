#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param data A data frame containing \code{Id}, \code{TagPre}, and \code{TagPost}.
#' @param groups Optional. A character vector.
#' @param ignore Optional. An integer vector.
#'
#' @return A data frame containing \code{NumEnt}, \code{NumErr}, \code{PerErr}, and \code{Score} for each \code{Id}. If \code{groups} is specified, group errors are also computed. If \code{ignore} is specified, those indices are ignored when calculating \code{NumErr}.
#'
#' @examples
#' compute.errors(data = data, groups = groupnames, ignore = ignore_these_errors)
#'
#' @export compute.errors

compute.errors <- function(data, groups, ignore) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(data)) {
    stop("Please specify data.")
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  if(!missing(ignore) & !is.integer(ignore)) {
    stop("ignore must be a vector of integers.")
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
  # Remove errors from ignored index
  if(missing(ignore) | length(ignore) == 0) {
    i.error <- which(data$TagPre != data$TagPost)
  } else {
    i.error <- intersect(which(data$TagPre != data$TagPost), (1:nrow(data))[-ignore])
  }

  # For each row in output,
  for(n in 1:nrow(output)) {

    # Index obs for Id
    i <- which(data$Id == output[n,"Id"])

    # Count entries
    ent <- length(i)

    # Count errors
    err <- length(intersect(i.error, i))

    # Calculate percent error
    per <- (1 - err/ent) * 100

    # Generate a score
    if(ent == 0) {
      score <- "---"
    } else {
      score <- paste0(ent - err, "/", ent, " | ", round(per, 0), "%")
    }

    # Write numeric
    output[n,c("NumEnt", "NumErr", "PerErr")] <- c(ent, err, round(per, 2))
    # Write character
    output[n,"Score"] <- score

  }

  # Add total
  ent <- sum(output$NumEnt)
  err <- sum(output$NumErr)
  per <- (1 - err/ent) * 100
  score <- paste0(ent - err, "/", ent, " | ", round(per, 0), "%")
  # Write numeric
  output[n+1,c("NumEnt", "NumErr", "PerErr")] <- c(ent, err, round(per, 2))
  # Write character
  output[n+1,c("Id","Score")] <- c("total", score)

  # Add groups (if specified)
  if(!missing(groups)) {

    # Initiate a data frame of data to add
    add <- data.frame(
      Id     = "",
      NumEnt = 0,
      NumErr = 0,
      PerErr = 0,
      Score  = "",
      stringsAsFactors = FALSE
    )

    # Vector of unique groups
    g <- unique(groups)

    # For each group,
    for(n in 1:length(g)) {
      # Index group
      i <- which(groups == g[n])
      # Get values
      ent <- length(i)
      err <- length(intersect(i, i.error))
      per <- (1 - err/ent) * 100
      score <- paste0(ent - err, "/", ent, " | ", round(per, 0), "%")

      # Write numeric
      add[n,c("NumEnt", "NumErr", "PerErr")] <- c(ent, err, round(per, 2))
      # Write character
      add[n,c("Id","Score")] <- c(g[n], score)
    }

    # Bind to output
    output <- rbind(output, add)

  }

  output

}
