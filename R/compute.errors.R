#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param initials,tagpre,tagpost Character vectors.
#' @param groups Optional. A character vector.
#' @param ignore Optional. An integer vector.
#'
#' @return A data frame containing \code{ent}, \code{err}, \code{per}, and \code{score} by each level in \code{initials}. If \code{groups} is specified, group errors are also computed. If \code{ignore} is specified, those indices are ignored when calculating \code{err}.
#'
#' @examples
#' compute.errors(initials = data$initials, tagpre = data$tagpre, tagpost = data$tagpost, groups = data$group)
#'
#' @export compute.errors

compute.errors <- function(initials, tagpre, tagpost, groups, ignore) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(initials)) {
    stop('Please specify initials.')
  }
  if(missing(tagpre)) {
    stop('Please specify tagpre.')
  }
  if(missing(tagpost)) {
    stop('Please specify tagpost.')
  }
  # Check argument classes
  if(!is.character(initials)) {
    stop('intitials must be character class.')
  }
  if(!is.character(tagpre)) {
    stop('tagpre must be character class.')
  }
  if(!is.character(tagpost)) {
    stop('tagpost must be character class.')
  }
  if(!missing(ignore)) {
    if(!is.integer(ignore)) {
      stop('ignore must be a vector of integers.')
    }
  }

  # Vector of variable lengths
  x <- c(length(initials), length(tagpre), length(tagpost))
  # If groups is specified, add it
  if(!missing(groups)) {x <- c(x, length(groups))}
  # Check equal lengths
  if(!all(x == x[1])) {
    stop('variables must be of same length.')
  }


# Function ----------------------------------------------------------------

  # Initiate a data frame for output
  output <- data.frame(
    initials = unique(initials),
    group    = '',
    ent      = 0,
    err      = 0,
    per      = 0,
    score    = '',
    stringsAsFactors = FALSE
  )

  # Fill in group
  output$group <- groups[match(output$initials, initials)]

  # Index observations where tag was incorrect
  i.error <- which(tagpre != tagpost)
  # Remove errors from ignored index
  if(!missing(ignore)) {
      i.error <- i.error[!i.error %in% ignore]
  }

  # For each row in output,
  for(n in 1:nrow(output)) {

    # Index obs for user
    i <- which(initials == output$initials[n])

    # Count entries
    ent <- length(i)
    # Count errors
    err <- length(intersect(i.error, i))
    # Calculate percent
    per <- (1 - err / ent) * 100
    # Generate a score
    if(ent == 0) {
      score <- '---'
    } else {
      score <- paste0(ent - err, '/', ent, ' \\textbf{', round(per, 0), '}')
    }

    # Write variables
    output$ent[n] <- ent
    output$err[n] <- err
    output$per[n] <- round(per, 2)
    output$score[n] <- score

  }

  # Generate totals
  total <- tibble(
    initials = 'total',
    group    = 'total',
    ent      = sum(output$ent),
    err      = sum(output$err),
    per      = (1 - err / ent) * 100,
    score    = paste0(ent - err, '/', ent, ' \\textbf{', round(per, 0), '}')
  )

  # Bind
  output <- rbind(output, total)


# Handle Groups -----------------------------------------------------------

  if(!missing(groups)) {

    # For each group,
    for(n in unique(groups)) {
      # Index group
      i <- which(groups == n)

      # Generate totals
      total <- tibble(
        initials = 'total',
        group    = n,
        ent      = length(i),
        err      = length(intersect(i, i.error)),
        per      = (1 - err / ent) * 100,
        score    = paste0(ent - err, '/', ent, ' \\textbf{', round(per, 0), '}')
      )

      # Bind
      output <- rbind(output, total)

    }

  }

  output

}
