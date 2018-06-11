#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param initials A character vector or factor denoting groups to compute errors for.
#' @param tagpre A character vector or factor denoting the submitted tags.
#' @param tagpost A character vector or factor denoting the corrected tags.
#'
#' @return A data frame containing \code{ent}, \code{err}, \code{per}, and \code{score}, grouped by \code{initials}.
#'
#' @examples
#' compute.errors(data$initials, tagpre = data$tagpre, tagpost = data$tagpost)
#'
#' @export gimlet.errors

gimlet.errors <- function(initials, tagpre, tagpost) {

# HANDLE ARGUMENTS --------------------------------------------------------

  assert_that(
    is.character(initials) | is.factor(initials),
    is.character(tagpre) | is.factor(tagpre),
    is.character(post) | is.factor(tagpost),
    length(initials) == length(tagpre),
    length(initials) == length(tagpost)
  )

# FUNCTION ----------------------------------------------------------------

  # Detach parsed tags
  tagpre <- strsplit(x = tagpre, split = ' ')
  tagpost <- strsplit(x = tagpost, split = ' ')

  # Index observations where list of tags do not match
  errors <- mapply(tagpre, tagpost, FUN = function(pre, post) {
    !setequal(pre, post)
  })

  # Tabulate results
  errors <- data.frame(table(initials, errors))
  errors <- reshape(
    data = errors,
    idvar = 'initials',
    timevar = 'errors',
    direction = 'wide'
  )
  names(errors) <- c('initials', 'err', 'cor')

  # Add some variables
  errors$ent <- errors$err + errors$cor
  errors$per <- errors$cor / errors$ent * 100
  errors$score <- paste0(
    errors$cor, '/', errors$ent, ' (', round(errors$per, 0), '%)'
  )

  # Return data
  errors

}