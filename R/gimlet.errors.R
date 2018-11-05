#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param data A data frame containing character variables \code{initials}, \code{tagpre}, and \code{tagpost}.
#'
#' @return A data frame containing \code{err} (number errors), \code{cor} (number correct), \code{ent} (number entered), \code{per} (percent correct), and \code{score} (score term), grouped by \code{initials}.
#'
#' @examples
#' compute.errors(data)
#'
#' @export gimlet.errors

gimlet.errors <- function(data) {

  # HANDLE ARGUMENTS --------------------------------------------------------

  # Check argument classes
  assert_that(is.data.frame(data))

  # Vector of required variables
  vars_req <- c('initials', 'tagpre', 'tagpost')

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

  # Detach parsed tags
  tagpre <- strsplit(x = as.character(data$tagpre), split = ' ')
  tagpost <- strsplit(x = as.character(data$tagpost), split = ' ')

  # Index observations where list of tags do not match
  errors <- mapply(tagpre, tagpost, FUN = function(pre, post) {
    !setequal(pre, post)
  })

  # Tabulate results
  errors <- data.frame(table(data$initials, errors))
  errors <- reshape(
    data = errors,
    idvar = 'Var1',
    timevar = 'errors',
    direction = 'wide'
  )
  names(errors) <- c('initials', 'cor', 'err')

  # Add some variables
  errors$ent <- errors$err + errors$cor
  errors$per <- errors$cor / errors$ent * 100
  errors$score <- paste0(
    errors$cor, '/', errors$ent, ' (', round(errors$per, 0), '%)'
  )

  # Return data
  errors

}
