#' Compute user tagging errors.
#'
#' Compute user tagging errors.
#'
#' @param data A data frame containing character variables \code{`group_by`}, \code{tagpre}, and \code{tagpost}.
#' @param group_by Default 'initials'. A string denoting the grouping variable.
#'
#' @return A data frame containing \code{err} (number errors), \code{cor} (number correct), \code{ent} (number entered), \code{per} (percent correct), and \code{score} (score term), grouped by \code{group_by}.
#'
#' @examples
#' compute.errors(data)
#'
#' @export gimlet.errors

gimlet.errors <- function(data, group_by = 'initials') {

  # HANDLE ARGUMENTS --------------------------------------------------------

  # Check argument classes
  assert_that(
    is.data.frame(data),
    is.string(group_by)
  )

  # Vector of required variables
  vars_req <- c(group_by, 'tagpre', 'tagpost')

  # Determine which variables are not supplied
  vars_missing <- vars_req[!vars_req %in% names(data)]

  # Require that all variables are supplied
  assert_that(
    length(vars_missing) == 0,
    msg = paste('Additional variables required:', vars_missing, collapse = '\n')
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
  errors <- data.frame(table(data[[group_by]], errors))
  errors <- reshape(
    data = errors,
    idvar = 'Var1',
    timevar = 'errors',
    direction = 'wide'
  )
  names(errors) <- c(group_by, 'cor', 'err')

  # Add total
  errors[[group_by]] <- as.character(errors[[group_by]])
  errors <- rbind(errors, list('total', sum(errors$cor), sum(errors$err)))

  # Add some variables
  errors$ent <- errors$err + errors$cor
  errors$per <- errors$cor / errors$ent * 100
  errors$score <- paste0(
    errors$cor, '/', errors$ent, ' (', round(errors$per, 0), '%)'
  )

  # Return data
  errors

}
