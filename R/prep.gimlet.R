#' Pre-process Gimlet data.
#'
#' Pre-process Gimlet data.
#'
#' @param data A data frame with raw Gimlet data.
#'
#' @return A data frame with pre-processed data.
#'
#' @examples
#' prep.gimlet(data = read.gimlet('site', 'e@mail.com', 'pass'))
#'
#' @export prep.gimlet

prep.gimlet <- function(data) {

# Handle Arguments --------------------------------------------------------

  # Return error for missing arguments
  if(missing(data)) {
    stop('Please specify data.')
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    stop('data must be a data frame.')
  }

  # Vector of names from a Gimlet data query to be pre-processed
  names <- c('Initials', 'Location', 'Format', 'Asked at', 'Tags', 'Question', 'Answer')
  # Index data names not in names
  i <- which(!names %in% names(data))
  # If not all names exist,
  if(length(i)) {
    stop(paste('Important variables missing from data!', paste(names[i], collapse = '\n'), sep = '\n'))
  }


# Function ----------------------------------------------------------------

  # Select data
  output <- data.frame(
    initials = gsub(pattern = '[^[:alpha:]]', replacement = '', x = data$Initials),
    location = data$Location,
    format   = data$Format,
    datetime = data$'Asked at',
    tagpre   = data$Tags,
    question = data$Question,
    answer   = data$Answer,
    stringsAsFactors = FALSE
  )

  # For each variable in data,
  for(n in 1:length(output)) {

    # Attach
    x <- output[,n]
    # Remove trailing/leading whitespace
    x <- gsub(pattern = '^ +', replacement = '', x = x)
    # Remove duplicate whitespace
    x <- gsub(pattern = ' +', replacement = ' ', x = x)
    # Lower case
    x <- tolower(x = x)
    # Index blank or NA obs
    i <- union(which(x == ''), which(is.na(x)))
    # Label them with BLANK
    if(length(i)) {x[i] <- 'BLANK'}
    # Detach
    output[,n] <- x

  }

  # Return output
  output

}
