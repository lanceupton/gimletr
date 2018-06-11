#' Generate word clouds.
#'
#' Generate word clouds.
#'
#' @param ... Character vectors to include in the word cloud.
#' @param wc_opts A list of options to pass to \code{\link{wordcloud2}}.
#'
#' @return A png image of a wordcloud.
#'
#' @examples
#' gimlet.wc(data$question, data$answer, wc_opts = list(color = 'red'))
#'
#' @export gimlet.wc

gimlet.wc <- function(..., wc_opts = NULL) {

  # Combine vectors
  x <- tolower(c(...))

  # Remove punctuation
  x <- gsub(pattern = '[[:punct:]]', replacement = '', x = x)
  # Remove duplicate whitespace
  x <- gsub(pattern = ' +', replacement = ' ', x = x)

  # Split words
  x <- unlist(strsplit(x = x, split = ' '))

  # Remove NA
  x <- x[!is.na(x)]
  # Remove stopwords
  x <- x[!x %in% stopwords('english')]
  # Remove words <= 2 letters
  x <- x[!nchar(x) <= 2]

  # Generate a wordcloud
  do.call(
    what = wordcloud2,
    args = c(
      list(data = table(x)),
      wc_opts
    )
  )

}
