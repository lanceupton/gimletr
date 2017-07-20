#' Welcome to gimletr!
#'
#' Print a welcome message.
#'
#' @param name Your name.
#' @return A welcome message addressed to \code{name}.
#' @examples
#' hello("Tyler")
#' hello("Big T")
hello <- function(name) {
  msg <- c(
    paste0("Welcome to gimletr, ", name, "!"),
    "This package will help you with all your Gimlet data needs!"
  )
  print(msg)
}
