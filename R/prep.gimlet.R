#' Pre-process Gimlet data.
#'
#' Pre-processes Gimlet data.
#'
#' @param data A data frame (\code{\link[base]{data.frame}}) with Gimlet variables names.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing pre-processed data.
#'
#' @examples
#' prep.gimlet(read.gimlet("site", "e@mail.com", "pass"))
#'
#' @export prep.gimlet

prep.gimlet <- function(data) {

# Handle Arguments --------------------------------------------------------

  # Throw error if required arguments missing
  if(missing(data)) {
    error("Please specify data.")
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    error("data must be a data frame.")
  }

  # Check names
  x <- c("Initials", "Location", "Format", "Asked at", "Tags", "Question", "Answer")
  if(!all(x %in% names(data))) {
    msg <- paste("names(data) must include:", paste(x, collapse = "\n"), sep = "\n")
    stop(msg)
  }


# Function ----------------------------------------------------------------

  # Select data
  data <- data.frame(
    Id       = data$Initials,
    Location = data$Location,
    Format   = data$Format,
    DateTime = data$'Asked at',
    TagPre   = data$Tags,
    TagPost  = "",
    Question = data$Question,
    Answer   = data$Answer,
    stringsAsFactors = FALSE
  )

  # Format data
  for(n in 1:length(data)) {
    x <- data[,n]
    # Remove trailing/leading/duplicate whitespace
    x <- gsub(pattern = "^ +", replacement = "", x = x)
    x <- gsub(pattern = " +", replacement = " ", x = x)
    # Lower case
    x <- tolower(x = x)
    # Label blank observations
    i <- which(data[,n] == "")
    if(length(i) > 0) {data[i,n] <- "BLANK"}
    data[,n] <- x
  }

  # Remove bad characters from Id variable
  data$Id <- gsub(pattern = "[[:punct:]]|[[:space:]]|[[:digit:]]", replacement = "", x = data$Id)

  # Return pre-processed data
  data

}
