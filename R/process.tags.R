#' Verify tags in Gimlet data.
#'
#' Verify tags in Gimlet data.
#'
#' @param data A data frame (\code{\link[base]{data.frame}}) with Gimlet-provided variable names.
#' @param tags A character vector of promoted tags.
#'
#' @return A data frame (\code{\link[base]{data.frame}}) containing processed and verified tags.
#'
#' @examples
#' process.tags(prep.gimlet(read.gimlet("mylibrary", "e@mail.com", "password")), tags)
#'
#' @export process.tags

process.tags <- function(data, tags) {

# Handle Arguments --------------------------------------------------------

  # Throw error if required arguments missing
  if(missing(data)) {
    error("Please specify data.")
  }
  if(missing(tags)) {
    error("Please specify accepted tags.")
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    error("data must be a data frame.")
  }

  # Check names
  x <- c("Id", "Location", "Format", "DateTime", "TagPre", "TagPost", "Question", "Answer")
  if(!all(x %in% names(data))) {
    msg <- paste("names(data) must include:", paste(x, collapse = "\n"), sep = "\n")
    stop(msg)
  }


# Function ----------------------------------------------------------------

  # Predict tags based on first tag
  for(n in tags) {
    # Index tag in first position of tagpre
    i <- grep(x = data$TagPre, pattern = paste0("^", n))
    # Assign tag
    if(length(i) > 0) {
      data[i,"TagPost"] <- n
    }
  }

  # Return processed data, sorted by TagPost
  data[order(data$TagPost),]

}
