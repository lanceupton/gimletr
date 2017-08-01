#' Verify Initials in Gimlet data.
#'
#' Verify Initials in Gimlet data.
#'
#' @param data A data frame (\code{\link[base]{data.frame}}) with Gimlet-provided variable names.
#' @param id A character vector of users.
#'
#' @return A message containing information about ID documentation. If all IDs in \code{data} are found in \code{id}, This function returns \code{TRUE}.
#'
#' @examples
#' process.ids(prep.gimlet(read.gimlet("mylibrary", "e@mail.com", "password")), ids)
#'
#' @export process.ids

process.ids <- function(data, id) {

# Handle Arguments --------------------------------------------------------

  # Throw error if required arguments missing
  if(missing(data)) {
    error("Please specify data.")
  }
  if(missing(id)) {
    error("Please specify accepted IDs.")
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

  # Index IDs not in list
  i <- which(!data$Id %in% id)
  if(length(i) > 0) {
    # Print them
    message(cat("Undocumented IDs:", unique(data[i,"Id"]), sep = "\n"))
    # View related observations
    View(data[i,])
    return(FALSE)
  } else {
    message("All IDs in data are documented.")
    return(TRUE)
  }

}
