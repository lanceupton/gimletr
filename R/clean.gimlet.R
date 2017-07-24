#' Preprocess Gimlet data.
#'
#' Preprocesses data output from Gimlet.
#'
#' @param data A data frame with Gimlet-provided variable names.
#' @param tags Optional. A character vector of promoted tags.
#' @param id Optional. A character vector of users.
#'
#' @return A data frame containing processed data.
#'
#' @examples
#' clean.gimlet(read.gimlet("site", "e@mail.com", "pass"))
#'
#' @export clean.gimlet

clean.gimlet <- function(data, tags, id) {

# Handle Arguments --------------------------------------------------------

  # Throw error if required arguments missing
  if(missing(data)) {
    error("Please specify data.")
  }

  # Check argument classes
  if(!is.data.frame(data)) {
    error("data must be a data frame.")
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
    data[,n] <- x
  }

  # Remove bad characters from Id variable
  data$Id <- gsub(pattern = "[[:punct:]]|[[:space:]]|[[:digit:]]", replacement = "", x = data$Id)

  # Proceed based on optional arguments
  # If both are missing
  if(missing(tags) + missing(id) == 2) {
    # Return data without optional processing
    return(data)
  } else {
    # If tags is given
    if(!missing(tags)) {
      # Label blank tags
      i <- which(data$TagPre == "")
      if(length(i) > 0) {data[i,"TagPre"] <- "BLANK"}
      # Predict tags based on first tag
      for(n in tags) {
        # Index tag in first position of tagpre
        i <- grep(x = data$TagPre, pattern = paste0("^", n))
        # Assign tag
        data[i,"TagPost"] <- n
      }
    }
    # If id is given
    if(!missing(id)) {
      # Index IDs not in list
      i <- which(!dat$Id %in% id)
      if(length(i) > 0) {
        # Print them
        message(cat("Undocumented IDs:", unique(data[i,"Id"]), sep = "\n"))
      }
    }

    # Return processed data, sorted by tag
    data[order(data$TagPost),]

  }

}
