
### Gimlet ETL Script ###

# List parameters
par <- list(
  # First and final dates of report
  dates    = c("2017-01-01", "2017-01-31"),
  # Location of .csv files with login information, user initials, and promoted tags
  login    = "login.csv",
  initials = "initials.csv",
  tags     = "tags.csv",
  output   = "data.csv"
)


# Directories / Packages / Data -------------------------------------------

# Install and load packages
pack <- c("rvest", "httr", "dplyr", "tibble")
lapply(X = pack, FUN = library, character.only = TRUE)
devtools::install_github(repo = "lanceupton/gimletr")
library(gimletr)
rm(pack)

# Read data
dat <- lst(
  login    = read.csv(file = par$login, stringsAsFactors = FALSE),
  id       = unlist(read.csv(file = par$initials, stringsAsFactors = FALSE)$Id),
  tags     = unlist(read.csv(file = par$tags, stringsAsFactors = FALSE)),
  raw      = do.call(what = rbind, args = by(data = login, INDICES = 1:nrow(login), FUN = function(row) {
    do.call(what = read.gimlet, args = row)}))
)


# Process & Save ----------------------------------------------------------

# Pre-process data
dat$clean <- prep.gimlet(data = dat$raw)

# Process tags
dat$clean$TagPost <- process.tags(x = dat$clean$TagPre, tags = dat$tags)

# If there are no unmatched initials,
if(process.ids(x = dat$clean$Id, id = dat$id)) {
  
  # Save cleaned data
  write.csv(x = data$clean, file = par$output, row.names = FALSE)
  
}
