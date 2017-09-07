
### Gimlet Report Script ###

# List parameters
par <- list(
  input    = "data.csv",
  initials = "initials.csv",
  output   = "gimletr_report.pdf"
)


# Directories / Packages / Data -------------------------------------------

# Install and load packages
pack <- c("dplyr", "knitr", "pander", "rmarkdown", "stringi", "lubridate")
lapply(X = pack, FUN = library, character.only = TRUE)
devtools::install_github(repo = "lanceupton/gimletr")
library(gimletr)
rm(pack)

# Read data
dat <- read.csv(file = par$input, stringsAsFactors = FALSE)

# Handle datetime
dt <- as.POSIXct(dat$DateTime, format = "%m/%d/%Y %H:%M")
if(any(is.na(dt))) {
  dt <- as.POSIXct(dat$DateTime)
}
dat$DateTime <- dt
rm(dt)

# Get some date information
par$dates <- c(as.Date(min(dat$DateTime)), as.Date(max(dat$DateTime)))

# Sort by Id & DateTime
dat <- arrange(dat, Id, DateTime)

# Select reporting data
dat <- list(
  clean  = dat,
  report = data.frame(
    Id       = dat$Id,
    Date     = strftime(x = dat$DateTime, format = "%m/%d"),
    UsedTag  = dat$TagPre,
    Correct  = dat$TagPost,
    Question = dat$Question,
    Answer   = dat$Answer,
    stringsAsFactors = FALSE
  )
)


# Ranks -------------------------------------------------------------------

# Load initials
dat$id <- read.csv(file = par$initials, stringsAsFactors = FALSE)

# Vector of corresponding groups for each id
g <- dat$id[match(dat$clean$Id, dat$id$Id),"Group"]

# Compute stats for each user
stats <- merge(x = dat$id, y = compute.errors(data = dat$clean, groups = g), by = "Id", all = TRUE)

# Deal with NA obs
i <- which(is.na(stats$NumEnt))
if(length(i) > 0) {
  # Set numeric
  stats[i, c("NumEnt", "NumErr", "PerErr")] <- 0
  # Set character
  stats[i, "Score"] <- "---"
}

# Sort
stats <- arrange(.data = stats, desc(NumEnt), NumErr)


# Counts ------------------------------------------------------------------

# Counts per day
counts <- data.frame(table(dat$report$Date))
names(counts) <- c("date", "count")

# Calculate average
avg <- nrow(dat$clean) / as.integer(par$dates[2] - par$dates[1])

# Create a summary table
table <- rbind(
  stats[which(!is.na(stats$Group)),c("Id", "Score")],
  data.frame(
    Id    = c("---", stats[which(is.na(stats$Group)),"Id"]),
    Score = c("---", stats[which(is.na(stats$Group)),"Score"])
  ),
  data.frame(
    Id    = c("---", "**Entries/Day:**"),
    Score = c("---", round(avg, 0))
  )
)
rownames(table) <- NULL

rm(avg, g, i)

# Knit & Render Report ----------------------------------------------------

# Create a name for the report
par$name <- paste0(format(par$dates, format = "%m/%d/%Y"), collapse = " - ")

# Generate the report
knit(input = "Report.Rmd", output = "Report.md")
render(input = "Report.md", output_file = "Report.pdf")
