## PART 1 ----

# Read file
DATA <- as.character(unlist(read.delim("inputs/day2.txt", header = FALSE)))

# Parse regex
DATA_DF <- data.frame(
  magnitude = as.numeric(gsub("^.*([0-9]+)$", "\\1", DATA)), # extract the integer
  direction = as.character(gsub("^([a-z]+).*$", "\\1", DATA)) # extract the string
)

# Add additional fields
DATA_DF$axis <- ifelse(DATA_DF$direction %in% c("up", "down"), "vertical", "horizontal")
DATA_DF$amount <- ifelse(DATA_DF$direction == "up", -1, 1) * DATA_DF$magnitude
DATA_DF$adj_vert <- ifelse(DATA_DF$axis == "vertical", DATA_DF$amount, 0L)
DATA_DF$adj_horz <- ifelse(DATA_DF$axis == "horizontal", DATA_DF$amount, 0L)

# FINAL POSITION: 1507611 UNITS
sum(DATA_DF$adj_vert) * sum(DATA_DF$adj_horz)

## PART 2 ----

# Add aim and depth columns
DATA_DF$aim <- cumsum(DATA_DF$adj_vert)
DATA_DF$depth <- DATA_DF$aim * DATA_DF$adj_horz

# FINAL POSITION: 1880593125 UNITS
sum(DATA_DF$adj_horz) * sum(DATA_DF$depth)
