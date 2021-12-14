## PART 1 (This borrows from David Robinson's solution) ----

# Convert binary string to decimal
to_int <- function(x) { strtoi(paste(x, collapse = ""), base = 2) }

# Read file
DATA <- as.character(unlist(read.delim("inputs/day3.txt", header = FALSE, colClasses = "character")))
DATA_MAT <- do.call(rbind, lapply(strsplit(DATA, split = ""), as.integer))

# If only two options are 0/1, mean >= 0.5 means 1 is most common value
bit_modes <- round(colMeans(DATA_MAT), 0)

# Calculate values
gamma <- to_int(bit_modes)
epsilon <- to_int(1 - bit_modes)

# POWER CONSUMPTION: 3242606 UNITS
gamma * epsilon

## PART 2 ----

# Filter rows based on vals at given index
filter_mat <- function(CUR_MAT, k, k_max = 12, most_common = TRUE) {
  if (!is.matrix(CUR_MAT) | k > k_max) return(CUR_MAT)
  x <- CUR_MAT[, k]
  
  filter_val <- as.integer(ifelse(most_common, mean(x) >= 0.5, mean(x) < 0.5))
  filter_mat(CUR_MAT = CUR_MAT[CUR_MAT[,k] == filter_val,], k = k + 1, k_max = 12, most_common = most_common)
}

# Calculate values
oxygen_generation <- to_int(filter_mat(DATA_MAT, k = 1, k_max = 12, most_common = TRUE))
co2_scrubbing <- to_int(filter_mat(DATA_MAT, k = 1, k_max = 12, most_common = FALSE))

# LIFE SUPPORT RATING: 4856080 UNITS
oxygen_generation * co2_scrubbing