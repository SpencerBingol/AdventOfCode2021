## PART 1 ----
# Replicate lag() for INT vectors in base
lag_INT <- function(x, k = 1) { c(rep(NA_integer_, k), x[1:(length(x)-k)]) }

# Read file
DATA <- as.integer(unlist(read.delim("inputs/day1.txt", header = FALSE)))

# Compare reading to previous day: 1655 DAYS OF INCREASE
sum(DATA > lag_INT(DATA), na.rm = T)

## PART 2 ----

# Create lag vectors and sum
WINDOW <- DATA + lag_INT(DATA) + lag_INT(DATA, k = 2) # ORIG + LAG1 + LAG2

# Compare current to n-1 rolling window: 1683 WINDOWS OF INCREASE
sum(WINDOW > lag_INT(WINDOW), na.rm = T) # ORIG > LAG1
