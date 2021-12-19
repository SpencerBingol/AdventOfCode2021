# Move forward by a day
increment_day <- function(DATA) {
  num_new_fish <- length(DATA[DATA==0])
  DATA <- c(DATA-1, rep(8, num_new_fish))
  DATA[which(DATA < 0)] <- 6
  
  cat(num_new_fish, "new fish", length(DATA), "total fish\n")
  
  DATA
}

increment_day_table <- function(DATA_table) {
  c(DATA_table[2:7], DATA_table[8] + DATA_table[1], DATA_table[9], DATA_table[1])
}

calc_pop_growth <- function(DATA, num_days) {
  # Create the table for efficiency
  DATA_table <- c(0, as.integer(table(DATA)), 0, 0, 0)
  for (i in 1:num_days) { DATA_table <- increment_day_table(DATA_table) }
  
  # After 80 days, num fish: 359344
  format(sum(DATA_table), scientific = FALSE)
}

## PART 1 ----

# Load data 
DATA <- as.integer(unlist(read.csv("inputs/day6.txt", header = FALSE)))
calc_pop_growth(DATA, 80L)

## PART 2 ----
calc_pop_growth(DATA, 256L)
