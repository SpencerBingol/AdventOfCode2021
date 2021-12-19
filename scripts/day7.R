## PART 1 ----
DATA <- as.integer(read.delim("inputs/day7.txt", sep = ",", header = FALSE))

fuel_costs <- c()

for(i in 1:max(DATA)) {
  fuel_costs <- c(fuel_costs, sum(abs(DATA - i)))
}

# smallest fuel cost: 355150
min(fuel_costs)

## PART 2 ----
fuel_costs <- c()

for(i in 1:max(DATA)) {
  fuel_costs <- c(fuel_costs, sum((abs(DATA - i) * (abs(DATA - i) + 1)) / 2))
}

# Smallest fuel cost: 98368490
min(fuel_costs)
