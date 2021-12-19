## PART 0: SET UP ----

# For a given step, check if BINGO! condition is met.
check_boards_step <- function(boards, inputs, k) {
  
  # Which numbers have been called?
  cur_steps <- inputs[1:k]

  # Identify matching values in each matrix
  match_mats <- lapply(
    boards, 
    FUN = function(x, cur_steps) {
      matrix(x %in% cur_steps, nrow = 5, ncol = 5)
    },
    cur_steps = cur_steps
  )
  
  # If any row has number of TRUEs equal to number of columns (or vice-versa), BINGO! is met
  mat_is_solved <- sapply(match_mats, function(x) { any(c(rowSums(x) == ncol(x), colSums(x) == nrow(x))) })
  
  # Object to return
  list(
    nums_called = cur_steps,
    steps = length(cur_steps),
    match_mats = match_mats,
    mat_is_solved = mat_is_solved,
    any_solutions = any(mat_is_solved),
    all_solutions = all(mat_is_solved)
  )
}

# Read first line of input (numbers selected at random).
f <- file("inputs/day4.txt", "r")
inputs <- as.integer(strsplit(readLines(con = f, n = 1), ",")[[1]])
boards <- list()

# Read Bingo boards from text file.
while(TRUE) {
  cur_board_dat <- readLines(con = f, n = 6)
  if (length(cur_board_dat) == 0) break
  
  cur_board_vec <- as.integer(unlist(strsplit(cur_board_dat[2:6], " ")))
  boards[[length(boards)+1]] <- t(matrix(cur_board_vec[!is.na(cur_board_vec)], nrow = 5, ncol = 5))
}

# Establish object that stores information on all game steps
all_steps <- list()

# "Game" loop - which board wins last?
for (k in 1:length(inputs)) {
  # Process all boards for this step
  all_steps[[length(all_steps)+1]] <- check_boards_step(boards, inputs, k = k)
}

## PART 1 ----

# info on the solved board, and the last number called
first_step_with_solution <- min(which(sapply(all_steps, function(x) { x$any_solutions })))
first_step_info <- all_steps[[first_step_with_solution]]

first_board_solved <- which(first_step_info$mat_is_solved)[1]
last_num <- first_step_info$nums_called[length(first_step_info$nums_called)] # last number called.
unmarked_val_sum <- sum(boards[[first_board_solved]] * !first_step_info$match_mats[[first_board_solved]]) # zero-out marked values, and sum.

# calculate score
game_score <- unmarked_val_sum * last_num

# output results
cat("First board completed was", first_board_solved, "on step", paste0(first_step_with_solution, ". SCORE: ", game_score, "\n"))

## PART 2 ----

# Which is the first step with all boards completed?
min_step_all_solved <- min(which(sapply(all_steps, function(x) { x$all_solutions })))

# For each board, in how many steps is each matrix not "solved", plus 1 (this is the solution step)
mat_step_solved <- 1 + length(all_steps) - rowSums(matrix(unlist(lapply(all_steps, function(x) { x$mat_is_solved })), nrow = length(all_steps)))

# Board 92 was the last one solved, on step 83.
last_step_with_solution <- max(mat_step_solved)
last_board_solved <- which(last_step_with_solution == mat_step_solved)
last_step_info <- all_steps[[last_step_with_solution]]

# calculate score
last_num <- last_step_info$nums_called[length(last_step_info$nums_called)] # last number called.
unmarked_val_sum <- sum(boards[[last_board_solved]] * !last_step_info$match_mats[[last_board_solved]]) # zero-out marked values, and sum.
game_score <- unmarked_val_sum * last_num

# output results
cat("Last board completed was", last_board_solved, "on step", paste0(last_step_with_solution, ". SCORE: ", game_score, "\n"))
