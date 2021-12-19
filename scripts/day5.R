## PART 0 ----
# Given points, calculate all integer points covered by line
expand_line <- function(x1, x2, x3, x4) {
  
}

# Read input.
DATA <- as.character(unlist(read.delim("inputs/day5.txt", header = FALSE)))
vents <- t(matrix(as.integer(unlist(regmatches(DATA, gregexpr("[0-9]+", DATA)))), nrow = 4))
colnames(vents) <- c("x1", "y1", "x2", "y2")

## PART 1 ----

# Which lines are horizontal/vertical?
vents_vh <- vents[vents[,"x1"] == vents[,"x2"] | vents[,"y1"] == vents[,"y2"],]

# Bounds of matrix
min_x <- min(c(vents_vh[,"x1"], vents_vh[,"x2"]))
max_x <- max(c(vents_vh[,"x1"], vents_vh[,"x2"]))
min_y <- min(c(vents_vh[,"y1"], vents_vh[,"y2"]))
max_y <- max(c(vents_vh[,"y1"], vents_vh[,"y2"]))

# Empty matrix
mat_tally <- matrix(rep(0, max_x * max_y), nrow = max_x, ncol = max_y)

# For each horz/vert line, derive all points, and increment mat_tally
for (i in 1:nrow(vents_vh)) {
  # Get current line and identify all points along segment
  cur <- vents_vh[i,]
  cur_pts <- expand.grid(
    x = cur["x1"]:cur["x2"],
    y = cur["y1"]:cur["y2"]
  )

  # Increment matrix
  idxs <- ncol(mat_tally) * (cur_pts$y-1) + cur_pts$x
  mat_tally[idxs] <- mat_tally[idxs] + 1
}

# Number of points with at least two line segments intersecting: 5169
length(mat_tally[mat_tally > 1])

## PART 2 ----
vents_diag <- vents[vents[,"x1"] != vents[,"x2"] & vents[,"y1"] != vents[,"y2"],]

for (i in 1:nrow(vents_diag)) {
  # Get current line and identify all points
  cur <- vents_diag[i,]
  cur_pts <- data.frame(
    x = cur["x1"]:cur["x2"],
    y = cur["y1"]:cur["y2"]
  )
  
  # Increment matrix
  idxs <- ncol(mat_tally) * (cur_pts$y-1) + cur_pts$x
  mat_tally[idxs] <- mat_tally[idxs] + 1
}

# Number of points with at least two line segments intersecting: 5169
length(mat_tally[mat_tally > 1])
