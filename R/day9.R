# --- Day 9: Smoke Basin ---

library(dplyr)
library(purrr)

# parse input map to a matrix in the correct orientation
parse_input <- function(file) {
  map_df <-
    readLines(file) |>
    map(~ trimws(.x) |> strsplit('') |> unlist() |> as.numeric()) |>
    tibble::enframe() |>
    select(value) |> tidyr::unnest_wider(value) |>
    as.matrix()
}

# solve part 1
sum_low_points <- function(file) {
  # create matrix from input
  map_matrix <- parse_input(file)
  # init 2nd matrix to store risk level values
  low_pts <-
    matrix(
      data = NA,
      nrow = nrow(map_matrix),
      ncol = ncol(map_matrix)
    )
  # iterate over map matrix to find low points and memoize their risk level
  I <- nrow(map_matrix)
  J <- ncol(map_matrix)
  for (i in seq(I)) {
    for (j in seq(J)) {
      # check neighbors heights and see if (i,j) is a low point
      l <- ifelse(i > 1, map_matrix[i - 1, j], Inf)
      r <- ifelse(i < I, map_matrix[i + 1, j], Inf)
      t <- ifelse(j > 1, map_matrix[i, j - 1], Inf)
      b <- ifelse(j < J, map_matrix[i, j + 1], Inf)
      lowest <- min(c(l, r, t, b))
      # add risk level to low points
      if (map_matrix[i, j] < lowest) {
        low_pts[i, j] <- 1 + map_matrix[i, j]
      }
    }
  }
  # find the sum of risk level
  return(sum(low_pts, na.rm = T))
}

# Part 1 solution
sum_low_points('data/day9.txt')




