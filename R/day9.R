# --- Day 9: Smoke Basin ------------------------------------------------

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

find_low_points <- function(map_matrix) {
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
  return(low_pts)
}

# solve part 1
sum_risk_levels <- function(file) {
  # create matrix from input
  map_matrix <- parse_input(file)
  low_pts <- find_low_points(map_matrix)
  # find the sum of risk level
  return(sum(low_pts, na.rm = T))
}

# Part 1 solution: 522
sum_risk_levels('data/day9.txt')


# --- Part Two ------------------------------------------------------------

# A basin is all locations that eventually flow downward to a single low point.

# Therefore, every low point has a basin, although some basins are very small.

# Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

map_basins <- function(mat, lowpts) {

  I <- nrow(mat)
  J <- ncol(mat)
  # if any point unassigned, continue mapping
  while (any(is.na(lowpts))) {
    # iterate over map matrix to grow basins
    for (i in seq(I)) {
      for (j in seq(J)) {
        if (is.na(lowpts[i, j])) {
          # check if neighboring basing to left, right, top, or bottom
          neighbors <- c(
            l <- ifelse(i > 1, lowpts[i - 1, j], NA),
            r <- ifelse(i < I, lowpts[i + 1, j], NA),
            t <- ifelse(j > 1, lowpts[i, j - 1], NA),
            b <- ifelse(j < J, lowpts[i, j + 1], NA)
          )
          # ignore any ridges
          neighbors[neighbors == 'X'] <- NA
          # if neighbor is in a basin, add point ij to that basin
          if (any(!is.na(neighbors))) {
            lowpts[i, j] <- neighbors[which(!is.na(neighbors))][1]
          }
        }
      }
    }
  }
  return(lowpts)
}


## create map matrix and chart low points and ridges
mat <- parse_input(file = 'data/day9.txt')
lowpts <- find_low_points(mat)

# assign a number id to each lowpoint
# ridges are 'X'
lowpts[!is.na(lowpts)] <-1:sum(!is.na(lowpts))
lowpts[which(mat == 9)] <- 'X'

# assign points to basins; remove ridges; count points per basin, sort top.
basins <- 
  map_basins(mat, lowpts) |>
  as.vector() |> 
  as.factor() |> 
  as_tibble() |> 
  filter(value != 'X') |> 
  count(value, sort = T)

# take product of sizes of the three largest basins
basins |> 
  slice(1:3) |> 
  pull(n) |> prod()
