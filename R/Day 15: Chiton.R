




# --- Day 15: Chiton ---

library(tidyverse)

parse_input <- function(file) {
  tibble(value = readLines(file) |> strsplit(''),
         row = readLines(file) |> length() |> seq()) |>
    unnest(value) |>
    group_by(row) |>
    mutate(col = row_number()) |>
    ungroup()
  
  
}


file <- 'data/day15.txt'
risk <- parse_input(file)

risk_matrix <-
  risk$value |> unlist() |> as.numeric() |> matrix(nrow =  sqrt(nrow(risk)))
memo_matrix <-
  matrix(Inf, nrow = sqrt(nrow(risk)), ncol = sqrt(nrow(risk)))
memo_matrix[1, 1] <- 0
# You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).
I <- nrow(risk_matrix)
J <- ncol(risk_matrix)


for (i in seq(I)) {
  for (j in seq(J)) {
    current_risk <- memo_matrix[i, j]
    # update below
    if (i < I) {
      if (memo_matrix[i + 1, j] > (current_risk + risk_matrix[i + 1, j])) {
        memo_matrix[i + 1, j] <- current_risk + risk_matrix[i + 1, j]
      }
    }
    # update right
    if (j < J) {
      if (memo_matrix[i, j + 1] > (current_risk + risk_matrix[i, j + 1])) {
        memo_matrix[i, j + 1] <- current_risk + risk_matrix[i, j + 1]
      }
    }
  } # for j
} # for i


memo_matrix[sqrt(nrow(risk)), sqrt(nrow(risk))]



# --- Part Two ---

# Have to increase the matrix by 5*5 of original size with increment +1
# for each row & column...

# 0 1 2 3 4
# 1 2 3 4 5
# 2 3 4 5 6
# 3 4 5 6 7
# 4 5 6 7 8

# risk levels above 9 wrap back around to 1
# 123456789->1

risk <- parse_input('data/day15.txt') |> 
  mutate(across(everything(), as.numeric))
    
risk |> summary()

risk_rows <- 
  map(.x = 0:4, .f = ~ {
    risk |>
      mutate(
        # add blocks of 100*100 rows/cols of map
        row = (.x * sqrt(nrow(risk))) + row,
        value = value + .x
      )
  }) |>
  bind_rows()
summary(risk_rows)


risk <- map(.x = 0:4, .f = ~ {
  risk_rows |>
    mutate(
      across(everything(), as.numeric),
      # add blocks of rows/cols of map with increased risk
      col = (.x * max(risk$col)) + col,
      value = value + .x
    )
}) |>
  bind_rows()  |> 
  arrange(col, row)
summary(risk)

risk <-  risk |> mutate(value = ifelse(value > 9, value - 9, value))
while(any(risk$value > 9)){
}


risk_matrix <- risk$value |> 
  matrix(nrow =  max(risk$row), ncol = max(risk$col)) |>
  t()

memo_matrix <- matrix(Inf, nrow = max(risk$row), ncol = max(risk$col)) |> t()
risk_matrix[1, 1] <- 0
memo_matrix[1, 1] <- 0

# You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).
I <- nrow(risk_matrix)
J <- ncol(risk_matrix)

current_risk <- 0
for (i in seq(I)) {
  for (j in seq(J)) {
    current_risk <- memo_matrix[i, j]
    # update below
    if (i < I) {
      if (memo_matrix[i + 1, j] > (current_risk + risk_matrix[i + 1, j])) {
        memo_matrix[i + 1, j] <- current_risk + risk_matrix[i + 1, j]
      }
    }
    # update right
    if (j < J) {
      if (memo_matrix[i, j + 1] > (current_risk + risk_matrix[i, j + 1])) {
        memo_matrix[i, j + 1] <- current_risk + risk_matrix[i, j + 1]
      }
    }
  } # for j
} # for i


memo_matrix[nrow(risk)]

