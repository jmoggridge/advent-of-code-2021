# --- Day 4: Giant Squid ---

#   You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

# Maybe it wants to play bingo?

#   Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

# The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

# <test4.txt>

# The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

# To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

## just parsing stuff

parse_input <- function(file) {
  input <- readLines(file)
  
  # parse first line of input for draws
  balls <-
    input[[1]] |>
    str_split(',', simplify = T) |>
    as.numeric()
  
  # how many boards in data?
  n_boards <- (length(input[3:length(input)]) + 1) / 6
  
  ## parse data by board into long form table
  boards <-
    input[3:length(input)] |>
    as_tibble() |>
    filter(value != "") |>
    mutate(
      board = rep(1:n_boards, each = 5),
      value = str_trim(value) |>  str_split('[ ]+'),
      value = map(value, as.numeric)
    ) |> 
    unnest_wider(value, names_sep = '_') |>
    group_by(board) |>
    mutate(row = row_number()) |>
    pivot_longer(value_1:value_5, names_to = 'col', values_to = 'value') |>
    mutate(col = str_remove(col, 'value_') |> as.numeric(),
           marked = FALSE)
  
  return(list(boards = boards,
              balls = balls))
}


play_bingo <- function(boards, balls) {
  ## call a number and mark boards
  all_hits <- tibble()
  
  ## do balls until found a winner
  for (x in 1:length(balls)) {
    # mark squares matching the drawed ball
    boards <- boards |>
      mutate(marked = ifelse(value == balls[[x]], TRUE, marked))
    # extract the hits
    new_hits <- boards |> filter(marked)
    # keep the unmarked squares
    boards <- boards |> filter(!marked)
    
    # combine all existing hits to check for winner
    all_hits <- bind_rows(all_hits, new_hits)
    # any with a full row
    complete_rows <- all_hits |>
      count(board, row) |>
      filter(n == 5)
    # any with a full col
    complete_cols <- all_hits |>
      count(board, col) |>
      filter(n == 5)
    winner <- bind_rows(complete_rows, complete_cols)
    # stop the game if there's a winner
    if (nrow(winner) > 0) {
      last_ball <- balls[[x]]
      break
    }
  }
  return(list(
    winner = winner,
    last_ball = last_ball,
    marked_boards = bind_rows(boards, all_hits)
  ))
}

find_solution <-  function(marked_boards, winner, last_ball) {
  ## sum of all unmarked numbers on that board
  ## multiply that sum by the number that was just called when the board won
  marked_boards |>
    filter(board %in% winner$board & !marked) |>
    pull(value) |>
    sum() * last_ball
}


# file <- 'data/sample4.txt'
file <- 'data/day4.txt'
data <- parse_input(file)
rs <- play_bingo(boards = data$boards, balls = data$balls)

sol <- find_solution(
  marked_boards = rs$marked_boards,
  winner = rs$winner,
  last_ball = rs$last_ball
)
cat("Part 1 solution:", sol)



# ----------------------------------------------------------------------

# --- Part Two -----

#   On the other hand, it might be wise to try a different strategy: let the giant squid win.

# You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

# In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

# Figure out which board will win last. Once it wins, what would its final score be?

play_bingo_badly <- function(boards, balls) {
  ## call a number and mark boards
  winners <- tibble()
  all_hits <- tibble()
  
  ## do balls until found a winner
  for (x in 1:length(balls)) {
    # mark squares matching the draw
    boards <- boards |>
      mutate(marked = ifelse(value == balls[[x]], TRUE, marked))
    # extract the hits
    new_hits <- boards |>
      filter(marked)
    # keep the unmarked squares
    boards <- boards |> filter(!marked)
    
    # combine all existing hits to check for winner
    all_hits <- bind_rows(all_hits, new_hits)
    # any with a full row
    complete_rows <- all_hits |>
      count(board, row) |>
      filter(n == 5)
    # any with a full col
    complete_cols <- all_hits |>
      count(board, col) |>
      filter(n == 5)
    new_winners <- 
      bind_rows(complete_rows, complete_cols)
    
    # add a new winner(s)
    if (nrow(new_winners) >= 1) {
      # new_winners [board, score, step]
      new_scores <- 
        # keep the board score at win-time
        bind_rows(boards, all_hits) |> 
        filter(board %in% new_winners$board & !marked) |> 
        group_by(board) |> 
        summarise(score = sum(value) * balls[[x]],
                  step = x)
      # add new results to winners data
      winners <- winners |> bind_rows(new_scores)
      
      # remove the winning boards from the game
      boards <- boards |> filter(!board %in% winners$board)
      all_hits <- all_hits |> filter(!board %in% winners$board)
    }
  }
  return( winners |> filter(step == max(step)) |> pull(score) )
  
}


file <- 'data/day4.txt'
data <- parse_input(file)
solution2 <- play_bingo_badly(boards = data$boards,
                 balls = data$balls)

cat("Part2 solution = ", solution2)