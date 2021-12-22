# https://adventofcode.com/2021/day/2

# --- Day 2: Dive! ---
  
# forward X increases the horizontal position by X units.
# down X increases the depth by X units.
# up X decreases the depth by X units.
# Your horizontal position and depth both start at 0.
# 
# Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?
#   
#   

library(dplyr)
library(stringr)


readLines('./data/day2.txt') |> 
  as_tibble() |> 
  mutate(
    direction = str_extract(value, '^[a-z]+'),
    value = str_extract(value, '[0-9]+') |> as.numeric(),
    horizontal = ifelse(direction == 'forward', value, 0),
    depth = case_when(
      direction == 'down' ~ value,
      direction == 'up' ~ -value,
      TRUE ~ 0
    )
  ) |> 
  select(horizontal, depth) |> 
  summarise(across(everything(), sum)) |> 
  summarise(solution = horizontal * depth)


# --- Part Two ---