# https://adventofcode.com/2021/day/2

# --- Day 2: Part 1:  Dive! ----

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
    depth = case_when(direction == 'down' ~ value,
                      direction == 'up' ~ -value,
                      TRUE ~ 0)
  ) |>
  select(horizontal, depth) |>
  summarise(across(everything(), sum)) |>
  summarise(solution = horizontal * depth)

cat("DAY2: part 1 solution ^^")
cat("DAY2: part 2 solution ^^")


# --- Part Two ------
# you'll also need to track a third value, aim, which also starts at 0.

#
# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
#   It increases your depth by your aim multiplied by X.
#'
# 1 # forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
# 2 # down 5 adds 5 to your aim, resulting in a value of 5.
# 3 # forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
# 4 # up 3 decreases your aim by 3, resulting in a value of 2.
# 5 # down 8 adds 8 to your aim, resulting in a value of 10.
# 6 # forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.

    # down X increases your aim by X units.
    # up X decreases your aim by X units.
    # forward X increases your horizontal position by X units.
       # and it increases your depth by your aim multiplied by X.

    # parse instructions into direction + value
    # compute these directions as change in aim and horizontal position
    # create a cumulative sum for forward and aim to get current values.
    # use the current aim to calculate the depth change at each step
    # create a cumulative sum of depth changes starting from 0

readLines('./data/day2.txt') |>
  as_tibble() |>
  mutate(
    direction = str_extract(value, '^[a-z]+'),
    value = str_extract(value, '[0-9]+') |> as.numeric(),
    horizontal = ifelse(direction == 'forward', value, 0),
    aim = case_when(
      direction == 'down' ~ value,
      direction == 'up' ~ -value,
      TRUE ~ 0)
    ) |> 
  mutate(across(horizontal:aim, cumsum, .names="{.col}_cumsum")) |> 
  mutate(depth = ifelse(direction == 'forward', lag(aim_cumsum)*value, 0),
         depth = ifelse(is.na(depth), 0, depth)) |> 
  mutate(depth_cumsum = cumsum(depth)) |> 
  filter(row_number()==n()) |> 
  summarise(solution = horizontal_cumsum * depth_cumsum)









