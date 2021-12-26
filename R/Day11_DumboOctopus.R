# --- Day 11: Dumbo Octopus --------------------------------------------------

# 100 octopuses arranged neatly in a 10 by 10 grid.
# Each octopus slowly gains energy over time and flashes brightly for a moment when its energy is full. 
# Although your lights are off, maybe you could navigate through the cave without disturbing the octopuses if you could predict when the flashes of light will happen.

# Each octopus has an energy level - your submarine can remotely measure the energy level of each octopus (your puzzle input). For example:
#

#       5483143223
#       2745854711
#       5264556173
#       6141336146
#       6357385478
#       4167524645
#       2176841721
#       6882881134
#       4846848554
#       5283751526


# The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an energy level of 5, the bottom-right one has an energy level of 6, and so on.

# 
# You can model the energy levels and flashes of light in steps.
# During a single step, the following occurs:

# -  First, the energy level of each octopus increases by 1.

#' Then, _any octopus with an energy level greater than 9 flashes_.
#'  This increases the energy level of all adjacent octopuses by 1, including 
#'  octopuses that are diagonally adjacent. If this causes an octopus to have
#'  an energy level greater than 9, it also flashes. 
#'  This process continues as long as new octopuses keep having their energy
#'   level increased beyond 9. (An octopus can only flash at most once per step.)
#
# Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.

# Adjacent flashes can cause an octopus to flash on a step even if it begins that step with very little energy. Consider the middle octopus with 1 energy in this situation:...
# 
# After step 10, there have been a total of 204 flashes. Fast forwarding, here is the same configuration every 10 steps: ....
# 
# After 100 steps, there have been a total of 1656 flashes.
# 
# Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps. How many total flashes are there after 100 steps?
#   
#   

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

parse_input <- function(file){
  readLines(file) |> 
    as_tibble() |> 
    setNames('energy') |> 
    mutate(
      energy = stringr::str_split(energy, ''),
      row = row_number(),
    ) |> 
    unnest(energy) |> 
    group_by(row) |> 
    mutate(col = row_number()) |> 
    ungroup() |> 
    mutate(energy = as.numeric(energy))
}


#' @title glowneighbs
#' @description  do for each flasher... 
#' increase neighbor energy by 1,
#' push current flasher to resting group
#' if any neighbor pushed over 9 energy, add it to flashers stack
glow_neighbs <- function(octopi, flashing, resting){
  
  # add current octo to resting group and reset energy
  resting <- resting |>
    bind_rows(flashing |> slice(1))
  
  x <- flashing$row[1]
  y <- flashing$col[1]
  # affect neighbors of top octopus in flashers
  proximal_octopi <- octopi |>
    filter(row %in% c((x-1):(x+1)), col %in% (y-1):(y+1)) |> 
    mutate(energy = energy + 1)
  
  # check if any new octopi to add to 'flashing' stack
  # remove current flasher
  flashing <- flashing |> 
    bind_rows(proximal_octopi |> filter(energy>9)) |>
    slice(-1)
  
  # add non-flashing back to grid
  octopi <- octopi |>
    anti_join(proximal_octopi, by=c('row','col')) |> 
    bind_rows(proximal_octopi |> filter(energy<=9))
  
  return(list(
    octopi = octopi, 
    flashing = flashing,
    resting = resting))
}


count_flashes <- function(octopi, steps){
  flash_count <- list()[steps]
  
  #' # During a single step, the following occurs:
  for (step in 1:steps){
    #'   # First, the energy level of each octopus increases by 1.
    octopi$energy <- octopi$energy + 1
    #'   # Then, _any octopus with an energy level greater than 9 flashes_.
    flashing <- octopi |> filter(energy>9)
    octopi <- octopi |> anti_join(flashing, by=c('row','col'))
    resting <- tibble()
    while(nrow(flashing) > 0){
      rs <- glow_neighbs(octopi, flashing, resting)
      octopi <- rs$octopi
      flashing <- rs$flashing
      resting <- rs$resting
    }
    
    octopi <- octopi |> 
      bind_rows(resting |> mutate(energy = 0))
    
    # How many total flashes are there after 100 steps?
    # -keep track of flashes / step
    flash_count[step] <- nrow(resting)
  }
  
  flash_count |> unlist() |> sum() 
}

  
# Part 1 Solution ## 1585
parse_input(file = 'data/day11.txt') |> 
  count_flashes(100)





  
  