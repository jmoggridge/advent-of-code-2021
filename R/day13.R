# --- Day 13: Transparent Origami ---
  
# To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:
# 

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# The first section is a list of dots on the transparent paper.
get_points <- function(input){
  input |> 
    filter(!str_detect(value, 'fold'), !value == '') |> 
    separate(value, into = c('x','y')) |> 
    mutate(across(everything(), as.numeric))
}

# Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines).
get_instructions <- function(input, points){
  instructions <- input |> 
    filter(str_detect(value, 'fold')) |> 
    mutate(value = str_remove(value, 'fold along ')) |> 
    separate(value, into = c('dim', 'place')) |> 
    mutate(place = as.numeric(place))
  return(instructions)
}

# do a fold
do_folding <- function(dim, pl, points){
  upper_left <- points |> filter(get(dim) < pl)
  
  lower_right <- points |>  filter(get(dim) > pl)
  lower_right <- lower_right |>
    select(all_of(dim)) |> 
    mutate(across(everything(), ~{pl - (.x - pl)})) |> 
    bind_cols(lower_right |> select(-dim))
  
  points <- bind_rows(upper_left, lower_right) |> distinct()
  return(points)
}

# visualize points

create_map <- function(points){
  ggplot(points, aes(x,-y)) +
    geom_point(data = crossing(points), color ='grey') +
    geom_point(size = 2) +
    theme_minimal()
}

### Main : part 1 ---------

input <-  readLines('data/day13.txt') |> as_tibble()
points <- get_points(input)
instructions <- get_instructions(input, points)
  
points <- do_folding(dim = instructions$dim[1],
                     pl = instructions$place[1], 
                     points = points)

# part 1 solution -- 842
nrow(points)



# --- Part Two ------------------------------------------------------------

# Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.
# 
# What code do you use to activate the infrared thermal imaging camera system?

input <-  readLines('data/day13.txt') |> as_tibble()
points <- get_points(input)
instructions <- get_instructions(input, points)

purrr::walk(1:nrow(instructions), ~{
  points <<- do_folding(dim = instructions$dim[.x],
                       pl = instructions$place[.x], 
                       points = points)
})
           
create_map(points)







