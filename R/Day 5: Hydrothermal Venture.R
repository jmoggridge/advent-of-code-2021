# --- Day 5: Hydrothermal Venture ---
#   You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.
# 
# They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:
#   
# 0,9 -> 5,9
# 8,0 -> 0,8
# 9,4 -> 3,4
# 2,2 -> 2,1
# 7,0 -> 7,4
# 6,4 -> 2,0
# 0,9 -> 2,9
# 3,4 -> 1,4
# 0,0 -> 8,8
# 5,5 -> 8,2

# Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:
#   
# An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
# An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
# For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
# 
# So, the horizontal and vertical lines from the above list would produce the following diagram:

# .......1..
# ..1....1..
# ..1....1..
# .......1..
# .112111211
# ..........
# ..........
# ..........
# ..........
# 222111....

# In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
# 
# To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
# 
# Consider only horizontal and vertical lines. At how many points do at least two lines overlap?



library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

plot_diagram <- function(df){
  ggplot(df, aes(x = x1, y = y1, color = vent)) + 
    scale_y_reverse() +
    geom_segment(aes(xend = x2, yend=y2), arrow = arrow()) +
    geom_point() +
    scale_color_viridis_d() +
    ggrepel::geom_text_repel(aes(label = vent), color= 'black') +
    theme_minimal() +
    theme(aspect.ratio=1) 
}

parse_data <- function(file){
  tibble(vents = readLines(file)) |> 
    separate(col = vents, sep = ' -> |,', into = c('x1', 'y1', 'x2', 'y2')) |> 
    mutate(
      vent = as.factor(row_number()),
      across(x1:y2, as.numeric),
      slope = (y2-y1)/(x2-x1)
    ) |> 
    relocate(vent, x1, x2, y1, y2)
  
}

horizontal_and_vertical_points <- function(df){
  df |> 
    filter(slope %in% c(0, -Inf, Inf)) |> 
    select(-slope) |> 
    group_by(vent) |> 
    nest(vector = c(x1,x2,y1,y2)) |> 
    mutate(points = map(vector, function(df) {
      expand_grid(x = df$x1:df$x2,  y=df$y1:df$y2)
    }))
}

####
parse_data('data/day5.txt') |> 
  horizontal_and_vertical_points() |> 
  select(-vector) |> 
  unnest(cols = c(points)) |> 
  # find overlap
  group_by(x,y) |> 
  summarize(overlap = n()) |> 
  filter(overlap >=2) |> 
  nrow()
# 8622


#---------------------------------------------------------------------------

# --- Part Two ---

#  Unfortunately, considering only horizontal and vertical lines
#  doesn't give you the full picture; you need to also consider 
#  diagonal lines.

#  Because of the limits of the hydrothermal vent mapping system,
#  the lines in your list will only ever be horizontal, vertical, 
#  or a diagonal line at exactly 45 degrees. In other words:

# An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
# An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

# Considering all lines from the above example would now produce the following diagram:
# 
# 1.1....11.
# .111...2..
# ..2.1.111.
# ...1.2.2..
# .112313211
# ...1.2....
# ..1...1...
# .1.....1..
# 1.......1.
# 222111....

# You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
# 
# Consider all of the lines. At how many points do at least two lines overlap?


diagonal_points <- function(df){
  df |> 
    filter(slope %in% c(1, -1)) |> 
    group_by(vent) |> 
    nest(vector = c(x1,x2,y1,y2)) |> 
    mutate(points = map(
      .x = vector,  
      .f = function(df) {
        tibble(x = seq(df$x1, df$x2), y = seq(df$y1, df$y2))
      })
    ) |> 
    ungroup() |> 
    unnest(points) |> 
    select(x, y)
}



parse_data('data/day5.txt') |> 
  horizontal_and_vertical_points() |> 
  ungroup() |> 
  select(points) |> 
  unnest(points) |> 
  bind_rows(diagonal_points(df)) |> 
  count(x,y) |> 
  filter(n >=2) |> 
  nrow()
#22037









