# --- Day 12: Passage Pathing ---
#' https://adventofcode.com/2021/day/12
#'
#'
#' Not just a path - the only way to know if you've found the best path is to find all of them.
#' Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:

# start-A
# start-b
# A-c
# A-b
# b-d
# A-end
# b-end


library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

parse_input <- function(file) {
  readLines(file) |>
    as_tibble() |>
    separate(value, into = c('node', 'dest'), sep = '-')
}




# edge list
explore_cave <- function(file) {
  depth_first_search <- function(start, target, path) {
    # add node to path
    path <- c(path, start)
    # don't visit small caves more than once
    lil_caves <- path[which(grepl('[a-z]+', path))]
    if (length(lil_caves) !=
        length(unique(lil_caves[which(grepl('[a-z]+', lil_caves))]))) {
      return(NULL)
    }
    
    # if currently at target, record current path & continue
    if (start == target) {
      paths[length(paths) + 1] <<- list(path)
    }
    # iterate over dests
    dests <- cave_adjlist[start][[1]]
    for (dest in dests) {
      depth_first_search(start = dest,
                         target = target,
                         path = path)
    }
    
    return(NULL)
  }
  
  cave_edges <- parse_input(file)
  # directional adjacency list
  cave_adj <- cave_edges |>
    bind_rows(tibble(node = cave_edges$dest,
                     dest = cave_edges$node)) |>
    group_by(node) |>
    summarise(dests = list(dest))
  
  cave_adjlist <- cave_adj$dests |>
    setNames(cave_adj$node)
  
  # """Your goal is to find the number of distinct paths
  # that start at start, end at end, and don't visit
  # small caves more than once. """
  
  # use depth first search to try all possible paths
  paths <- list()
  depth_first_search(start = 'start',
                     target = 'end',
                     path = c())
  return(paths)
}



# part 1 solution: # 4691
paths <- explore_cave('data/day12.txt')
print(length(paths))
# sample12 -> 10
# sample12_2 -> 19
# sample12_3 -> 226
# 



# --- Part Two ---

# After reviewing the available paths, you realize you might have time to visit a single small cave twice.

# Specifically, big caves can be visited any number of times,
# a single small cave can be visited at most twice,
# and the remaining small caves can be visited at most once.

# However, the caves named start and end can only be visited
# exactly once each: once you leave the start cave, you may not
# return to it, and once you reach the end cave, the path must
# end immediately.



explore_cave_part2 <- function(file) {
  
  depth_first_search <- function(start, target, path) {
    # add node to path
    path <- c(path, start)
    
    # a single small cave can be visited at most twice,
    if (
      any(
        map_lgl(.x = lil_caves, 
                .f = ~{sum(str_detect(path, .x)) > 2})
      )
    ){
      return(NULL)
    }
    # # the remaining small caves can be visited at most once.
    if ( map_lgl(.x = lil_caves,
                 .f = function(lilcave){
                   path[!str_detect(path, 'start|end')] |> 
                     str_detect(lilcave) |> sum() > 1
                 }) |>
         sum() > 1
    ){return(NULL)}
      
    # once you reach the end cave, the path must end immediately.
    if (start == target) {
      paths[length(paths) + 1] <<- list(path)
      return(NULL)
    }
    # otherwise continue to explore to dests
    dests <- cave_adjlist[start][[1]]
    for (dest in dests) {
      depth_first_search(dest, target, path)
    }
    return(NULL)
  }
  
  ## wrapper
  cave_edges <- parse_input(file)
  
  # directional adjacency list
  cave_adj <- cave_edges |>
    bind_rows(tibble(node = cave_edges$dest,
                     dest = cave_edges$node)) |>
    # once you leave the start cave, you may not return to it
    filter(dest != 'start') |>
    # other nodes -> dests
    group_by(node) |>
    summarise(dests = list(dest))
  
  lil_caves <- cave_adj |> 
    select(node) |> 
    filter(grepl('[a-z]+', node), !grepl('start|end', node)) |> 
    pull(node)
  
  cave_adjlist <- cave_adj$dests |>
    setNames(cave_adj$node)
  
  # """Your goal is to find the number of distinct paths
  # that start at start, end at end, and don't visit
  # small caves more than once. """
  
  # use depth first search to try all possible paths
  paths <- list()
  depth_first_search(start = 'start',
                     target = 'end',
                     path = c())
  return(paths)
}

## Solution part 2: 140718
paths <- explore_cave_part2(file = 'data/day12.txt')
print(length(paths))
beepr::beep()

