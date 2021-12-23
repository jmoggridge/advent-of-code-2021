
library(dplyr)

# each lanternfish creates a new lanternfish once every 7 days.
# model each fish as a single number that represents the number of days until it creates a new lanternfish.

# a new lanternfish would surely need slightly longer before it's capable of producing more lanternfish: two more days for its first cycle.

# After one day, its internal timer would become 2.
# ...
# After another day, its internal timer would become 0.
# After another day, its internal timer would reset to 6, and it would create a new lanternfish with an internal timer of 8.
# After another day, the first lanternfish would have an internal timer of 5, and the second lanternfish would have an internal timer of 7.


count_fish <- function(file){
  readLines('data/day06.txt') |> 
    stringr::str_split(',', simplify = T) |>
    as.numeric() |> 
    tibble() |> 
    setNames('days') |> 
    count(days)
  }

simulated_fishes <- function(fish, days){
  total <- list()[0:days]
  for (t in 1:days){
    oldfish <- fish |> filter(days == 0) |> mutate(days = 7)
    newfish <- fish |> filter(days == 0) |> mutate(days = 9)
    fish <- fish |> 
      filter(!days == 0) |> 
      bind_rows(oldfish, newfish) |> 
      mutate(days = days - 1) |> 
      group_by(days) |> 
      summarise(n = sum(n))
    total[t] <- sum(fish$n)
  }  
  return(total[days][[1]])
}


fish <- count_fish('data/day06.txt')
p1 <- simulated_fishes(fish, 80)
print(p1)


# --- Part Two ---
#   Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?
#   
#   After 256 days in the example above, there would be a total of 26984457539 lanternfish!
#   
#   How many lanternfish would there be after 256 days?
p2 <- simulated_fishes(fish, 256)
sprintf(fmt = "%.0f", p2) # 1746710169834

