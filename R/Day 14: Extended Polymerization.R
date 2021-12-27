# --- Day 14: Extended Polymerization ---


library(dplyr)
library(tidyr)
library(stringr)
library(glue)

get_rules <- function(file){
  readLines(file)[-c(1:2)] |>
    as_tibble() |>
    separate(value, into = c('old', 'to'), sep = ' -> ') |>
    mutate(left = glue("{str_extract(old, '^.')}{to}"),
           right = glue("{to}{str_extract(old, '.$')}")) |> 
    pivot_longer(cols = c(left, right), values_to = 'new') |> 
    select(-to, -name) |>
    group_by(old) |> 
    nest(new = new) |> 
    arrange(old)
}

# 
# ## need a pair replacement approach...
polymerize <- function(template, rules, steps) {
  # convert template to 2mers
  polymer <- template |> 
    str_split('') |>
    unlist() |>
    as_tibble() |> 
    mutate(leadval = lead(value),
           old = glue("{value}{lead(value)}")) |>
    filter(!is.na(leadval)) |>
    select(old) |> 
    group_by(old) |> 
    count()
  # repeatedly translate 2mers and sum
  for (step in 1:steps){
    polymer <- polymer |> 
      left_join(rules, by = "old") |> 
      unnest(new) |> 
      transmute(old = new, n) |> 
      group_by(old) |> 
      summarise(n = sum(n)) |> 
      ungroup()
  }
  # convert to single (first) letter count and add single last letter from tempalte
  rs <- polymer |> 
    transmute(letter = str_extract(old, '^.'), n = n) |> 
    bind_rows(tibble(letter = last_letter, n = 1L)) |> 
    group_by(letter) |> 
    summarize(n= sum(n)) |> 
    arrange(desc(n))
  
  digits <- rs |> 
    slice(1, n()) |> 
    pull(n)
 
  return( digits[1]-digits[2])
}

file <- 'data/day14.txt'
template <- readLines(file)[1]
last_letter <- str_extract(template, '.$')
rules <-  get_rules(file) 
polymerize(template, rules, steps = 10)
polymerize(template, rules, steps = 40) |> sprintf(fmt = '%0.f')













