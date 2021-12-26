# --- Day 10: Syntax Scoring ---

# the navigation subsystem (your puzzle input):

# The navigation subsystem syntax is made of several lines containing chunks.
# There are one or more chunks on each line, and chunks contain zero or more
# other chunks.
# Adjacent chunks are not separated by any delimiter; if one chunk stops,
# the next chunk (if any) can immediately start. Every chunk must
# open and close with one of four legal pairs of matching characters:

#' - If a chunk opens with (, it must close with ).
#' - If a chunk opens with [, it must close with ].
#' - If a chunk opens with {, it must close with }.
#' - If a chunk opens with <, it must close with >.
#'  ({[<>]})
#'
#' So, () is a legal chunk that contains no other chunks, as is [].
#' More complex but valid chunks include ([]), {()()()}, <([{}])>,
#'  [<>({}){}[([])<>]], and even (((((((((()))))))))).

# Some lines are incomplete, but others are corrupted.
# Find and discard the corrupted lines first.

#' A corrupted line is one where a chunk closes with the wrong character -
#' that is, where the characters it opens and closes with do not form one
#' of the four legal pairs listed above.

#' Examples of corrupted chunks include (], {()()()>, (((()))}, and
#' <([]){()}[{}]).
#' Such a chunk can appear anywhere within a line, and its presence causes
#'  the whole line to be considered corrupted.

# For example, consider the following navigation subsystem:

#' [({(<(())[]>[[{[]{<()<>>
#' [(()[<>])]({[<{<<[]>>(
#' {([(<{}[<>[]}>{[]{[(<()>
#' (((({<>}<{<{<>}{[]{[]{}
#' [[<[([]))<([[{}[[()]]]
#' [{[{({}]{}}([{[{{{}}([]
#' {<[[]]>}<{[{[{[]{()[[[]
#' [<(<(<(<{}))><([]([]()
#' <{([([[(<>()){}]>(<<{{
#' <{([{{}}[<[[[<>{}]]]>[]]
#' Some of the lines aren't corrupted, just incomplete; you can ignore these
#' lines for now. The remaining five lines are corrupted:
#' {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
#'   [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
#'  [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
#'  [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
#'  <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
#'  Stop at the first incorrect closing character on each corrupted line.
#'
#'  Did you know that syntax checkers actually have contests to see who can get
#'  the high score for syntax errors in a file? It's true! To calculate the
#'   syntax error score for a line, take the first illegal character on the
#'   line and look it up in the following table:
#'
#' ): 3 points.
#' ]: 57 points.
#' }: 1197 points.
#' >: 25137 points.
#'



library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

check_code_corruption <- function(code) {
  brackets <- list(
    '<' = '>',
    '{' = '}',
    '[' = ']',
    '(' = ')'
  )
  opens <- list()
  result <- 'ok'
  
  for (i in seq_along(code)) {
    char <- code[i]
    if (char %in% names(brackets)) {
      opens <- append(opens, char)
    } else {
      brackets[opens[length(opens)][[1]]]
      if (char == brackets[opens[length(opens)][[1]]]) {
        opens[length(opens)] <- NULL
      } else{
        result <- char
        break
      }
    }
  }
  return(result)
}

points <-
  tribble( ~ type, ~ points,
           ')', 3,
           ']', 57,
           '}', 1197,
           '>', 25137)


### Part 1 solution
readLines('data/day10.txt') |>
  as_tibble() |>
  mutate(code = map(value,  ~ str_trim(.x) |> str_split('', simplify = T))) |> 
  mutate(corrupt = map_chr(code, check_code_corruption)) |> 
  filter(corrupt != 'ok') |> 
  left_join(points, by = c('corrupt' = 'type')) |> 
  summarise(solution = sum(points))
  # 469755












