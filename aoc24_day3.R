## Advent of Code 2024, Day 3
## https://adventofcode.com/2024/day/3
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem

source("common.R")

library(data.table)

## Get the input data
inp <- readLines(input_fpath(3))

## -- PART 1 --
l_matches_idx <- gregexec("mul\\(([0-9]+),([0-9]+)\\)", inp)
l_matches <- regmatches(inp, l_matches_idx)
matches <- do.call(cbind, l_matches)

solution_1 <- sum(as.integer(matches[2,]) * as.integer(matches[3,]))

check_solution(3, 1, solution_1)

## -- PART 2 --
l_matches_do_idx <- gregexec("do\\(\\)", inp)
l_matches_dont_idx <- gregexec("don't\\(\\)", inp)

# Put all match indices in one big table
dt <- lapply(1:length(inp), function(i) {
  rbind(
    data.table(
      line = i,
      idx = l_matches_idx[[i]][1,],
      val1 = as.integer(l_matches[[i]][2,]),
      val2 = as.integer(l_matches[[i]][3,])
    ),
    data.table(
      line = i,
      idx = l_matches_do_idx[[i]][1,],
      enable = 1
    ),
    data.table(
      line = i,
      idx = l_matches_dont_idx[[i]][1,],
      enable = 0
    ),
    fill=TRUE
  )
}) |> rbindlist(fill=TRUE)

# Fill forward the 'enable' flag with 1 as a starting value
dt <- dt[order(line, idx)]
dt[, enabled := nafill(enable, "locf", 1)]

# Only sum up enabled multiplications
solution_2 <- dt[enabled == 1, sum(val1 * val2, na.rm =TRUE)]

check_solution(3, 2, solution_2)
