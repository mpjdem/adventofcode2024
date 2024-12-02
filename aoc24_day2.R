## Advent of Code 2024, Day 2
## https://adventofcode.com/2024/day/2
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem

source("common.R")

## Get the input data
inp <- data.matrix(read.table(input_fpath(2), fill=TRUE, sep=" ", header=FALSE))

## -- PART 1 --
is_safe <- function(v_row) {
  v_diff = diff(v_row)
  v_sign = length(unique(sign(v_diff[!is.na(v_diff)]))) == 1
  v_size = all(abs(v_diff) <= 3, na.rm=TRUE)
  v_sign & v_size
}

solution_1 <- sum(apply(inp, 1, is_safe))

check_solution(2, 1, solution_1)

## -- PART 2 --
l_dropped = list()
for (to_drop in 1:ncol(inp)) {
  l_dropped[[to_drop]] = apply(inp[,1:ncol(inp) != to_drop], 1, is_safe)
}

solution_2 <- sum(apply(do.call(cbind, l_dropped), 1, any))

check_solution(2, 2, solution_2)
