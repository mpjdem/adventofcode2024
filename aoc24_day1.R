## Advent of Code 2024, Day 1
## https://adventofcode.com/2024/day/1
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem

source("common.R")

library(data.table)

## Get the input data
dt <- fread(input_fpath(1), sep=" ")

## -- PART 1 --
solution_1 <- dt[, sum(abs(sort(V1) - sort(V2)))]

check_solution(1, 1, solution_1)

## -- PART 2 --
solution_2 <- merge(
  dt[, .(V1)], 
  dt[, .N, by=.(V2)], 
  by.x="V1", by.y="V2", all=FALSE
)[, sum(V1 * N)]

check_solution(1, 2, solution_2)
