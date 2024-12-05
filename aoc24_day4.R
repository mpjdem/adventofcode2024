## Advent of Code 2024, Day 4
## https://adventofcode.com/2024/day/4
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem

source("common.R")

library(data.table)

# Read input data
inp <- strsplit(readLines(input_fpath(4)), "")

## -- PART 1 --
# Transform into a coordinate table of individual characters
dt <- lapply(1:length(inp), \(i) {
  data.table(x=i, y=1:length(inp[[i]]), char=inp[[i]])
}) |> rbindlist()

# Function to count matches of XMAS or SAMX for a single string
count_matches <- function(s) {
  matches_idx = cbind(gregexec("XMAS", s)[[1]], gregexec("SAMX", s)[[1]])
  length(matches_idx[matches_idx != -1])
}

# Count in every direction. For diagonals we shift x by y (in both directions)
count_horizontal <- dt[order(x), count_matches(paste0(char, collapse="")), by = "y"][, sum(V1)]
count_vertical<- dt[order(y), count_matches(paste0(char, collapse="")), by = "x"][, sum(V1)]

dt[, shiftx_right := x + y]
dt[, shiftx_left := x - y]
count_diagonal1 <- dt[order(y), count_matches(paste0(char, collapse="")), by = "shiftx_right"][, sum(V1)]
count_diagonal2 <- dt[order(y), count_matches(paste0(char, collapse="")), by = "shiftx_left"][, sum(V1)]

solution_1 <- count_horizontal + count_vertical + count_diagonal1 + count_diagonal2
check_solution(4, 1, solution_1)

## -- PART 2 --
# For every central 'A' character, find the two characters adjacent on a diagonal
dt_diagonal1 <- merge(dt[char=="A"],  dt,  by="shiftx_left",  allow.cartesian=TRUE, suffixes = c(".central", ".diag"))[abs(y.central - y.diag) == 1]
dt_diagonal2 <- merge(dt[char=="A"],  dt,  by="shiftx_right",  allow.cartesian=TRUE, suffixes = c(".central", ".diag"))[abs(y.central - y.diag) == 1]

# Find all instances of MS or SM on both diagonals of the central A, and that's our answer 
solution_2 <- merge(
  dt_diagonal1[order(y.diag), paste0(char.diag, collapse=""), by=.(x.central, y.central)][V1 %in% c("MS","SM")],
  dt_diagonal2[order(y.diag), paste0(char.diag, collapse=""), by=.(x.central, y.central)][V1 %in% c("MS","SM")],
  all=FALSE, by=c("x.central", "y.central")
)[, .N]

check_solution(4, 2, solution_2)