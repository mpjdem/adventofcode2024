## Advent of Code 2024, Day 5
## https://adventofcode.com/2024/day/5
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem

source("common.R")

`%||%` <- function(x, default) if (is.null(x)) default else x
middle_element <- function(v) v[(length(v) + 1) / 2]

# Read input data
inp <- readLines(input_fpath(5))
inp_rules <- inp[1:(which(inp=="") - 1)]
inp_updates <- inp[(which(inp=="") + 1):length(inp)]

## -- PART 1 --
rules <- list()
for (rule in strsplit(inp_rules, "|", TRUE)) {
  if (is.null(rules[[rule[1]]])) {
    rules[[rule[1]]] <- rule[2]
  } else {
    rules[[rule[1]]] <- c(rules[[rule[1]]], rule[2])
  }
}

updates <- strsplit(inp_updates, ",", TRUE)

check_rules <- function(rules, update) {
  Reduce(function(prev, i) {
    prev & all(match(rules[[update[i]]] %||% "", update) > i, na.rm=TRUE)
  }, 1:length(update), init=TRUE)
}

correctly_ordered <- sapply(updates, check_rules, rules=rules)
solution_1 <- sum(as.integer(sapply(updates[correctly_ordered], middle_element)))

check_solution(5, 1, solution_1)

## -- PART 2 --
incorrect_updates <- updates[!correctly_ordered]

insert_page <- function(update, move_value_at_idx, insert_before_idx) {
  append(update[update!=update[move_value_at_idx]], update[move_value_at_idx], after=insert_before_idx-1)
}

reorder_pages <- function(rules, update) {
  for (page in update) {
    # The page at i is the page which should be before the pages matching its rules
    i <- which(page == update)
    rule_match_idx <- match(rules[[page]] %||% "", update, nomatch=999)
    violating_pages <- rules[[page]][rule_match_idx < i]
    if (length(violating_pages) > 0) {
      # Insert the page at i before the earliest violating page
      insert_before_idx <- min(match(violating_pages, update))
      update <- insert_page(update, i, insert_before_idx)
    }
  }
  update
}

correctly_ordered_updates <- lapply(incorrect_updates, reorder_pages, rules=rules)
stopifnot(all(sapply(correctly_ordered_updates, check_rules, rules=rules)))
solution_2 <- sum(as.integer(sapply(correctly_ordered_updates, middle_element)))

check_solution(5, 2, solution_2)
