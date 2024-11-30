#' Compose the file path of an input file
#'
#' @param day Which day of AoC (1-25)
#'
#' @return A `character` string
#'
input_fpath <- function(day, part) {

  stopifnot(day %in% 1:25)
  
  if (!dir.exists("input")) {
    stop("Directory `input` does not exist!")
  }

  res <- file.path("input", paste0("input", day, ".txt"))

  if (!file.exists(res)) {
    stop(paste0("File ", res, " could not be found!"))
  }

  res

}

#' Compose the file path of a solution file
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#'
#' @return A `character` string
#'
solution_fpath <- function(day, part) {

  stopifnot(day %in% 1:25)
  stopifnot(part %in% 1:2)

  if (!dir.exists("output")) {
    stop("Directory `output` does not exist!")
  }

  res <- file.path("output", paste0("output", day, "_", part, ".txt"))

  if (!file.exists(res)) {
    stop(paste0("File ", res, " could not be found!"))
  }

}

#' Check that a solution is correct, against a file in `output/`
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#' @param solution The answer to check
#'
#' @return `NULL`
#'
check_solution <- function(day, part, solution) {
  cat(paste0("Solution to Part ", part, ": ", solution, " - "))
  correct_solution <- readLines(solution_fpath(day, part))
  if (as.character(solution) == correct_solution) cat("correct!\n") else cat("wrong!\n")
  invisible()
}

#' Write a solution as the correct answer in `output/`
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#' @param solution The answer to write
#'
#' @return `NULL`
#'
write_solution <- function(day, part, solution) {
  writeLines(as.character(solution), solution_fpath(day, part))
  invisible()
}