t0 <- Sys.time()

for (aoc_day in 1:25) {

  fname <- paste0("aoc24_day", aoc_day, ".R")

  if (file.exists(fname)) {
    cat("** Day", aoc_day, "**\n")
    t0_day <- Sys.time()
    source(fname, local = new.env())
    cat(" \n")
    print(Sys.time() - t0_day)
    cat(" \n")
  } else {
    cat(paste0("Code for day ", aoc_day, " not found\n"))
  }

}

cat("** Total running time **\n")
print(Sys.time() - t0)