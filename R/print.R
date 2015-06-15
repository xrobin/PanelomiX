print.exhlist <- function(x, file="", append=FALSE, ...) {
   if (!is.null(names(x))) {
     x <- x[names(x)==""]
   }
  cat("", file=file, append=append)
  print.exh.params(x[[1]], file=file, append=TRUE)
  print.exh.timing(x[[1]], file=file, append=TRUE)
  print.exh.iterations(x[[1]], file=file, append=TRUE)
  print.exh.perf(x[[1]], file=file, append=TRUE)
  for(i in 1:length(x)) {
    cat("[[", i, "]]\n", sep="", file=file, append=TRUE)
    print.exh.thresholds(x[[i]], file=file, append=TRUE)
  }
}

print.exhcvlist <- function(x, file="", append=FALSE, ...) {
  cat("", file=file, append=append)
  cat(length(x), "x", length(x[[1]]), "-fold cross-validation\n", sep="", file=file, append=append)
  print.exh.params(attributes(x[[1]][[1]]), file=file, append=TRUE)
  print.exh.timing(x, file=file, append=TRUE)
}

print.exhcv <- function(x, file="", append=FALSE, ...) {
  cat("", file=file, append=append)
  cat(length(x), "-fold cross-validation\n", sep="", file=file, append=append)
  print.exh.params(attributes(x[[1]]), file=file, append=TRUE)
  print.exh.timing(x, file=file, append=TRUE)
}

print.exh <- function(x, file = "", append = FALSE) {
  print.exh.params(x, file=file, append=TRUE)
  print.exh.timing(x, file=file, append=TRUE)
  print.exh.iterations(x, file=file, append=TRUE)
  print.exh.perf(x, file=file, append=TRUE)
  print.exh.thresholds(x, file=file, append=TRUE)
}

print.exh.params <- function(x, file = "", append = FALSE) {
  cat("Training parameters:", "\n", file=file, append=append)
  cat("                     min.constr = ", x$min.constr, " (", x$constrain.on, ")", "\n", file=file, append=TRUE, sep="")
  cat("                     predictors =", x$predictors, "\n", file=file, append=TRUE)
  if (length(x$fixed.predictors) > 0)
    cat("                     fixed predictors =", x$fixed.predictors, "\n", file=file, append=TRUE)
  cat("                     response =", x$response, "\n", file=file, append=TRUE)
  cat("                     levels =", x$levels, "\n", file=file, append=TRUE)
}

print.exh.iterations <- function(x, file = "", append = FALSE) {
  cat("                     iterations =", x$nr.iter, "\n", file=file, append=TRUE)
}


print.exh.timing <- function(x, file = "", append = FALSE) {
  diff <- x$time.end - x$time.start
  time.end <- x$time.end
  if (length(diff) == 0) { # cas of exhcvlist & exhcv objects
    diff <- attr(x, "time.end") - attr(x, "time.start")
    time.end <- attr(x, "time.end")
  }
  diff.text <- sprintf("%.2f %s", as.numeric(diff), units(diff))
  end.date <- format(as.POSIXlt(time.end, tz="GMT"), "%Y-%m-%d %H:%M %Z")
  cat("Executed in ", diff.text, " (ended on ", end.date, ")\n", file=file, append=append, sep="")
}

print.exh.perf <- function(x, file = "", append = FALSE) {
  cat("Performance on training set (", x$nr.patients, " patients: ", x$nr.class[[as.character(x$levels[1])]], " ", x$levels[1], ", ", x$nr.class[[as.character(x$levels[2])]], " ", x$levels[2], "):\n", file=file, append=TRUE, sep="")
  cat("                     sensitivity =", sprintf("%.3f", x$sensitivity), "\n", file=file, append=TRUE)
  cat("                     specificity =", sprintf("%.3f", x$specificity), "\n", file=file, append=TRUE)
}

print.exh.thresholds <- function(x, file = "", append = FALSE) {
  cat("Thresholds:\n", file=file, append=TRUE)
  cat("                     ", paste(names(x$thresholds), unlist(x$directions[names(x$thresholds)]), signif(x$thresholds), sep=" ", collapse="\n                     "), "\n", sep="", file=file, append=TRUE)
  cat("                     positive when >= ", x$min.nr, ".\n", sep="", file=file, append=TRUE)
}

