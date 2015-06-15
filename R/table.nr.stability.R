## Frequency of the number of molecules included ##

table.nr.stability <- function(...) {
  UseMethod("table.nr.stability")
}

# table for a list of cross-validations (exhcvlist)
table.nr.stability.exhcvlist  <- function(exhcvlist) {
  nreps <- length(exhcvlist)
  nrs <- rep(0, length(exhcvlist[[1]][[1]][[1]]$panels.of.num))
  names(nrs) <- exhcvlist[[1]][[1]][[1]]$panels.of.num
  class(nrs) <- "table.nr.stability"
  for (reps in 1:nreps) {
    nrs <- nrs + table.nr.stability(exhcvlist[[reps]])
  }
  nrs/nreps
}

# table for a single cross-validation (exhcv)
table.nr.stability.exhcv  <- function(exhcv) {
  k <- length(exhcv)
  nrs <- rep(0, length(exhcv[[1]][[1]]$panels.of.num))
  names(nrs) <- exhcv[[1]][[1]]$panels.of.num
  class(nrs) <- "table.nr.stability"
  for (i in 1:k) {
    nrs <- nrs + table.nr.stability(exhcv[[i]])
  }
  nrs/k
}

# table for a single search with several outputs
table.nr.stability.exhlist <- function(exhlist) {
  if (is.null(names(exhlist))) {
    n.panels <- length(exhlist)
  }
  else {
    n.panels <- length(exhlist[names(exhlist)==""])
  }
  nrs <- rep(0, length(exhlist[[1]]$panels.of.num))
  names(nrs) <- exhlist[[1]]$panels.of.num
  class(nrs) <- "table.nr.stability"
  for (i in 1:n.panels) {
    panel <- exhlist[[i]]
    idx <- grep(paste("^", length(names(panel$thresholds)), "$", sep=""), names(nrs))
    nrs[idx] <- nrs[idx] + 1
  }
  nrs/n.panels
}


plot.table.nr.stability <- function(x, main="Panel size frequency", ...) {
  opa <- par(las=3)
  barplot(c(x), main=main, ...)
  par(opa)
}

