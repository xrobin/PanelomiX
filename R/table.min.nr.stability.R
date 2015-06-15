## Frequency of the number of molecules required for a positive test ##

table.min.nr.stability <- function(...) {
  UseMethod("table.min.nr.stability")
}

# table for a list of cross-validations (exhcvlist)
table.min.nr.stability.exhcvlist  <- function(exhcvlist) {
  nreps <- length(exhcvlist)
  #nrs <- rep(0, length(exhcvlist[[1]][[1]][[1]]$panels.of.num))
  #names(nrs) <- exhcvlist[[1]][[1]][[1]]$panels.of.num
  # Old weird version: why did I do that??? Now do it correctly
  nrs <- rep(0, max(exhcvlist[[1]][[1]][[1]]$panels.of.num))
  names(nrs) <- seq(1, max(exhcvlist[[1]][[1]][[1]]$panels.of.num))
  class(nrs) <- "table.min.nr.stability"
  for (reps in 1:nreps) {
    nrs <- nrs + table.min.nr.stability(exhcvlist[[reps]])
  }
  nrs/nreps
}

# table for a single cross-validation (exhcv)
table.min.nr.stability.exhcv  <- function(exhcv) {
  k <- length(exhcv)
  #nrs <- rep(0, length(exhcv[[1]][[1]]$panels.of.num))
  #names(nrs) <- exhcv[[1]][[1]]$panels.of.num
  # Old weird version: why did I do that??? Now do it correctly
  nrs <- rep(0, max(exhcv[[1]][[1]]$panels.of.num))
  names(nrs) <- seq(1, max(exhcv[[1]][[1]]$panels.of.num))
  class(nrs) <- "table.min.nr.stability"
  for (i in 1:k) {
    nrs <- nrs + table.min.nr.stability(exhcv[[i]])
  }
  nrs/k
}

# table for a single search with several outputs
table.min.nr.stability.exhlist <- function(exhlist) {
  if (is.null(names(exhlist))) {
    n.panels <- length(exhlist)
  }
  else {
    n.panels <- length(exhlist[names(exhlist)==""])
  }
  #nrs <- rep(0, length(exhlist[[1]]$panels.of.num))
  #names(nrs) <- exhlist[[1]]$panels.of.num
  # Old weird version: why did I do that??? Now do it correctly
  nrs <- rep(0, max(exhlist[[1]]$panels.of.num))
  names(nrs) <- seq(1, max(exhlist[[1]]$panels.of.num))
  class(nrs) <- "table.min.nr.stability"
  for (i in 1:n.panels) {
    panel <- exhlist[[i]]
    idx <- grep(paste("^", panel$min.nr, "$", sep=""), names(nrs))
    nrs[idx] <- nrs[idx] + 1
  }
  nrs/n.panels
}


plot.table.min.nr.stability <- function(x, main="Panel min nr frequency", ...) {
  opa <- par(las=3)
  barplot(c(x), main=main, ...)
  par(opa)
}

