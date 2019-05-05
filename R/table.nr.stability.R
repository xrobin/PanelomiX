#' Frequency of the number of molecules included 
#' @param object the panel model
#' @param ... additional arguments to and from other methods
#' @export
table.nr.stability <- function(object, ...) {
	UseMethod("table.nr.stability")
}

#' @rdname table.nr.stability
#' @export
table.nr.stability.exhcvlist  <- function(object, ...) {
	nreps <- length(object)
	nrs <- rep(0, length(object[[1]][[1]][[1]]$panels.of.num))
	names(nrs) <- object[[1]][[1]][[1]]$panels.of.num
	class(nrs) <- "table.nr.stability"
	for (reps in 1:nreps) {
		nrs <- nrs + table.nr.stability(object[[reps]])
	}
	nrs/nreps
}

#' @rdname table.nr.stability
#' @export
table.nr.stability.exhcv  <- function(object, ...) {
	k <- length(object)
	nrs <- rep(0, length(object[[1]][[1]]$panels.of.num))
	names(nrs) <- object[[1]][[1]]$panels.of.num
	class(nrs) <- "table.nr.stability"
	for (i in 1:k) {
		nrs <- nrs + table.nr.stability(object[[i]])
	}
	nrs/k
}

#' @rdname table.nr.stability
#' @export
table.nr.stability.exhlist <- function(object, ...) {
	if (is.null(names(object))) {
		n.panels <- length(object)
	}
	else {
		n.panels <- length(object[names(object)==""])
	}
	nrs <- rep(0, length(object[[1]]$panels.of.num))
	names(nrs) <- object[[1]]$panels.of.num
	class(nrs) <- "table.nr.stability"
	for (i in 1:n.panels) {
		panel <- object[[i]]
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

