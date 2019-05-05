#' Frequency of the number of molecules required for a positive test 
#' @param object the panel model
#' @param ... additional arguments to and from other methods
#' @export
table.min.nr.stability <- function(object, ...) {
	UseMethod("table.min.nr.stability")
}

#' @rdname table.min.nr.stability
#' @export
table.min.nr.stability.exhcvlist  <- function(object, ...) {
	nreps <- length(object)
	#nrs <- rep(0, length(exhcvlist[[1]][[1]][[1]]$panels.of.num))
	#names(nrs) <- exhcvlist[[1]][[1]][[1]]$panels.of.num
	# Old weird version: why did I do that??? Now do it correctly
	nrs <- rep(0, max(object[[1]][[1]][[1]]$panels.of.num))
	names(nrs) <- seq(1, max(object[[1]][[1]][[1]]$panels.of.num))
	class(nrs) <- "table.min.nr.stability"
	for (reps in 1:nreps) {
		nrs <- nrs + table.min.nr.stability(object[[reps]])
	}
	nrs/nreps
}

#' @rdname table.min.nr.stability
#' @export
table.min.nr.stability.exhcv  <- function(object, ...) {
	k <- length(object)
	#nrs <- rep(0, length(exhcv[[1]][[1]]$panels.of.num))
	#names(nrs) <- exhcv[[1]][[1]]$panels.of.num
	# Old weird version: why did I do that??? Now do it correctly
	nrs <- rep(0, max(object[[1]][[1]]$panels.of.num))
	names(nrs) <- seq(1, max(object[[1]][[1]]$panels.of.num))
	class(nrs) <- "table.min.nr.stability"
	for (i in 1:k) {
		nrs <- nrs + table.min.nr.stability(object[[i]])
	}
	nrs/k
}

#' @rdname table.min.nr.stability
#' @export
table.min.nr.stability.exhlist <- function(object, ...) {
	if (is.null(names(object))) {
		n.panels <- length(object)
	}
	else {
		n.panels <- length(object[names(object)==""])
	}
	nrs <- rep(0, max(object[[1]]$panels.of.num))
	names(nrs) <- seq(1, max(object[[1]]$panels.of.num))
	class(nrs) <- "table.min.nr.stability"
	for (i in 1:n.panels) {
		panel <- object[[i]]
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

