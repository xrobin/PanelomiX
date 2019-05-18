#' Frequency of the choice of molecules 
#' @param object the panel model
#' @param ... additional arguments to and from other methods
#' @export
#' @examples 
#' data(aSAH, package = "pROC")
#' cv <- exh.train.cv(aSAH, c("age", "s100b", "ndka"), "outcome", progress=FALSE)
#' table.mol.stability(cv)
table.mol.stability <- function(object, ...) {
	UseMethod("table.mol.stability")
}

#' @rdname table.mol.stability
#' @export
table.mol.stability.exhcvlist  <- function(object, ...) {
	nreps <- length(object)
	res <- rep(0, length(attr(object[[1]][[1]], "all.predictors")))
	names(res) <- attr(object[[1]][[1]], "all.predictors")
	class(res) <- "table.mol.stability"
	for (reps in 1:nreps) {
		res <- res + table.mol.stability(object[[reps]])
	}
	res/nreps
}


#' @rdname table.mol.stability
#' @export
table.mol.stability.exhcv  <- function(object, ...) {
	k <- length(object)
	res <- rep(0, length(attr(object[[1]], "all.predictors")))
	names(res) <- attr(object[[1]], "all.predictors")
	class(res) <- "table.mol.stability"
	for (i in 1:k) {
		res <- res + table.mol.stability(object[[i]])
	}
	res/k
}


#' @rdname table.mol.stability
#' @export
table.mol.stability.exhlist <- function(object, ...) {
	if (is.null(names(object))) {
		n.panels <- length(object)
	}
	else {
		n.panels <- length(object[names(object)==""])
	}
	all.predictors <- attr(object, "all.predictors")
	res <- rep(0, length(all.predictors))
	names(res) <- all.predictors
	class(res) <- "table.mol.stability"
	for (i in 1:n.panels) {
		panel <- object[[i]]
		res <- res + as.numeric(all.predictors%in%names(panel$thresholds))
	}
	res/n.panels
}

plot.table.mol.stability <- function(x, main="Marker selection frequency", ...) {
	opa <- par(las=3)
	barplot(sort(x, decreasing=T), main=main, ...)
	par(opa)
}

