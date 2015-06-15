#### STABILITY TABLES AND PLOTS ####

## Frequency of the choice of molecule ##

table.mol.stability <- function(...) {
	UseMethod("table.mol.stability")
}

# table for a list of cross-validations (exhcvlist)
table.mol.stability.exhcvlist  <- function(exhcvlist) {
	nreps <- length(exhcvlist)
	res <- rep(0, length(exhcvlist[[1]][[1]][[1]]$all.predictors))
	names(res) <- exhcvlist[[1]]$all.predictors
	class(res) <- "table.mol.stability"
	for (reps in 1:nreps) {
		res <- res + table.mol.stability(exhcvlist[[reps]])
	}
	res/nreps
}

# table for a single cross-validation (exhcv)
table.mol.stability.exhcv  <- function(exhcv) {
	k <- length(exhcv)
	res <- rep(0, length(exhcv[[1]][[1]]$all.predictors))
	names(res) <- exhcv[[1]]$all.predictors
	class(res) <- "table.mol.stability"
	for (i in 1:k) {
		res <- res + table.mol.stability(exhcv[[i]])
	}
	res/k
}

# table for a single search with several outputs
table.mol.stability.exhlist <- function(exhlist) {
	if (is.null(names(exhlist))) {
		n.panels <- length(exhlist)
	}
	else {
		n.panels <- length(exhlist[names(exhlist)==""])
	}
	res <- rep(0, length(exhlist[[1]]$all.predictors))
	names(res) <- exhlist[[1]]$all.predictors
	class(res) <- "table.mol.stability"
	for (i in 1:n.panels) {
		panel <- exhlist[[i]]
		res <- res + as.numeric(panel$all.predictors%in%names(panel$thresholds))
	}
	res/n.panels
}

plot.table.mol.stability <- function(x, main="Marker selection frequency", ...) {
	opa <- par(las=3)
	barplot(sort(x, decreasing=T), main=main, ...)
	par(opa)
}

