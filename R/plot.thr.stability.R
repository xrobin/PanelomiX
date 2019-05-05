#' Plots the stability of thresholds over cross-validation
#' @param x the panels as an \code{exhlist}, single \code{exhcv} or \code{exhcvlist}
#' @param data data to be plotted
#' @param found.mols the vector of molecules that were found. Automatically extracted from \code{x} unless provided
#' @param add whether to just add to the plot
#' @param plot whether to actually plot that part
#' @param ... additional arguments to \code{\link{plot}}
#' @importFrom graphics axis barplot box legend par plot points
#' @rawNamespace S3method(plot, thr.stability)
#' @export plot.thr.stability
plot.thr.stability <- function(...) {
	UseMethod("plot.thr.stability")
}

plot.new.thr.stability <- function(found.mols, data, main="Panel thresholds", exhobject=NULL, smart.mol.list=NULL, legend=TRUE, ...) {
	opa <- par(las=3)
	plot(NA, NA, xlim=c(1, length(found.mols)), ylim=c(0, dim(data)[1]), xlab="", ylab="threshold rank", main=main, axes=F, ...)
	box()
	axis(2)
	if (! is.null(smart.mol.list))
		names(found.mols) <- sapply(names(found.mols), function(x, sml) {return(sml[[x]])}, sml=smart.mol.list)
	axis(1, at=1:length(found.mols), labels=names(found.mols))
	if (!is.null(exhobject) && legend) {
		min.nrs <- table.min.nr.stability(exhobject)
		min.nrs <- min.nrs[min.nrs!=0]
		legend("bottomright", title="Min nr", legend=names(min.nrs), fill=as.numeric(names(min.nrs))+1)
	}
	par(opa)
}

#' @rdname plot.thr.stability
#' @method plot.thr.stability exhlist
#' @export
plot.thr.stability.exhlist <- function(x, data=NULL, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
	if (is.null(found.mols)) {
		found.mols <- table.mol.stability(x)
		found.mols <- found.mols[found.mols>0]
		found.mols <- sort(found.mols, decreasing=TRUE)
	}
	if (is.null(data)) {
		if (is.null(x$train.data)) {
			if (is.null(x[1]$train.data)) {
				stop("No data found in plot.thr.stability.exhlist")
			}
			else {
				data <- x[[1]]$train.data
			}
		}
		else {
			data <- x$train.data
		}
	}
	if (!add) {
		plot.new.thr.stability(found.mols, data, exhobject=x, ...)
	}
	if (plot) {
		# Filter exhlist so that we do not have training.data and test.data
		if (is.null(names(x))) {
			n.panels <- length(x)
		}
		else {
			n.panels <- length(x[names(x)==""])
		}
		# Plot the line for each panel
		for (i in 1:n.panels) {
			panel <- x[[i]]
			x <- y <- c()
			for (j in 1:length(names(panel$thresholds))) {
				y <- c(y, floor(rank(c(panel$thresholds[j], data[[names(panel$thresholds)[j]]])))[1])
				x <- c(x, grep(names(panel$thresholds)[j], names(found.mols)))
			}
			idx <- order(x)
			points(x[idx], y[idx], type="o", col=panel$min.nr+1, ...)
		}
	}
}

#' @rdname plot.thr.stability
#' @method plot.thr.stability exhcv
#' @export
plot.thr.stability.exhcv <- function(x, data, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
	if (is.null(found.mols)) {
		found.mols <- table.mol.stability(x)
		found.mols <- found.mols[found.mols>0]
		found.mols <- sort(found.mols, decreasing=TRUE)
	}
	if (!add) {
		plot.new.thr.stability(found.mols, data, exhobject=x, ...)
	}
	if (plot) {
		k <- length(x)
		for (i in 1:k) {
			plot.thr.stability(x[[i]], data=data, found.mols=found.mols, add=TRUE, ...)
		}
	}
}

#' @rdname plot.thr.stability
#' @method plot.thr.stability exhcvlist
#' @export
plot.thr.stability.exhcvlist <- function(x, data, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
	if (is.null(found.mols)) {
		found.mols <- table.mol.stability(x)
		found.mols <- found.mols[found.mols>0]
		found.mols <- sort(found.mols, decreasing=TRUE)
	}
	if (!add) {
		plot.new.thr.stability(found.mols, data, exhobject=x, ...)
	}
	if (plot) {
		nreps <- length(x)
		for (rep in 1:nreps) {
			plot.thr.stability(x[[rep]], data=data, found.mols=found.mols, add=TRUE, ...)
		}
	}
}

