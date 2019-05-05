#' Plot the MDS of a panel
#' @param x an train panel
#' @param newdata the data to plot
#' @param xlab,ylab,main labels for the plot
#' @param ask ask to create new pages of the plot
#' @param ... further arguments for \code{\link{plot}}
#' @importFrom grDevices devAskNewPage dev.interactive
#' @importFrom graphics abline legend plot
#' @importFrom stats cmdscale dist na.omit
#' @export 
plot.exh <- function(x, newdata=NULL, xlab="First MDS dimension", ylab="Second MDS dimension", main="MDS on PRIM", ask = dev.interactive(), ...) {
	if (ask) {
		oask <- devAskNewPage(TRUE)
		on.exit(devAskNewPage(oask))
	}
	predictors <- x$panel
	if (is.null(newdata)) data <- attr(x, "exhlist")$train.data
	else data <- newdata
	#print(ifelse(is.null(newdata), , newdata))
	#print(is.null(newdata))
	#stop()
	dat <- data[predictors]
	#  dat = log(1+dat) # 1+log transform
	#dat = t((t(dat)-mean(dat))/sd(dat)) # Z transform
	d <- dist(dat)
	mds <- cmdscale(d)
	
	col <- calcColors(data[[x$response]], x$levels)
	pch <- 21
	plot(mds[,1:2], type="p", xlab = xlab, ylab = ylab, col=col, pch=pch, main=main, ...)
	legend("bottomright",legend=x$levels, fill=c("green", "red"), title="Response")
	
	for (i in 1:length(predictors)) {
		for (j in i+1:length(predictors)) {
			if (j>length(predictors)) {
				break;
			}
			plot(dat[[predictors[i]]],dat[[predictors[j]]],
				 col=col,pch=pch,
				 xlab=predictors[i], ylab=predictors[j], log="xy", main="Markers 2 by 2")
			abline(v=x$thresholds[[i]])
			abline(h=x$thresholds[[j]])
		}
	}
}


#' Plot diagnosis on cross validation
#' @param x the cross-validation of a panel (\code{\link{exh.train.cv}})
#' @param data some data for the plot
#' @param training.panels not sure about that one
#' @param ... ignored
#' @export plot.cv.diag
#' @importFrom graphics par
#' @rawNamespace S3method(plot, cv.diag)
plot.cv.diag <- function(x, data, training.panels, ...) {
	
	par(mar=c(10, 4, 2, 0)+.1)
	
	#table.mol.stability(x)
	ts <- table.mol.stability(x)
	plot(ts)
	
	#table.nr.stability(x)
	plot(table.nr.stability(x))
	
	#table.min.nr.stability(x)
	plot(table.min.nr.stability(x))
	
	plot.thr.stability(x, data)
	
	training.pred <- predict(training.panels)
	response <- training.panels[[1]]$train.data[[training.panels[[1]]$response]]
	roc(response, training.pred, levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE)
	
	# CV ROC
	cv.pred <- predict(x)
	roc(attr(cv.pred, "response"), cv.pred, levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE, add=TRUE, col="grey")
	
	# Single mols
	best.mols <- names(sort(ts, decreasing=TRUE))
	roc(data[[training.panels[[1]]$response]], data[[best.mols[1]]], levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE, add=TRUE, col="#FF0000")
	if (length(best.mols) >= 2)
		roc(data[[training.panels[[1]]$response]], data[[best.mols[2]]], levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE, add=TRUE, col="#00FF00")
	if (length(best.mols) >= 3)
		roc(data[[training.panels[[1]]$response]], data[[best.mols[3]]], levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE, add=TRUE, col="#0000FF")
	
	# ROC legend	
	legend("bottomright", legend=c("Training", "Cross-validation", na.omit(best.mols[1:3])), lwd=2, col=c("black", "grey", c("#FF0000", "#00FF00", "#0000FF")[1:min(3, length(best.mols))]))
}
