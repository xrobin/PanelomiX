# Plot MDS (kind of PCA)
plot.exh <- function(panel, newdata=NULL, xlab="First MDS dimension", ylab="Second MDS dimension", main="MDS on PRIM", ask = dev.interactive(), ...) {
	if (ask) {
		oask <- devAskNewPage(TRUE)
		on.exit(devAskNewPage(oask))
	}
	predictors <- panel$panel
	if (is.null(newdata)) data <- attr(panel, "exhlist")$train.data
	else data <- newdata
	#print(ifelse(is.null(newdata), , newdata))
	#print(is.null(newdata))
	#stop()
	dat <- data[predictors]
	#  dat = log(1+dat) # 1+log transform
	#dat = t((t(dat)-mean(dat))/sd(dat)) # Z transform
	d <- dist(dat)
	mds <- cmdscale(d)
	
	col <- calcColors(data[[panel$response]], panel$levels)
	pch <- 21
	plot(mds[,1:2], type="p", xlab = xlab, ylab = ylab, col=col, pch=pch, main=main, ...)
	legend("bottomright",legend=panel$levels, fill=c("green", "red"), title="Response")
	
	for (i in 1:length(predictors)) {
		for (j in i+1:length(predictors)) {
			if (j>length(predictors)) {
				break;
			}
			plot(dat[[predictors[i]]],dat[[predictors[j]]],
				 col=col,pch=pch,
				 xlab=predictors[i], ylab=predictors[j], log="xy", main="Markers 2 by 2")
			abline(v=panel$thresholds[[i]])
			abline(h=panel$thresholds[[j]])
		}
	}
}


# Plot diagnosis on cross validation
plot.cv.diag <- function(panels.cv, data, training.panels) {
	
	par(mar=c(10, 4, 2, 0)+.1)
	
	#table.mol.stability(panels.cv)
	ts <- table.mol.stability(panels.cv)
	plot(ts)
	
	#table.nr.stability(panels.cv)
	plot(table.nr.stability(panels.cv))
	
	#table.min.nr.stability(panels.cv)
	plot(table.min.nr.stability(panels.cv))
	
	plot.thr.stability(panels.cv, data)
	
	# ROC
	suppressPackageStartupMessages(require(pROC, quietly=TRUE))
	
	training.pred <- predict(training.panels)
	response <- training.panels[[1]]$train.data[[training.panels[[1]]$response]]
	roc(response, training.pred, levels=training.panels[[1]]$levels, plot=TRUE, percent=TRUE)
	
	# CV ROC
	cv.pred <- predict(panels.cv)
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
