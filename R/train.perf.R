# Generic train.perf
train.perf <- function(...) {
	UseMethod("train.perf")
}

# function for a single exh - works for both train & test: pass as newdata.
train.perf.exh <- function(exh, ...) {
	newdata <- exh$test.data
	p <- predict(exh, newdata, center=F)
	roc <- roc(newdata[[exh$response]], p, levels=exh$levels, plot=F, ...)$AUC
	perf <- perf(p[newdata[[exh$response]]==exh$levels[1]], p[newdata[[exh$response]]==exh$levels[2]], exh$min.nr, ">=")
	return(list("sensitivity"=perf$se,"specificity"=perf$sp, "auc"=roc))
}

train.perf.exhlist <- function(exhlist, ...) {
	se <- sp <- auc <- c()
	if (is.null(names(exhlist)))
		length <- length(exhlist)
	else
		length <- length(exhlist[names(exhlist)==""])
	for (i in 1:length) {
		if (is.null(exhlist$train.data)) {
			perf <- test.perf.exh(exhlist[[i]], newdata=exhlist[[1]]$train.data, ...)
			se <- c(se, perf$se)
			sp <- c(sp, perf$sp)
			auc <- c(auc, perf$auc)
		}
		else {
			perf <- perf.exh(exhlist[[i]], newdata=exhlist$train.data, ...)
			se <- c(se, perf$se)
			sp <- c(sp, perf$sp)
			auc <- c(auc, perf$auc)
		}
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

train.perf.exhcv <- function(exhcv, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(exhcv)) {
		perf <- train.perf(exhcv[[i]], ...)
		se <- c(se, mean(perf$se))
		sp <- c(sp, mean(perf$sp))
		auc <- c(auc, mean(perf$auc))
		# roc <- c(roc, plot.roc()$AUC)
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

train.perf.exhcvlist <- function(exhcvlist, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(exhcvlist)) {
		mp <- train.perf(exhcvlist[[i]])
		se <- c(se, mean(mp$se))
		sp <- c(sp, mean(mp$sp))
		auc <- c(auc, mean(mp$auc))
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

