#' Performance on the training data
#' @description Calculates the fit on the training data. See \code{\link{test.perf}} to test the fit on a new data set
#' @param object the model
#' @param ... additional arguments to and from other methods
#' @export
train.perf <- function(...) {
	UseMethod("train.perf")
}

#' @rdname train.perf
#' @export
train.perf.exh <- function(object, ...) {
	newdata <- object$test.data
	p <- predict(object, newdata, center=F)
	roc <- roc(newdata[[object$response]], p, levels=object$levels, plot=F, ...)$AUC
	perf <- perf(p[newdata[[object$response]]==object$levels[1]], p[newdata[[object$response]]==object$levels[2]], object$min.nr, ">=")
	return(list("sensitivity"=perf$se,"specificity"=perf$sp, "auc"=roc))
}

#' @rdname train.perf
#' @export
train.perf.exhlist <- function(object, ...) {
	se <- sp <- auc <- c()
	if (is.null(names(object)))
		length <- length(object)
	else
		length <- length(object[names(object)==""])
	for (i in 1:length) {
		if (is.null(object$train.data)) {
			perf <- test.perf.exh(object[[i]], newdata=object[[1]]$train.data, ...)
			se <- c(se, perf$se)
			sp <- c(sp, perf$sp)
			auc <- c(auc, perf$auc)
		}
		else {
			perf <- test.perf.exh(object[[i]], newdata=object$train.data, ...)
			se <- c(se, perf$se)
			sp <- c(sp, perf$sp)
			auc <- c(auc, perf$auc)
		}
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

#' @rdname train.perf
#' @export
train.perf.exhcv <- function(object, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(object)) {
		perf <- train.perf(object[[i]], ...)
		se <- c(se, mean(perf$se))
		sp <- c(sp, mean(perf$sp))
		auc <- c(auc, mean(perf$auc))
		# roc <- c(roc, plot.roc()$AUC)
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

#' @rdname train.perf
#' @export
train.perf.exhcvlist <- function(object, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(object)) {
		mp <- train.perf(object[[i]])
		se <- c(se, mean(mp$se))
		sp <- c(sp, mean(mp$sp))
		auc <- c(auc, mean(mp$auc))
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

