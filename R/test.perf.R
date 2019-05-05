#' Performance on a test data set
#' @description Calculates the fit on a test data.
#' @param object the model
#' @param newdata a \code{\link{data.frame}} containing the test data
#' @param ... additional arguments to and from other methods
#' @export
test.perf <- function(object, ...) {
	UseMethod("test.perf")
}

#' @rdname test.perf
#' @export
test.perf.exh <- function(object, newdata=object$test.data, ...) {
	p <- predict(object, newdata, center=F)
	roc <- roc(newdata[[object$response]], p, levels=object$levels, plot=F, ...)$AUC
	perf <- perf(p[newdata[[object$response]]==object$levels[1]], p[newdata[[object$response]]==object$levels[2]], object$min.nr, ">=")
	return(list("sensitivity"=perf$se,"specificity"=perf$sp, "auc"=roc))
}

#' @rdname test.perf
#' @export
test.perf.exhlist <- function(object, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(object[names(object)==""])) {
		perf <- test.perf.exh(object[[i]], newdata=object$test.data, ...)
		se <- c(se, perf$se)
		sp <- c(sp, perf$sp)
		auc <- c(auc, perf$auc)
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

#' @rdname test.perf
#' @export
test.perf.exhcv <- function(object, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(object)) {
		perf <- test.perf(object[[i]], ...)
		se <- c(se, mean(perf$se))
		sp <- c(sp, mean(perf$sp))
		auc <- c(auc, mean(perf$auc))
		# roc <- c(roc, plot.roc()$AUC)
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

#' @rdname test.perf
#' @export
test.perf.exhcvlist <- function(object, ...) {
	se <- sp <- auc <- c()
	for (i in 1:length(object)) {
		mp <- test.perf(object[[i]])
		se <- c(se, mean(mp$se))
		sp <- c(sp, mean(mp$sp))
		auc <- c(auc, mean(mp$auc))
	}
	return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

perf <- function(controls, patients, threshold, direction='>') {
	# compute performances, returns as list
	if (direction == '>') {
		tp <- sum(as.numeric(patients>threshold))
		tn <- sum(as.numeric(controls<=threshold))
		fp <- sum(as.numeric(controls>threshold))
		fn <- sum(as.numeric(patients<=threshold))
	}
	else if (direction == '<') {
		tp <- sum(as.numeric(patients<threshold))
		tn <- sum(as.numeric(controls>=threshold))
		fp <- sum(as.numeric(controls<threshold))
		fn <- sum(as.numeric(patients>=threshold))
	}
	else if (direction == '>=') {
		tp <- sum(as.numeric(patients>=threshold))
		tn <- sum(as.numeric(controls<threshold))
		fp <- sum(as.numeric(controls>=threshold))
		fn <- sum(as.numeric(patients<threshold))
	}
	else if (direction == '<=') {
		tp <- sum(as.numeric(patients<=threshold))
		tn <- sum(as.numeric(controls>threshold))
		fp <- sum(as.numeric(controls<=threshold))
		fn <- sum(as.numeric(patients>threshold))
	}
	else {
		stop("argument 'direction' not implemented")
	}
	spec <- tn/length(controls)
	sens <- tp/length(patients)
	ppv <- tp/(tp+fp)
	npv <- tn/(tn+fn)
	return(list("sensitivity"=sens,"specificity"=spec, "ppv"=ppv, "npv"=npv))
}


