#' Model predictions
#' @rdname predict
#' @param object a model: \code{exh}, \code{exhlist}, \code{exhcv} or \code{exhcvlist}
#' @param newdata some data to make the predictions
#' @param center should the predicted values be centered around 0? If TRUE, returned values will be comprized between -1 and +1, with mean 0. Useful when multiple panels have a different \code{min.nr} and averaging them doesn't can't be done directly
#' @param ... passing arguments to and from other methods
#' @param statistics how to average
#' @importFrom stats predict
#' @examples
#' data(aSAH, package = "pROC")
#' panels <- exh.train(aSAH, c("age", "s100b", "ndka"), "outcome", progress=FALSE)
#' predict(panels, aSAH)
#' @export
predict.exh <- function(object, newdata=NULL, center=TRUE, ...) {
	if (is.null(newdata)) {
		p <- exh.score4(object$train.data[object$panel], object$thresholds, object$direction)
	}
	else {
		p <- exh.score4(newdata[object$panel], object$thresholds, object$direction)
	}
	if (center) {
		p <- p - (object$min.nr)
		p[p < 0] <- p[p < 0] / object$min.nr
		p[p > 0] <- p[p > 0] / (length(object$thresholds) - (object$min.nr))
	}
	#attr(p, "response") <- response
	return(p)
}

#' @rdname predict
#' @export
predict.exhlist <- function(object, newdata=NULL, statistics=mean, ...) {
	# filter exhlist. Store only exh in exhlist2. Sometimes there are named values that are not exh and that we don't want to analyze.
	if (! is.null(names(object)))
		exhlist2 <- object[names(object)==""] # keep only the entries without a name: that are the exh!
	else
		exhlist2 <- object
	if (is.null(newdata) || is.na(newdata)) {
		if (!is.null(object$test.data)) {
			# if there is a test.data, use this: can this code ever be reached? It seems more likely that there will always be a training data
			preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=object$test.data, ...)), 1, statistics)
			names(preds) <- rownames(object$test.data)
		}
		else if (!is.null(object[[1]]$test.data)) {
			# if there is a test.data in the first exh?
			preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=object[[1]]$test.data, ...)), 1, statistics)
			names(preds) <- rownames(object[[1]]$test.data)
		}
		else if (!is.null(object$train.data)) {
			#otherwise use the train data
			preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=object$train.data, ...)), 1, statistics)
			names(preds) <- rownames(object[[1]]$train.data)
		}
		else if (!is.null(object[[1]]$train.data)) { # try to find data in the first exh
			preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=object[[1]]$train.data, ...)), 1, statistics)
			names(preds) <- rownames(object[[1]]$train.data)
		}
		else {
			stop("No train.data, test.data or newdata. Unable to predict anything without data.")
		}
	}
	else {
		# newdata is provided:
		preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=newdata, ...)), 1, statistics)
		names(preds) <- rownames(newdata)
	}
	return(preds)
}

#' @rdname predict
#' @export
predict.exhcv <- function(object, ...) {
	k <- length(object)
	predictions <- ids <- c()
	# as the number of values returned by predict for each exhcv[[i]] can vary, it seems unsafe to use a as.data.frame(lapply(...))
	for (i in 1:k) {
		predictions <- c(predictions, predict(object[[i]], statistics=mean, ...))
	}
	return(predictions)
}


#' @rdname predict
#' @export
predict.exhcvlist <- function(object, statistics=mean, ...) {
	nreps <- length(object)
	predictions <- list()
	for (reps in 1:nreps) {
		p <- predict(object[[reps]], ...)
		predictions[[reps]] <- p[order(names(p))]
	}
	predictions <- apply(as.data.frame(predictions), 1, statistics)
	
	# attach response as attribute
	if (!is.null(attr(object, "train.data"))) # sort predictions and attach response as attribute if possible
		attr(predictions, "response") <- attr(object, "train.data")[[object[[1]][[1]][[1]]$response]][order(rownames(attr(object, "train.data")))]
	return(predictions)
}
