# exh: the panel object
# newdata: the data on which to test the panel
# center: should the predicted values be centered around 0? If TRUE, returned values will be comprized between -1 and +1, with 0 as
predict.exh <- function(exh, newdata=NULL, center=TRUE) {
  if (is.null(newdata)) {
    p <- exh.score4(exh$train.data[exh$panel], exh$thresholds, exh$direction)
    #response <- exh$train.data[exh$response]
  }
  else {
    p <- exh.score4(newdata[exh$panel], exh$thresholds, exh$direction)
    #response <- exh$train.data[exh$response]
  }
  if (center) {
    p <- p - (exh$min.nr)
    p[p < 0] <- p[p < 0] / exh$min.nr
    p[p > 0] <- p[p > 0] / (length(exh$thresholds) - (exh$min.nr))
  }
  #attr(p, "response") <- response
  return(p)
}

predict.exhlist <- function(exhlist, newdata=NULL, statistics=mean, ...) {
  # filter exhlist. Store only exh in exhlist2. Sometimes there are named values that are not exh and that we don't want to analyze.
  if (! is.null(names(exhlist)))
    exhlist2 <- exhlist[names(exhlist)==""] # keep only the entries without a name: that are the exh!
  else
    exhlist2 <- exhlist
  if (is.null(newdata) || is.na(newdata)) {
    if (!is.null(exhlist$test.data)) {
      # if there is a test.data, use this: can this code ever be reached? It seems more likely that there will always be a training data
      preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=exhlist$test.data, ...)), 1, statistics)
      names(preds) <- rownames(exhlist$test.data)
    }
    else if (!is.null(exhlist[[1]]$test.data)) {
      # if there is a test.data in the first exh?
      preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=exhlist[[1]]$test.data, ...)), 1, statistics)
      names(preds) <- rownames(exhlist[[1]]$test.data)
    }
    else if (!is.null(exhlist$train.data)) {
      #otherwise use the train data
      preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=exhlist$train.data, ...)), 1, statistics)
      names(preds) <- rownames(exhlist[[1]]$train.data)
    }
    else if (!is.null(exhlist[[1]]$train.data)) { # try to find data in the first exh
      preds <- apply(as.data.frame(lapply(exhlist2, predict, newdata=exhlist[[1]]$train.data, ...)), 1, statistics)
      names(preds) <- rownames(exhlist[[1]]$train.data)
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

predict.exhcv <- function(exhcv, ...) {
  k <- length(exhcv)
  predictions <- ids <- c()
  # as the number of values returned by predict for each exhcv[[i]] can vary, it seems unsafe to use a as.data.frame(lapply(...))
  for (i in 1:k) {
    predictions <- c(predictions, predict(exhcv[[i]], statistics=mean, ...))
    #ids <- c(ids, as.character(exhcv[[i]]$test.data[,1]))
  }
  #names(predictions) <- ids
  return(predictions)
}

# Predict
predict.exhcvlist <- function(exhcvlist, statistics=mean, ...) {
  nreps <- length(exhcvlist)
  predictions <- list()
  for (reps in 1:nreps) {
    p <- predict(exhcvlist[[reps]], ...)
    predictions[[reps]] <- p[order(names(p))]
  }
  predictions <- apply(as.data.frame(predictions), 1, statistics)

  # attach response as attribute
  if (!is.null(attr(exhcvlist, "train.data"))) # sort predictions and attach response as attribute if possible
    attr(predictions, "response") <- attr(exhcvlist, "train.data")[[exhcvlist[[1]][[1]][[1]]$response]][order(rownames(attr(exhcvlist, "train.data")))]
  return(predictions)
}
