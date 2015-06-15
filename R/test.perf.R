# PERFORMANCE in SE and SP & ROC of the panels on the training and test sets

test.perf <- function(...) {
  UseMethod("test.perf")
}

# function for a single exh - works for both train & test: pass as newdata.
test.perf.exh <- function(exh, newdata=exh$test.data, ...) {
  require(pROC)
  p <- predict(exh, newdata, center=F)
  roc <- roc(newdata[[exh$response]], p, levels=exh$levels, plot=F, ...)$AUC
  perf <- perf(p[newdata[[exh$response]]==exh$levels[1]], p[newdata[[exh$response]]==exh$levels[2]], exh$min.nr, ">=")
  return(list("sensitivity"=perf$se,"specificity"=perf$sp, "auc"=roc))
}


test.perf.exhlist <- function(exhlist, ...) {
  se <- sp <- auc <- c()
  for (i in 1:length(exhlist[names(exhlist)==""])) {
    perf <- test.perf.exh(exhlist[[i]], newdata=exhlist$test.data, ...)
    se <- c(se, perf$se)
    sp <- c(sp, perf$sp)
    auc <- c(auc, perf$auc)
  }
  return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

test.perf.exhcv <- function(exhcv, ...) {
  se <- sp <- auc <- c()
  for (i in 1:length(exhcv)) {
      perf <- test.perf(exhcv[[i]], ...)
      se <- c(se, mean(perf$se))
      sp <- c(sp, mean(perf$sp))
      auc <- c(auc, mean(perf$auc))
   # roc <- c(roc, plot.roc()$AUC)
  }
  return(list("sensitivity"=se,"specificity"=sp, "auc"=auc))
}

test.perf.exhcvlist <- function(exhcvlist, ...) {
  se <- sp <- auc <- c()
  for (i in 1:length(exhcvlist)) {
    mp <- test.perf(exhcvlist[[i]])
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


