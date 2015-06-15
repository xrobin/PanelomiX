# Calls exh.train in nreps k-fold CV round to evaluate performance
exh.train.cv <- function(data, predictors, response, fixed.predictors=NULL, id=NULL, levels=base::levels(as.factor(data[[response]])), nreps=10, k=10, stratified=TRUE, na.rm=c("all", "local"), rand.response=FALSE, progress="progress_cv", ...) {
  na.rm <- match.arg(na.rm)
  all.predictors <- c(predictors, fixed.predictors)
  # Remove nas
  if (na.rm == "all")
    data <- data[!apply(as.data.frame(data[,all.predictors]), 1, function(x) {any(is.na(x))}),]

  if (is.null(id)) {
    random.string <- ""
    while (nchar(random.string) != 10 || random.string %in% names(data))
      random.string <- paste("id.", paste(c(letters, LETTERS, 0:9)[ceiling(runif(7, 0, 62))], collapse=""), sep="")
    id <- random.string
    data[[id]] <- rownames(data)
  }
  rownames(data) <- data[[id]] # make sure we have ID as row names

  data <- data[,c(id, all.predictors, response)]
  data <- subset(data, data[[response]]==levels[1] | data[[response]]==levels[2])

  # 10-fold CV
  preds <- numeric()
  outcomes <- data[[response]][0]

  exhcv <- list()
  class(exhcv) <- "exhcvlist"
  attr(exhcv, "time.start") <- Sys.time()
  for (reps in 1:nreps) {
    exhcv[[reps]] <- list()
    class(exhcv[[reps]]) <- "exhcv"
    attr(exhcv[[reps]], "time.start") <- Sys.time()
    if (rand.response) {
      data[[response]] <- sample(data[[response]])
    }
    s <- cv.indices(n=length(data[[response]]), k=k, stratified=stratified, response=data[[response]], levels=levels)
#    s = sample(rep(1:k, ceiling(npat/k))[1:npat])
    for (i in 1:k){ 
      cat(reps, i, "\n", file=progress, append=TRUE)
      test = data[!is.na(s) & s==i,] # remove the NAs that are generated if response contains more than the two levels
      learn = data[!is.na(s) & s!=i,]
      exhcv[[reps]][[i]] <- exh.train(learn, predictors, response, fixed.predictors, levels=levels, verbose=F, progress=FALSE, ...)
      exhcv[[reps]][[i]]$test.data <- test
      #exhcv[[reps]][[i]] <- try(exh.train(learn, predictors, response, fixed.predictors, levels=levels, verbose=F, progress=FALSE, ...))
      #  if (class(exhcv[[reps]][[i]])=="try-error")
      #    next()
#      if (!send.to.java || !run.java.later) {
#        preds <- c(preds, predict(model, newdata=test))
#        outcomes <- c(outcomes, test[[response]])
#      }
    }
    attr(exhcv[[reps]], "time.end") <- Sys.time()
  }

  # add whole data
  attr(exhcv, "train.data") <- data
  attr(exhcv, "time.end") <- Sys.time()
  return(exhcv)
  
  # restore factor if needed
  if (class(data[[response]])=="factor") {
    outcomes <- as.factor(outcomes)
    attributes(outcomes) <- attributes(data[[response]][0])
  }
  # Test on whole training set
#  final.model <- exh.train(data, predictors, response, levels=levels, ...)
  # Add CV results to model
#  final.model$CV <- list()
#  final.model$CV$outcomes <- outcomes
#  final.model$CV$preds <- preds
  # Print ROC with training + CV
#  plot.roc(data[[response]], predict(final.model), levels=levels, area.display=T, partial.area=c(100, 90), show.area=T, percent=T, show.thres=T)
#  plot.roc(outcomes, preds, add=T, levels=levels, col=options()$colors$sapphire, partial.area=c(100, 90), show.area=T, show.area.y=45, percent=T, show.thres=T, area.ci=T)
#  plot.roc(data[[response]], predict(final.model), levels=levels, percent=T, show.thres=T)
#  plot.roc(outcomes, preds, add=T, levels=levels, col=options()$colors$sapphire, percent=T, show.thres=T)
#  legend("bottomright", lwd=2, col=c("black", options()$colors$sapphire), legend=c("Training set", "Cross-Validation"))

#  return(final.model)
}



cv.indices <- function(n, k, stratified=TRUE, response=NA, levels=base::levels(as.factor(response))) {
  # returns the indices for cross-validation
  # n: number of patients
  # k: fols of CV
  # stratified: if CV must be stratified (similar number of patients of each group in each fold) or not. Defaults to true
  # response, levels: only required if stratified=TRUE (default)
  if (stratified) {
    # make sure the responses are given
    if (any(is.na(response)))
      stop("Unable to compute stratified CV folds if response is NA")
    if (length(response) != n)
      stop("n differs from the size of the response. Please check for consistency")
    #patients in group 1
    group.1 <- response==levels[1]
    group.2 <- response==levels[2]
    # attribute each patient to a test fold. Start by both "border": fold 1 (for level 1) and k (for level 2), and let the values fill between
    s1 <- sample(rep(1:k, ceiling(sum(group.1)/k))[1:sum(group.1)]) # 1:k to have more in fold 1
    s2 <- sample(rep(k:1, ceiling(sum(group.2)/k))[1:sum(group.2)]) # k:1 to have more in fold 10
    # prepare the index to return
    s <- rep(NA, n)
    s[group.1] <- s1
    s[group.2] <- s2
    return(s)
  }
  else { # non stratified
    return(sample(rep(1:k, ceiling(n/k))[1:n]))
  }
}


