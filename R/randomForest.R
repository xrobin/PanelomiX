
## RandomForest filtering of markers only ##
filter.markers.randomForest <- function(rfmodel,
                               response,
                               predictors=rownames(rfmodel$importance), # this is all predictors, including fixed.predictors
                               fixed.predictors=NULL,
                               data,
                               levels=base::levels(as.factor(data[[response]])),
                               remove.n.mols = 1 # Not counting fixed.markers
                               ) {
  molecules <- rownames(rfmodel$importance)
  split.vars <- factor(rfmodel$forest$bestvar[rfmodel$forest$nodestatus == 1])
  levels(split.vars) <- molecules
  #split.points <- rfmodel$forest$xbestsplit[rfmodel$forest$nodestatus == 1]
  #threshs <- split(split.points, split.vars)
#  names(threshs) <- molecules[as.numeric(names(threshs))]
  unfixed.molecules <- molecules[!molecules%in%fixed.predictors]
  t <- sort(table(split.vars)[unfixed.molecules], decreasing=TRUE)
  keep.molecules <- names(t)[1:(length(t)-remove.n.mols)]
  return(keep.molecules)
}
                                           
## RandomForest thresholds filtering ##
filter.thresholds.randomForest <- function(rfmodel,
                               response,
                               predictors=rownames(rfmodel$importance), # this is all predictors, including fixed.predictors
                               fixed.predictors=NULL,
                               data,
                               levels=base::levels(as.factor(data[[response]])),
                               #molecules=if (is.character(predictors)) predictors else rownames(predictors),
                               decreasing=TRUE,
                               min=NULL,
                               num=NULL,
                               limit.mols=NULL # Not counting fixed.markers 
                               ) {
  molecules <- rownames(rfmodel$importance)
  split.vars <- factor(rfmodel$forest$bestvar[rfmodel$forest$nodestatus == 1])
  levels(split.vars) <- molecules
  split.points <- rfmodel$forest$xbestsplit[rfmodel$forest$nodestatus == 1]
  threshs <- split(split.points, split.vars)
#  names(threshs) <- molecules[as.numeric(names(threshs))]

  if (!is.null(limit.mols)) {
    unfixed.molecules <- molecules[!molecules%in%fixed.predictors]
    t <- sort(table(split.vars)[unfixed.molecules], decreasing=decreasing)
    molecules <- names(t)[1:min(limit.mols, length(unfixed.molecules))]
    molecules <- unique(c(molecules, fixed.predictors))
  }

  # This function computes euclidian distances
  match.thresh.euclidian <- function(i, obs.thresholds, target.thresholds) {
    distance <- sqrt((obs.thresholds[2,i] - target.thresholds[2,])^2 + (obs.thresholds[3,i] - target.thresholds[3,])^2)
    which.min(distance)
  }

  ret <- list()
  for (mol in molecules) {
    # Get target and observed thresholds
    target.thresholds <- pROC::coords(pROC::roc(data[[response]], data[[mol]], levels=levels), "l", drop=FALSE)
    target.thresholds <- target.thresholds[,is.finite(target.thresholds[1,]), drop=FALSE] # remove Inf and -Inf
    # Make sure the is.finite() test didn't remove everything.
    # If it is the case, die with a clear message.
    if (dim(target.thresholds)[2] < 1) {
      stop(paste("No finite threshold found for molecule ", mol, ". Please check your data.", sep=""))
    }
    obs.levels <- unique(threshs[[mol]])
    # This is the slow step – coords with 1000 levels takes about 1s
    obs.thresholds <- pROC::coords.roc(pROC::roc(data[[response]], data[[mol]], levels=levels), obs.levels, drop=FALSE)

    # Match each observed with a target using euclidian distance
    matches <- target.thresholds[1,sapply(1:ncol(obs.thresholds), match.thresh.euclidian, obs.thresholds=obs.thresholds, target.thresholds=target.thresholds)]

    # Retrieve the matched thresholds from the matches!
    matched.thresholds <- matches[match(threshs[[mol]], obs.thresholds[1,])]
    # Get the sorted threshold frequencies
    matched.thresholds <- sort(table(matched.thresholds), decreasing=decreasing)
    if (!is.null(min)) {
      matched.thresholds <- matched.thresholds[matched.thresholds>=min]
    }
    if (!is.null(num)) {
      matched.thresholds <- matched.thresholds[1:min(num, length(matched.thresholds))]
    }
    ret[[mol]] <- as.numeric(names(matched.thresholds))
  }
  return(ret)
}
