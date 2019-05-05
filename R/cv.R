#' Cross-validation of a panel
#' @description
#' Calls exh.train in nreps k-fold CV round to evaluate performance
#' @param data the dataset
#' @param predictors the name of the columns to be tested as predictors in the panel. May or may not be used.
#' @param response the binary response column name
#' @param id the name of a column storing unique IDs for the observations
#' @param fixed.predictors predictors to force into the panel
#' @param levels the values of \code{response} to use as negative and positive data
#' @param na.rm whether to omit rows with \code{NA} observations
#' @param nreps number of cross-validation repeats
#' @param k number of folds of the cross-validation (use \code{k = nrow(data)}) for leave-one-out CV
#' @param stratified whether to keep the balance of positive/negative samples in all the CV folds
#' @param verbose enables additional output for debugging
#' @param working.dir,java.keep.files same as for \code{\link{exh.train}}
#' @param ... further arguments passed to \code{\link{exh.train}}. \code{constrain.on}, \code{min.constr}, \code{panels.of.num} or \code{limit.java.threads} are probably the most useful ones.
#' @export
#' @importFrom stats na.omit runif
#' @examples 
#' data(aSAH, package="pROC")
#' exh.train.cv(aSAH, c("age", "s100b", "ndka"), "outcome")
exh.train.cv <- function(data, predictors, response, 
						 fixed.predictors=NULL, 
						 id=NULL, 
						 levels=base::levels(as.factor(data[[response]])), 
						 na.rm=FALSE,
						 nreps=10, k=10, stratified=TRUE, 
						 verbose=FALSE,
						 working.dir=NULL,
						 java.keep.files=TRUE,
						 ...) {
	all.predictors <- c(predictors, fixed.predictors)
	
	if (missing(id) || is.null(id)) {
		random.string <- ""
		while (nchar(random.string) != 10 || random.string %in% names(data))
			random.string <- paste("id.", paste(c(letters, LETTERS, 0:9)[ceiling(runif(7, 0, 62))], collapse=""), sep="")
		id <- random.string
		data[[id]] <- rownames(data)
	}
	rownames(data) <- data[[id]] # make sure we have ID as row names
	
	# Remove nas
	if (na.rm)
		data <- na.omit(data[, c(id, all.predictors, response)])
	# Remove patients with unneeded levels
	data <- data[data[[response]] %in% levels,]
	# And remove unused levels
	data[[response]] <- factor(data[[response]], levels=levels)
	# Now if we still have any na, return NA
	if (any(is.na(data))) {
		return(NA)
	}
	
	# 10-fold CV
	preds <- numeric()
	outcomes <- data[[response]][0]
	
	if (missing(working.dir) || is.null(working.dir) || is.na(working.dir) || working.dir == "") {
		working.dir <- tempfile("PanelomiX_")
		if (verbose) {
			message(paste0("Creating temporary file in ", working.dir, "."))
		}
		if (missing(java.keep.files) || is.null(java.keep.files)) {
			java.keep.files <- FALSE
		}
	}
	else if (missing(java.keep.files) || is.null(java.keep.files)) {
		java.keep.files <- TRUE
	}
	dir.create(working.dir, showWarnings = FALSE)
	progress.file <- file.path(working.dir, "progress")
	
	exhcv <- list()
	class(exhcv) <- "exhcvlist"
	attr(exhcv, "time.start") <- Sys.time()
	for (reps in 1:nreps) {
		exhcv[[reps]] <- list()
		class(exhcv[[reps]]) <- "exhcv"
		attr(exhcv[[reps]], "time.start") <- Sys.time()
		s <- cv.indices(n=length(data[[response]]), k=k, stratified=stratified, response=data[[response]], levels=levels)
		for (i in 1:k){ 
			cat(reps, i, "\n", file=progress.file, append=TRUE)
			test = data[!is.na(s) & s==i,] # remove the NAs that are generated if response contains more than the two levels
			learn = data[!is.na(s) & s!=i,]
			fold.working.dir <- file.path(working.dir, paste(reps, i, sep="_"))
			exhcv[[reps]][[i]] <- exh.train(learn, predictors, response, fixed.predictors, 
											levels=levels, verbose=FALSE,
											working.dir = fold.working.dir, java.keep.files = java.keep.files,
											...)
			exhcv[[reps]][[i]]$test.data <- test
		}
		attr(exhcv[[reps]], "time.end") <- Sys.time()
	}
	
	# add whole data
	attr(exhcv, "train.data") <- data
	attr(exhcv, "time.end") <- Sys.time()
	if (! java.keep.files) {
		rm <- unlink(paste(working.dir, sep=""), recursive=TRUE)
		if (rm == 1)
			warning(paste0("Directory ", working.dir, " could not be removed. Please delete it manually."))
	}
	return(exhcv)
}


#' @name cv.indices
#' @rdname Internals
#' @param n number of observations
#' @param k number of folds
#' @param stratified if CV must be stratified (similar number of observations of each group in each fold) or not. Defaults to \code{TRUE}.
#' @param response,levels as for \code{\link{exh.train.cv}}
cv.indices <- function(n, k, stratified=TRUE, response, levels) {
	# returns the indices for cross-validation
	# n: number of patients
	# k: fols of CV
	# stratified: if CV must be stratified (similar number of patients of each group in each fold) or not. Defaults to true
	# response, levels: only required if stratified=TRUE (default)
	if (stratified) {
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


