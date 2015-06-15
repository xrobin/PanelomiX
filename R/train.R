#' Train a panel
#' @param data the dataset
#' @param predictors the name of the columns to be tested as predictors in the panel. May or may not be used.
#' @param response the binary response column name
#' @param fixed.predictors predictors to force into the panel
#' @param id a column with sample ids
#' @param constrain.on,min.constr objective constrains. Consider only panel with \code{min.nr} \dQuote{specificity} or \dQuote{sensitivity}, or maximize global \dQuote{accuracy}
#' @param levels the values of \code{response} to use as negative and positive data
#' @param panels.of.num a vector of integer, defining the acceptable number of markers included in the panel
#' @param test.thresholds the thresholds to test. Ignored if filter.randomForest is a list
#' @param directions a named list (after the \code{predictors} and \code{fixed.predictors}) of directions as described in \code{\link{pROC::roc}}
#' @param filter.number.thresholds if \code{TRUE}, filter the \code{test.thresholds} to speed up the search. Ignored if filter.randomForest is a list. Can be the character \dQuote{2x2} or a numeric (length 1)
#' @param filter.randomForest a \code{\link{list}} with 2 elements: \code{molecules} (number of markers to keep) \code{thresholds} (number of thresholds to keep per marker)
#' @param verbose enables additional output for debugging
#' @param multiple.panels one of \dQuote{all}, \dQuote{random} or \dQuote{first} when multiple panels are equivalent, which one(s) to report
#' @param na.rm one of \dQuote{all} or \dQuote{local}
#' @param working.dir a (preferably empty) directory where the files required by java will be placed. If \code{\link{missing}}, \code{NULL} \code{NA} or an empty string, a random name will be attributed in /tmp, otherwise the given string will be used.
#' @param java.keep.files if \code{TRUE}, the java files are kept, if \code{FALSE} they are deleted. If \code{NULL} or \code{\link{missing}}, the files will be kept unless a temporary file was created because \code{working.dir} was missing \code{NULL}, \code{NA} or an empty string. Note that even with \code{java.keep.files = FALSE}, files main remain if the panel doesn't complete.
#' @param limit.java.threads how many threads to use. if \code{NA} (default), java will define itself how many threads to use. If an integer is given, will force to this number
#' @examples 
#' data(aSAH, package="pROC")
#' exh.train(aSAH, c("age", "s100b", "ndka"), "outcome")
#' @export
exh.train <- function(data, predictors, response,
					  fixed.predictors=NULL,
					  id=NULL,
					  constrain.on=c("specificity", "sensitivity", "accuracy"),
					  min.constr=0.95,
					  levels=base::levels(as.factor(data[[response]])),
					  panels.of.num=1:length(predictors),
					  test.thresholds=NA, # ignored if filter.randomForest is a list
					  directions=NULL,
					  filter.number.thresholds=NA, # ignored if filter.randomForest is a list
					  filter.randomForest=FALSE,
					  verbose=FALSE,
					  multiple.panels=c("all", "random", "first"),
					  na.rm=FALSE,
					  working.dir=NULL,
					  java.keep.files=TRUE,
					  limit.java.threads=NA) {
	
	# Match arguments
	constrain.on <- match.arg(constrain.on)
	multiple.panels <- match.arg(multiple.panels)
	all.predictors <- c(predictors, fixed.predictors)
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
	#  require(gtools) # function combinations - not used anymore
	exh <- list()
	class(exh) <- "exh"
	exh$time.start <- Sys.time()
	exh$train.data <- data
	#  exh$min.nr <- min.nr # positive molecules to have a positive test
	exh$predictors <- predictors
	exh$fixed.predictors <- fixed.predictors
	exh$all.predictors <- all.predictors
	exh$response <- response
	exh$levels <- levels
	exh$id <- id
	exh$nr.patients <- length(data[[response]])
	exh$nr.required.mol <- 0
	exh$panels.of.num <- panels.of.num
	splitted <- split(as.data.frame(data[,all.predictors]), data[[response]])
	train.data.good <- splitted[[as.character(levels[1])]]
	names(train.data.good) <- all.predictors
	exh$nr.class[[as.character(levels[1])]] <- dim(train.data.good)[1]
	train.data.bad <- splitted[[as.character(levels[2])]]
	names(train.data.bad) <- all.predictors
	exh$nr.class[[as.character(levels[2])]] <- dim(train.data.bad)[1]
	exh$train.data.good <- train.data.good
	exh$train.data.bad <- train.data.bad
	possible.thresholds <- list()
	# Check constraints
	exh$constrain.on <- constrain.on # se or sp?
	exh$optimize <- ifelse(constrain.on=="specificity", "sensitivity", "specificity") # sp or se?
	exh$min.constr <- min.constr
	exh$max.perf <- -1 # stores the maximum sp or se achieved
	exh$nr.iter <- 0 # count iterations (informative)
	exh$best.panels <- list() # placeholder to stores the best panel thresholds
	#exh$best.panels
	
	# Directions
	if (is.null(directions)) {
		directions <- list()
	}
	if (any(sapply(directions[all.predictors], is.null))) {
		if (!interactive())
			suppressPackageStartupMessages(library(pROC, quietly = TRUE))
		else 
			require(pROC, quietly = TRUE)
		for (predictor in all.predictors) {
			if (!is.numeric(data[[predictor]])) {
				stop("All predictor columns must be numeric")
			}
			if (is.null(directions[[predictor]])) {
				curve <- pROC::roc(data[[response]], data[[predictor]], levels=levels)
				directions[[predictor]] <- curve$direction
			}
		}
	}
	exh$directions <- directions
	
	### Now determine the thresholds ###
	# Filter with randomForest
	if (is.list(filter.randomForest)) {
		if (!interactive())
			suppressPackageStartupMessages(library(randomForest))
		else 
			require(randomForest, quietly = TRUE)
		
		# Filter by random forest
		# Create the class weights for RF
		classwt <- if (constrain.on == "specificity") {c(min.constr, 1 - min.constr)} else if (constrain.on == "sensitivity") {c(1 - min.constr, min.constr)}
		if (constrain.on != "accuracy" && any(classwt == 0)) {
			classwt <- NULL
			warning("Cannot define classwt with 100% sensitivity or specificity")
		}
		# More intellingent filtering of markers 1 by 1:
		predictors.filtered <- predictors
		all.predictors.filtered <- all.predictors
		while (length(all.predictors.filtered) > filter.randomForest$molecules) {
			formula <- as.formula(paste("as.factor(", response, ") ~", paste(all.predictors.filtered, collapse="+")))
			rfModel <- randomForest(formula, data = data, classwt=classwt)
			predictors.filtered <- filter.markers.randomForest(rfmodel=rfModel,
															   response=response, predictors=predictors.filtered,
															   fixed.predictors=fixed.predictors, data=data,
															   levels=levels, remove.n.mols=1)
			all.predictors.filtered <- c(predictors.filtered, fixed.predictors)
		}
		# Now we've filtered predictors out, it's our new predictors!
		# We can now determine the thresholds
		formula <- as.formula(paste("as.factor(", response, ") ~", paste(all.predictors.filtered, collapse="+")))
		
		rfModel <- randomForest(formula, data = data, classwt=classwt)
		possible.thresholds <- filter.thresholds.randomForest(rfmodel=rfModel,
															  response=response, predictors=all.predictors.filtered,
															  fixed.predictors=fixed.predictors, data=data,
															  levels=levels, num=filter.randomForest$thresholds,
															  limit.mols=filter.randomForest$molecules)
		
		# as we possibly changed the number and values of predictors (with limit.mols), reset it:
		exh$all.predictors <- all.predictors <- names(possible.thresholds) 
		exh$predictors <- predictors <- all.predictors[!all.predictors%in%fixed.predictors]
	}
	else { # or use pROC to determine the possible thresholds if missing from test.thresholds
		if (any(is.na(test.thresholds))) {
			for (pred in all.predictors) {
				predictor.roc <- pROC::roc(response=data[[response]], predictor=data[[pred]], levels=levels, direction=directions[[predictor]])
				coords.vector <- as.vector(pROC::coords(predictor.roc, "l", ret="t"))
				coords.vector <- coords.vector[is.finite(coords.vector)] # remove infinite values generated by pROC
				possible.thresholds[[pred]] <- coords.vector
			}
		}
		else {
			possible.thresholds <- test.thresholds
		}
	}
	
	if (!is.na(filter.number.thresholds)) {
		if (is.character(filter.number.thresholds) & length(filter.number.thresholds)==1 & filter.number.thresholds=="2x2") {
			print("Filtering thresholds by 2x2 combinations")
			# Test all the 2x2 combinations for the thresholds
			combinations <- combn(predictors, 2)
			# initialize the list
			thresholds.2x2 <- list()
			for (i in 1:dim(combinations)[2]) {
				cur_combination = c(combinations[,i], fixed.predictors)
				print(cur_combination)
				e <- exh.train(data, cur_combination, response, constrain.on=constrain.on, min.constr=min.constr, levels=levels, panels.of.num=2, test.thresholds=test.thresholds, verbose=F, multiple.panels="all")
				# take the thresholds and merge to the list
				for (panel in e) {
					for (mol in names(panel$threshold)) {
						thresholds.2x2[[mol]] <- c(thresholds.2x2[[mol]], panel$threshold[[mol]])
					}
				}
			}
			# simplify the list
			for (pred in all.predictors)
				thresholds.2x2[[pred]] <- unique(thresholds.2x2[[pred]])
			# from now on use this list of thresholds
			possible.thresholds <- thresholds.2x2
			print("New 2x2 thresholds")
			print(thresholds.2x2)
		}
		if (is.numeric(filter.number.thresholds) & length(filter.number.thresholds)==1) {
			for (pred in all.predictors) {
				num.thresholds.for.this.pred <- length(possible.thresholds[[pred]])
				if (filter.number.thresholds < num.thresholds.for.this.pred) {
					# determine n intervals and find the best threshold inside each interval
					require(pROC)
					r <- roc(data[[response]], data[[pred]], levels=levels, plot=FALSE)
					stop("Sorry the ROC part seems to be buggy and needs to be rewritten")
					sq <- sort(rep(c(1:filter.number.thresholds), length.out=length(r$SES))) # determine the regions
					sums <- r$SES + r$SPS # we'll look for the best sums SE + SP in each region
					new.thresholds <- c()
					for(region in 1:filter.number.thresholds) {
						best.idx.region <- order(sums[sq==region], decreasing=TRUE)[1] # find the index of the best threshold in the region
						new.thresholds <- c(new.thresholds, r$thresholds[sq==region][best.idx.region]) # get the corresponding threshold
					}
					possible.thresholds[[pred]] <- new.thresholds
				}
			}
		}
	}
	exh$possible.thresholds <- possible.thresholds
	
	# PUT OPTIONS TO JSON FOR JAVA
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
	conf.filename <- file.path(working.dir, "conf")
	dir.create(working.dir, showWarnings = FALSE)
	file.create(conf.filename)
	
	# Prepare the data
	data.to.write <- exh$train.data[c(exh$id, exh$all.predictors, exh$response)];
	data.to.write <- subset(data.to.write, exh$train.data[[exh$response]]==exh$levels[1] | exh$train.data[[exh$response]]==exh$levels[2])
	write.csv(data.to.write, row.names=FALSE, file=file.path(working.dir, "data.csv"))
	# Prepare the conf file
	cat("response:", paste('"', exh$response, '"', sep=""), "\n", sep="", file=conf.filename)
	cat("levels:", paste('"', paste(exh$levels, collapse='","'), '"', sep=""), "\n", sep="", file=conf.filename, append=TRUE)
	if (length(exh$predictors) > 0)
		cat("predictors:", paste('"', paste(exh$predictors, collapse='","'), '"', sep=""), "\n", sep="", file=conf.filename, append=TRUE)
	if (length(exh$fixed.predictors) > 0)
		cat("fixed.predictors:", paste('"', paste(exh$fixed.predictors, collapse='","'), '"', sep=""), "\n", sep="", file=conf.filename, append=TRUE)
	cat("datafile:", '"data.csv"', "\n", sep="", file=conf.filename, append=TRUE)
	cat("outputfile:\"result\"\n", file=conf.filename, append=TRUE)
	cat("progressfile:\"progress\"\n", file=conf.filename, append=TRUE)
	cat("thresholds:\n", file=conf.filename, append=TRUE)
	for(mol in all.predictors)
		#if (length(exh$possible.thresholds[[mol]]) > 0)
		cat('-"', mol, '":', paste(exh$possible.thresholds[[mol]], collapse=","), "\n", sep="", file=conf.filename, append=TRUE)
	#    cat("maxsize:", max(exh$panels.of.num), sep="", file=conf.filename, append=TRUE)
	cat("directions:\n", file=conf.filename, append=TRUE)
	for(mol in all.predictors)
		#if (length(exh$possible.thresholds[[mol]]) > 0)
		cat('-"', mol, '":', directions[[mol]], "\n", sep="", file=conf.filename, append=TRUE)
	
	# Define the panel sizes for java:
	panels.of.num <- panels.of.num - length(fixed.predictors)
	panels.of.num <- panels.of.num[panels.of.num >= 0]
	cat("panels.of.num:", paste(panels.of.num, collapse=','), "\n", sep="", file=conf.filename, append=TRUE)
	if (!is.null(id))
		cat("id:", id, "\n", sep="", file=conf.filename, append=TRUE)
	cat("constrain.on:", exh$constrain.on, "\n", sep="", file=conf.filename, append=TRUE)
	# print a min.constr only if constrain.on != "accuracy")
	if (exh$constrain.on != "accuracy")
		cat("min.constr:", exh$min.constr, "\n", sep="", file=conf.filename, append=TRUE)
	if (is.numeric(limit.java.threads)) {
		cat("num.proc:", limit.java.threads, "\n", sep="", file=conf.filename, append=TRUE)
	}
	cat("#time.start:", as.character(exh$time.start), "\n", sep="", file=conf.filename, append=TRUE)
	
	args <- rJava::.jarray(c("-m=0", "-c", conf.filename), "java/lang/String")
	panelomix <- rJava::.jcall("ch/unige/bprg/panelomix/PanelomiX", "V", "main", args)
	rJava::.jcheck()
	# Also suggested: J("ch.unige.bprg.panelomix.PanelomiX", "main")
	# system(paste('java ch.unige.bprg.panelomix.PanelomiX -m=0 -c "', getwd(), '/', conf.filename, '"', sep=""))
	# get the data back
	panels <- read.java.output(conf.filename)
	if (! java.keep.files) {
		rm <- unlink(paste(working.dir, sep=""), recursive=TRUE)
		if (rm == 1)
			warning(paste0("Directory ", working.dir, " could not be removed. Please delete it manually."))
	}
	return(panels)
}

#' Reads the output of the java PanelomiX processing. If you plan to re-read a panel later, make sure you run \code{\link{exh.train}} with \code{java.keep.files=TRUE}. 
#' @param conf.file the name of the configuration file to read data from
#' @param result.file can be used to restrict which files to read exactly
#' @importFrom stringr str_match_all
#' @export
read.java.output <- function(conf.file="conf", result.file=NA) {
	# first recover the configuration options
	default.panel <- list()
	class(default.panel) <- "exh"
	#  for (conf.line in readLines(conf.file, warn=FALSE)) {
	lines <- readLines(conf.file, warn=FALSE)
	for (i in 1:length(lines)) {
		conf.line <- lines[[i]]
		if (length(grep("^response:", conf.line))>0) {
			default.panel$response <- sub("^response:\"(.+)\"$", "\\1", conf.line, perl=TRUE)
		}
		else if (length(grep("^predictors:", conf.line))>0) {
			default.panel$predictors <- sub("^predictors:\"(.+)\"$", "\\1", conf.line, perl=TRUE)
			default.panel$predictors <- strsplit(default.panel$predictors, "\",\"")[[1]]
		}
		else if (length(grep("^fixed.predictors:", conf.line))>0) {
			default.panel$fixed.predictors <- sub("^fixed.predictors:\"(.+)\"$", "\\1", conf.line, perl=TRUE)
			default.panel$fixed.predictors <- strsplit(default.panel$fixed.predictors, "\",\"")[[1]]
		}
		else if (length(grep("^id:", conf.line))>0) {
			default.panel$id <- sub("^id:(.+)$", "\\1", conf.line, perl=TRUE)
		}
		else if (length(grep("^thresholds:", conf.line))>0) {
			i <- i+1
			conf.line <- lines[[i]]
			default.panel$possible.thresholds <- list()
			while(length(grep("^-", conf.line))>0) {
				mol <- sub("^-\"(.+)\":.+$", "\\1", conf.line, perl=TRUE)
				thr <- as.numeric(strsplit(sub("^-\".+\":(.*)$", "\\1", conf.line, perl=TRUE), ",")[[1]])
				default.panel$possible.thresholds[[mol]] <- thr
				i <- i+1
				conf.line <- lines[[i]]
			}
			i <- i-1
		}
		else if (length(grep("^datafile:", conf.line))>0) {
			datafile <- sub("^datafile:\"(.+)\"$", "\\1", conf.line, perl=TRUE)
			# is this file absolute?
			if (length(grep(":", datafile))==0) { # if not, is it relative?
				if (length(grep("/", datafile))==0) {
					#if not, make the datafile name similar to conf.file: we expect both should be in the same directory
					datafile <- paste(dirname(conf.file), datafile, sep="/")
				}
			}
			default.panel$train.data <- read.csv(datafile)
			if (!is.null(default.panel$id))
				rownames(default.panel$train.data) <- default.panel$id
			default.panel$nr.patients <- dim(default.panel$train.data)[1]
		}
		else if (length(grep("^levels:", conf.line))>0) {
			if (length(grep("^levels:\"(.+)\"$", conf.line))>0) { # quotes surrounding the level
				default.panel$levels <- strsplit(sub("^levels:\"(.+)\"$", "\\1", conf.line, perl=TRUE), "\",\"")[[1]]
			}
			else { # quotes not surrounding the level
				default.panel$levels <- strsplit(sub("^levels:(.+)$", "\\1", conf.line, perl=TRUE), ",")[[1]]
			}
		}
		else if (length(grep("^min.constr:", conf.line))>0) {
			default.panel$min.constr <- sub("^min.constr:(.+)$", "\\1", conf.line, perl=TRUE)
		}
		else if (length(grep("^constrain.on:", conf.line))>0) {
			default.panel$constrain.on <- sub("^constrain.on:(.+)$", "\\1", conf.line, perl=TRUE)
		}
		else if (length(grep("^panels.of.num:", conf.line))>0) {
			default.panel$panels.of.num <- as.numeric(strsplit(sub("^panels.of.num:(.+)$", "\\1", conf.line, perl=TRUE), ",")[[1]])
			
		}
		else if (is.na(result.file) && length(grep("^outputfile:", conf.line))>0) {
			result.file <- sub("^outputfile:\"(.+)\"$", "\\1", conf.line, perl=TRUE)
			# is this file absolute?
			if (length(grep(":", result.file))==0) { # if not, is it relative?
				if (length(grep("/", result.file))==0) {
					#if not, make the datafile name similar to conf.file: we expect both should be in the same directory
					result.file <- paste(dirname(conf.file), result.file, sep="/")
				}
			}
		}
		else if (length(grep("^#time.start:", conf.line))>0) {
			default.panel$time.start <- as.POSIXct(sub("^#time.start:(.+)$", "\\1", conf.line, perl=TRUE))
			default.panel$time.end <- Sys.time()
		}
	}
	# Restore 'all.predictors' field
	default.panel$all.predictors <- c(default.panel$predictors, default.panel$fixed.predictors)
	# Restore sensible panels.of.num
	default.panel$panels.of.num <- default.panel$panels.of.num + length(default.panel$fixed.predictors)
	# additional default values
	splitted <- split(default.panel$train.data, default.panel$train.data[[default.panel$response]])
	default.panel$nr.class <- c(dim(splitted[[default.panel$levels[1]]])[1], dim(splitted[[default.panel$levels[2]]])[1])
	names(default.panel$nr.class) <- default.panel$levels
	# Instantiate working.panels: stores the best panels
	working.panels <- list()
	# then read the output
	for (line in readLines(result.file, warn=FALSE)) {
		if (length(grep("^Better panel found: ", line))>0) {
			se_str <- sub("^Better panel found: se = (.+); sp = .+$", "\\1", line, perl=TRUE)
			se <- as.numeric(sub(",", ".", se_str))/100
			sp_str <- sub("^Better panel found: se = .+; sp = (.+)$", "\\1", line, perl=TRUE)
			sp <- as.numeric(sub(",", ".", sp_str))/100
			working.panels <- list()
			class(working.panels) <- "exhlist"
		}
		else if (length(grep("^Iterations: ", line))>0){ # this is the last line
			if (length(working.panels) < 1) stop("No panel found.")
			for (i in 1:length(working.panels)) {
				# take number of iterations
				working.panels[[i]]$nr.iter <- as.numeric(sub("^Iterations: (\\d+)$", "\\1", line, perl=TRUE))
			}
			return(working.panels)
		}
		else {
			panel <- default.panel
			matches <- str_match_all(line, "([^;<>]+)( (<|>)|:) ([^;<>]+)(; )?")[[1]]
			panel$panel <- character()
			panel$thresholds <- numeric()
			panel$directions <- list()
			dev.null <- apply(matches, 1, function(match) {
				if (match[2] == "positive at") {
					panel$min.nr <<- as.integer(match[5])
				}
				else {
					panel$panel <<- c(panel$panel, match[2])
					panel$thresholds[[match[2]]] <<- as.numeric(match[5])
					panel$directions[[match[2]]] <<- match[4]
				}
			})
			if (length(working.panels) >0 && length(panel$thresholds) > length(working.panels[[1]]$thresholds))
				next
			panel$sensitivity <- se
			panel$specificity <- sp
			working.panels[[length(working.panels)+1]] <- panel
		}
	}
}


