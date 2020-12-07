#' Train a panel with the new java interface
#' @return a list (class \code{exhlist}) with one or more panels (class \code{exh})
#' @param data the dataset
#' @param predictors the name of the columns to be tested as predictors in the panel. May or may not be used.
#' @param response the binary response column name
#' @param fixed.predictors predictors to force into the panel
#' @param id a column with sample ids
#' @param constrain.on,min.constr objective constrains. Consider only panel with \code{min.nr} \dQuote{specificity} or \dQuote{sensitivity}, or maximize global \dQuote{accuracy}
#' @param levels the values of \code{response} to use as negative and positive data
#' @param panels.of.num a vector of integer, defining the acceptable number of markers included in the panel
#' @param test.thresholds.predictors the thresholds of predictors to test. Ignored if filter.randomForest is a list
#' @param test.thresholds.fixed.predictors the thresholds of fixed predictors to test. Ignored if filter.randomForest is a list
#' @param directions a named list (after the \code{predictors} and \code{fixed.predictors}) of directions as described in \code{\link[pROC]{roc}}
#' @param filter.number.thresholds if \code{TRUE}, filter the \code{test.thresholds} to speed up the search. Ignored if filter.randomForest is a list. Can be the character \dQuote{2x2} or a numeric (length 1)
#' @param filter.randomForest a \code{\link{list}} with 2 elements: \code{molecules} (number of markers to keep) \code{thresholds} (number of thresholds to keep per marker)
#' @param verbose enables additional output for debugging
#' @param multiple.panels one of \dQuote{all}, \dQuote{random} or \dQuote{first} when multiple panels are equivalent, which one(s) to report
#' @param na.rm one of \dQuote{all} or \dQuote{local}
#' @param working.dir a (preferably empty) directory where the files required by java will be placed. If \code{\link{missing}}, \code{NULL} \code{NA} or an empty string, a temporary folder will be created and removed afterwards, otherwise the given string will be used and files will be kept in place. For example \code{\link{tempdir}()}.
#' @param rand.response randomize the response.
#' @param limit.java.threads how many threads to use. If \code{NA} (default), java will define itself how many threads to use. If an integer is given, will force to this number
#' @param progress whether to display a graphical progress bar
#' @param ... additional arguments passed from and to other methods. In particular, further arguments for to \code{\link[pROC]{roc}}.
#' @examples
#' data(aSAH, package = "pROC")
#' exh.train(aSAH, c("age", "s100b", "ndka"), "outcome")
#' @export
#' @import pROC
#' @import methods
#' @import rJava
#' @importFrom stats as.formula na.omit
#' @importFrom utils combn read.csv write.csv
#' @importFrom R.utils getAbsolutePath
exh.train <- function(data, predictors, response,
                      fixed.predictors = NULL,
                      id = NULL,
                      constrain.on = c("specificity", "sensitivity", "accuracy"),
                      min.constr = 0.95,
                      levels = base::levels(as.factor(data[[response]])),
                      panels.of.num = 1:length(predictors),
					  test.thresholds.predictors = NA, # ignored if filter.randomForest is a list
					  test.thresholds.fixed.predictors = NA, # ignored if filter.randomForest is a list
                      directions = NULL,
                      filter.number.thresholds = NA, # ignored if filter.randomForest is a list
                      filter.randomForest = FALSE,
                      verbose = TRUE,
                      multiple.panels = c("all", "random", "first"),
                      na.rm = c("all", "local"),
                      rand.response = FALSE,
                      limit.java.threads = NA,
                      working.dir = NULL,
                      progress = TRUE,
                      ...) {

  # Initialize rJava
  .jinit(".")
  # Match arguments
  constrain.on <- match.arg(constrain.on)
  na.rm <- match.arg(na.rm)
  multiple.panels <- match.arg(multiple.panels)
  all.predictors <- c(predictors, fixed.predictors)
  test.thresholds <- c(test.thresholds.predictors, test.thresholds.fixed.predictors)
  # Remove nas
  if (na.rm == "all") {
    data <- data[!apply(as.data.frame(data[, all.predictors]), 1, function(x) {
      any(is.na(x))
    }), ]
  }
  # Remove unneeded columns in the data
  data <- data[, c(id, predictors, fixed.predictors, response)]
  # Remove patients with unneeded levels
  data <- data[data[[response]] %in% levels, ]
  # And remove unused levels
  data[[response]] <- factor(data[[response]], levels = levels)
  exh <- list()
  class(exh) <- "exh"
  exh$time.start.r <- Sys.time()
  exh$train.data <- data
  #  exh$min.nr <- min.nr # positive molecules to have a positive test
  exh$predictors <- predictors
  exh$fixed.predictors <- fixed.predictors
  exh$all.predictors <- all.predictors
  exh$response <- response
  if (rand.response) {
    exh$response <- sample(exh$response)
  }
  exh$levels <- levels
  exh$id <- id
  exh$nr.patients <- length(data[[response]])
  exh$nr.required.mol <- 0
  exh$panels.of.num <- panels.of.num
  splitted <- split(as.data.frame(data[, all.predictors]), data[[response]])
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
  exh$optimize <- ifelse(constrain.on == "specificity", "sensitivity", "specificity") # sp or se?
  exh$min.constr <- min.constr
  exh$max.perf <- -1 # stores the maximum sp or se achieved
  exh$nr.iter <- 0 # count iterations (informative)
  exh$best.panels <- list() # placeholder to stores the best panel thresholds
  # exh$best.panels

  ### Determine the directions ###
  if (is.null(directions)) {
    directions <- list()
  }
  if (any(sapply(directions[all.predictors], is.null))) {
    for (predictor in all.predictors) {
      if (is.null(directions[[predictor]])) {
        curve <- pROC::roc(data[[response]], data[[predictor]], levels = levels, quiet=TRUE)
        directions[[predictor]] <- curve$direction
      }
    }
  }
  exh$directions <- directions

  ### Now determine the thresholds ###
  # Filter with randomForest
  if (is.list(filter.randomForest)) {
    requireNamespace("randomForest", quietly = TRUE)

    # Filter by random forest
    # Create the class weights for RF
    classwt <- if (constrain.on == "specificity") {
      c(min.constr, 1 - min.constr)
    } else {
      c(1 - min.constr, min.constr)
    }

    # More intellingent filtering of markers 1 by 1:
    predictors.filtered <- predictors
    all.predictors.filtered <- all.predictors
    while (length(all.predictors.filtered) > filter.randomForest$molecules) {
      formula <- as.formula(paste("as.factor(", response, ") ~", paste(all.predictors.filtered, collapse = "+")))
      rfModel <- randomForest::randomForest(formula, data = data, classwt = classwt)
      predictors.filtered <- filter.markers.randomForest(
        rfmodel = rfModel,
        response = response, predictors = predictors.filtered,
        fixed.predictors = fixed.predictors, data = data,
        levels = levels, remove.n.mols = 1
      )
      all.predictors.filtered <- c(predictors.filtered, fixed.predictors)
    }
    # Now we've filtered predictors out, it's our new predictors!
    # predictors <- predictors.filtered
    # all.predictors <- all.predictors.filtered
    # We can now determine the thresholds
    formula <- as.formula(paste("as.factor(", response, ") ~", paste(all.predictors.filtered, collapse = "+")))
    classwt <- if (constrain.on == "specificity") {
      c(min.constr, 1 - min.constr)
    } else {
      c(1 - min.constr, min.constr)
    }
    rfModel <- randomForest::randomForest(formula, data = data, classwt = classwt)
    possible.thresholds <- filter.thresholds.randomForest(
      rfmodel = rfModel,
      response = response, predictors = all.predictors.filtered,
      fixed.predictors = fixed.predictors, data = data,
      levels = levels, num = filter.randomForest$thresholds,
      limit.mols = filter.randomForest$molecules
    )
    #        # Test: how frequently are the markers chosen? Old method
    #        chosen.mols <- c()
    #        n <- 100
    #        thresh.found <- replicate(n, {
    #          classwt <- if (constrain.on == "specificity") {c(min.constr, 1 - min.constr)} else {c(1 - min.constr, min.constr)}
    #          rfModel <- randomForest(formula, data = data, classwt=classwt)
    #          possible.thresholds <- filter.thresholds.randomForest(rfmodel=rfModel,
    #                                   response=response, predictors=all.predictors.filtered,
    #                                   fixed.predictors=fixed.predictors, data=data,
    #                                   levels=levels, num=filter.randomForest$thresholds,
    #                                   limit.mols=filter.randomForest$molecules)
    #          chosen.mols <<- c(chosen.mols, names(possible.thresholds))
    #          if ("vcam.A" %in% names(possible.thresholds) && any(possible.thresholds$vcam.A > 658174.81 & possible.thresholds$vcam.A < 659515.72)) {
    #            return(1)
    #          }
    #          return(0)
    #        })
    #        sort(table(chosen.mols), decreasing=TRUE)/n
    #        thresh.found
    #
    #        # New method
    #        chosen.mols <- c()
    #        n <- 100
    #        thresh.found <- replicate(n, {
    #          predictors.filtered <- predictors
    #          all.predictors.filtered <- all.predictors
    #          while (length(all.predictors.filtered) > filter.randomForest$molecules) {
    #            formula <- as.formula(paste("as.factor(", response, ") ~", paste(all.predictors.filtered, collapse="+")))
    #            classwt <- if (constrain.on == "specificity") {c(min.constr, 1 - min.constr)} else {c(1 - min.constr, min.constr)}
    #            rfModel <- randomForest(formula, data = data, classwt=classwt)
    #            predictors.filtered <- filter.markers.randomForest(rfmodel=rfModel,
    #                                     response=response, predictors=predictors.filtered,
    #                                     fixed.predictors=fixed.predictors, data=data,
    #                                     levels=levels, remove.n.mols=1)
    #            all.predictors.filtered <- c(predictors.filtered, fixed.predictors)
    #          }
    #          classwt <- if (constrain.on == "specificity") {c(min.constr, 1 - min.constr)} else {c(1 - min.constr, min.constr)}
    #          rfModel <- randomForest(formula, data = data, classwt=classwt)
    #          possible.thresholds <- filter.thresholds.randomForest(rfmodel=rfModel,
    #                                   response=response, predictors=all.predictors.filtered,
    #                                   fixed.predictors=fixed.predictors, data=data,
    #                                   levels=levels, num=filter.randomForest$thresholds,
    #                                   limit.mols=filter.randomForest$molecules)
    #          chosen.mols <<- c(chosen.mols, names(possible.thresholds))
    #          if ("vcam.A" %in% names(possible.thresholds) && any(possible.thresholds$vcam.A > 658174.81 & possible.thresholds$vcam.A < 659515.72)) {
    #            return(1)
    #          }
    #          return(0)
    #        })
    #        sort(table(chosen.mols), decreasing=TRUE)/n
    #        thresh.found

    # as we possibly changed the number and values of predictors (with limit.mols), reset it:
    exh$all.predictors <- all.predictors <- names(possible.thresholds)
    exh$predictors <- predictors <- all.predictors[!all.predictors %in% fixed.predictors]
  }
  else { # or use pROC to determine the possible thresholds if missing from test.thresholds
    if (all(is.na(test.thresholds))) {
      for (pred in all.predictors) {
        predictor.roc <- pROC::roc(data[[response]], data[[pred]], levels = levels, direction = directions[[predictor]])
        coords.vector <- pROC::coords(predictor.roc, "l", ret = "t", transpose = "FALSE")$threshold
        coords.vector <- coords.vector[is.finite(coords.vector)] # remove infinite values generated by pROC
        possible.thresholds[[pred]] <- coords.vector
      }
    }
    else {
    	if (length(test.thresholds.predictors) == length(predictors) & all(is.na(test.thresholds.fixed.predictors))){
    		test.thresholds <- as.data.frame(t(c(test.thresholds.predictors, rep(NA, length(fixed.predictors)))))
    		colnames(test.thresholds) <- all.predictors
    	} else if (length(test.thresholds.fixed.predictors) == length(fixed.predictors) & all(is.na(test.thresholds.predictors))) { 
    		test.thresholds <- as.data.frame(t(c(rep(NA, length(predictors)), test.thresholds.fixed.predictors)))
    		colnames(test.thresholds) <- all.predictors
    	} else {
    		print("Number of thresholds to test is not equal to number of (fixed) predictors.")
    		break; 
    	} 
    	
    	for (pred in all.predictors) {
    		predictor.roc <- pROC::roc(data[[response]], data[[pred]], levels = levels, direction = directions[[predictor]])
    		coords.vector <- pROC::coords(predictor.roc, 
    									  "l", ret = "t", transpose = "FALSE")$threshold
    		coords.vector <- coords.vector[is.finite(coords.vector)] # remove infinite values generated by pROC
    		
    		if (is.na(test.thresholds[[pred]])) {
    			possible.thresholds[[pred]] <- coords.vector
    		} else { 
    			possible.thresholds[[pred]] <- rep(test.thresholds[[pred]], length(coords.vector))
    		}
    	}
   }
  }

  if (!is.na(filter.number.thresholds)) {
    if (is.character(filter.number.thresholds) & length(filter.number.thresholds) == 1 & filter.number.thresholds == "2x2") {
      print("Filtering thresholds by 2x2 combinations")
      # Test all the 2x2 combinations for the thresholds
      combinations <- combn(predictors, 2)
      # initialize the list
      thresholds.2x2 <- list()
      for (i in 1:dim(combinations)[2]) {
        cur_combination <- c(combinations[, i], fixed.predictors)
        print(cur_combination)
        e <- exh.train(data, cur_combination, response, constrain.on = constrain.on, min.constr = min.constr, levels = levels, panels.of.num = 2, test.thresholds = test.thresholds, verbose = FALSE, multiple.panels = "all", ...)
        # take the thresholds and merge to the list
        for (panel in e) {
          for (mol in names(panel$threshold)) {
            thresholds.2x2[[mol]] <- c(thresholds.2x2[[mol]], panel$threshold[[mol]])
          }
        }
      }
      # simplify the list
      for (pred in all.predictors) {
        thresholds.2x2[[pred]] <- unique(thresholds.2x2[[pred]])
      }
      # from now on use this list of thresholds
      possible.thresholds <- thresholds.2x2
      print("New 2x2 thresholds")
      print(thresholds.2x2)
    }
    if (is.numeric(filter.number.thresholds) & length(filter.number.thresholds) == 1) {
      #      limit.se <- list()
      #      limit.sp <- list()
      #      best.sp <- list()
      #      best.se <- list()
      #      best.thresholds <- list()
      for (pred in all.predictors) {
        #        if (pred == "bioplex.mip1a.A")
        num.thresholds.for.this.pred <- length(possible.thresholds[[pred]])
        if (filter.number.thresholds < num.thresholds.for.this.pred) {
          #          # basic way: find n thresholds equally spread through the ROC curve
          #          i <- round(seq(1, num.thresholds.for.this.pred, length.out=filter.number.thresholds))
          #          possible.thresholds[[pred]] <- possible.thresholds[[pred]][i]
          # more intelligent way: determine n intervals and find the best threshold inside each interval
          r <- roc(data[[response]], data[[pred]], levels = levels, direction = directions[[pred]], plot = FALSE, ...)
          sq <- sort(rep(c(1:filter.number.thresholds), length.out = length(r$SES))) # determine the regions
          sums <- r$SES + r$SPS # we'll look for the best sums SE + SP in each region
          new.thresholds <- c()
          #          new.best.se <- new.best.sp <- c()
          #          new.limit.se <- r$SES[length(r$SES)]
          #          new.limit.sp <- r$SPS[length(r$SPS)]
          for (region in 1:filter.number.thresholds) {
            best.idx.region <- order(sums[sq == region], decreasing = TRUE)[1] # find the index of the best threshold in the region
            new.thresholds <- c(new.thresholds, r$thresholds[sq == region][best.idx.region]) # get the corresponding threshold
            #            new.limit.se <- c(new.limit.se, r$SES[sq==region][1])
            #            new.limit.sp <- c(new.limit.sp, r$SPS[sq==region][1])
            #            new.best.se <- c(new.best.se, r$SES[sq==region][best.idx.region])
            #            new.best.sp <- c(new.best.sp, r$SPS[sq==region][best.idx.region])
          }
          #          limit.se[[pred]] <- new.limit.se
          #          limit.sp[[pred]] <- new.limit.sp
          #          best.se[[pred]] <- new.best.se
          #          best.sp[[pred]] <- new.best.sp
          possible.thresholds[[pred]] <- new.thresholds
        }
      }
    }
  }
  exh$possible.thresholds <- possible.thresholds

  # Initialize the class
  exh$time.start.java <- Sys.time()
  panelomix <- .jnew("ch/unige/bprg/panelomix/PanelomiX")

  # Prepare a temp dir
  remove.tmp.dir.after <- FALSE
  if (missing(working.dir) || is.null(working.dir) || is.na(working.dir) || working.dir == "") {
    remove.tmp.dir.after <- TRUE
    working.dir <- tempfile("PanelomiX_")
  }
  else {
    # make sure the path is absolute
    working.dir <- getAbsolutePath(working.dir)
  }
  
  dir.create(working.dir, showWarnings = FALSE, recursive = TRUE)
  if (verbose) {
  	message(paste0("Creating temporary file in ", working.dir, "."))
  }

  data.filename <- file.path(working.dir, "data.csv")
  data.to.write <- exh$train.data[c(exh$id, exh$all.predictors, exh$response)]
  data.to.write <- subset(data.to.write, exh$train.data[[exh$response]] == exh$levels[1] | exh$train.data[[exh$response]] == exh$levels[2])
  write.csv(data.to.write, row.names = FALSE, file = data.filename)

  # see http://java.sun.com/docs/books/jni/html/types.html for JNI return values syntax
  .jcall(panelomix, , "setDataFile", data.filename)
  .jcall(panelomix, , "setResponse", exh$response)
  .jcall(panelomix, , "setLevels", exh$levels)
  for (mol in exh$fixed.predictors) {
    .jcall(panelomix, "Z", "putFixedPredictor", mol, directions[[mol]], .jfloat(exh$possible.thresholds[[mol]]))
  }
  for (mol in exh$predictors) {
    .jcall(panelomix, "Z", "putPredictor", mol, directions[[mol]], .jfloat(exh$possible.thresholds[[mol]]))
  }
  .jcall(panelomix, , "setOptimizeOptions", exh$constrain.on, .jfloat(exh$min.constr[1]))

  # Define the panel sizes for java:
  panels.of.num <- panels.of.num - length(fixed.predictors)
  panels.of.num <- panels.of.num[panels.of.num > 0]

  # Here we put an ugly hack.
  # For some reason .jcall will sometimes produce an ArrayIndexOutOfBoundsException on the first call to setPanelSizes for no apparent reason.
  # Repeating the call a second time with the same parameters seems to work.
  try(.jcall(panelomix, , "setPanelSizes", panels.of.num), silent = TRUE)
  # So check if the data was inserted correctly (panels.of.num is identical to getPanelSizes). Otherwise, repeat.
  if (!identical(panels.of.num, .jcall(panelomix, "[I", "getPanelSizes"))) {
    .jcall(panelomix, , "setPanelSizes", panels.of.num)
  }

  #  if (!is.null(id))
  #    cat("id:", id, "\n", sep="", file=conf.filename, append=TRUE)
  if (is.numeric(limit.java.threads)) {
    .jcall(panelomix, , "setNProc", as.integer(limit.java.threads))
  }

  if (!is.null(progress) && is.character(progress)) { # progress file
    .jcall(panelomix, , "setProgressBar", FALSE)
    .jcall(panelomix, , "setProgressFile", progress)
  }
  else if (!is.null(progress) && is.numeric(progress)) { # progress bar
    .jcall(panelomix, , "setMonitorTime", as.integer(progress))
  }
  else if (!is.null(progress) && progress) { # progress bar
    .jcall(panelomix, , "setProgressBar", TRUE)
  }
  else { # no progress bar / file
    .jcall(panelomix, , "setProgressBar", FALSE)
  }

  jPanelList <- .jcall(panelomix, "Lch/unige/bprg/panelomix/PanelList;", "run")

  exh$time.end <- Sys.time()

  panels <- list()
  class(panels) <- "exhlist"
  attr(panels, "iterations") <- .jcall(panelomix, "J", "getIterations")
  attr(panels, "time") <- .jcall(panelomix, "D", "getExecTime")
  attr(panels, "train.data") <- data.to.write
  attr(panels, "response") <- exh$response
  attr(panels, "levels") <- exh$levels
  attr(panels, "predictors") <- exh$predictors
  attr(panels, "possible.thresholds") <- exh$possible.thresholds
  attr(panels, "all.predictors") <- exh$all.predictors
  attr(panels, "fixed.predictors") <- exh$fixed.predictors
  attr(panels, "nr.patients") <- exh$nr.patients
  attr(panels, "panels.of.num") <- exh$panels.of.num
  attr(panels, "constrain.on") <- exh$constrain.on
  attr(panels, "min.constr") <- exh$min.constr
  attr(panels, "panels.of.num") <- exh$panels.of.num
  attr(panels, "time.start.r") <- exh$time.start.r
  attr(panels, "time.start.java") <- exh$time.start.java
  attr(panels, "time.end") <- exh$time.end
  size <- .jcall(jPanelList, "I", "getMinLength")
  sensitivity <- .jcall(jPanelList, "F", "getSensitivity")
  specificity <- .jcall(jPanelList, "F", "getSpecificity")
  attr(panels, "size") <- size
  attr(panels, "sensitivity") <- sensitivity
  attr(panels, "specificity") <- specificity
  for (jPanel in as.list(jPanelList)) {
    panel <- list()
    class(panel) <- "exh"
    panel$response <- exh$response
    panel$levels <- exh$levels
    panel$sensitivity <- sensitivity
    panel$specificity <- specificity
    panel$size <- size

    # Get the panel's thresholds
    jThresholds <- .jcall(jPanel, "Ljava/util/List;", "getThresholds")
    markers <- directions <- character(size)
    thresholds <- numeric(size)
    i <- 1
    for (jThreshold in as.list(jThresholds)) {
      markers[i] <- .jcall(jThreshold, "S", "getMarker")
      directions[i] <- .jcall(jThreshold, "S", "getDirection")
      thresholds[i] <- .jcall(jThreshold, "F", "getThreshold")
      i <- i + 1
    }
    panel$panel <- markers
    names(thresholds) <- markers
    panel$thresholds <- thresholds
    names(directions) <- markers
    panel$directions <- directions

    # Get the panel's minNr
    panel$min.nr <- .jcall(jPanel, "I", "getPositiveAt")

    # Add to panels
    panels[[length(panels) + 1]] <- panel
  }
  if (remove.tmp.dir.after) {
    unlink(working.dir, recursive = TRUE)
  }
  return(panels)
}
