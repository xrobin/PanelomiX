plot.thr.stability <- plot.thr.stability <- function(...) {
  UseMethod("plot.thr.stability")
}

# plot: a logical

plot.new.thr.stability <- function(found.mols, data, main="Panel thresholds", exhobject=NULL, smart.mol.list=NULL, legend=TRUE, ...) {
  opa <- par(las=3)
  plot(NA, NA, xlim=c(1, length(found.mols)), ylim=c(0, dim(data)[1]), xlab="", ylab="threshold rank", main=main, axes=F, ...)
  box()
  axis(2)
  if (! is.null(smart.mol.list))
      names(found.mols) <- sapply(names(found.mols), function(x, sml) {return(sml[[x]])}, sml=smart.mol.list)
  axis(1, at=1:length(found.mols), labels=names(found.mols))
  if (!is.null(exhobject) && legend) {
    min.nrs <- table.min.nr.stability(exhobject)
    min.nrs <- min.nrs[min.nrs!=0]
    legend("bottomright", title="Min nr", legend=names(min.nrs), fill=as.numeric(names(min.nrs))+1)
  }
  par(opa)
}

plot.thr.stability.exhlist <- function(exhlist, data=NULL, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
  if (is.null(found.mols)) {
    found.mols <- table.mol.stability(exhlist)
    found.mols <- found.mols[found.mols>0]
    found.mols <- sort(found.mols, decreasing=TRUE)
  }
  if (is.null(data)) {
    if (is.null(exhlist$train.data)) {
      if (is.null(exhlist[1]$train.data)) {
        stop("No data found in plot.thr.stability.exhlist")
      }
      else {
        data <- exhlist[[1]]$train.data
      }
    }
    else {
      data <- exhlist$train.data
    }
  }
  if (!add) {
    plot.new.thr.stability(found.mols, data, exhobject=exhlist, ...)
  }
  if (plot) {
    # Filter exhlist so that we do not have training.data and test.data
    if (is.null(names(exhlist))) {
      n.panels <- length(exhlist)
    }
    else {
      n.panels <- length(exhlist[names(exhlist)==""])
    }
    # Plot the line for each panel
    for (i in 1:n.panels) {
      panel <- exhlist[[i]]
      x <- y <- c()
      for (j in 1:length(names(panel$thresholds))) {
        y <- c(y, floor(rank(c(panel$thresholds[j], data[[names(panel$thresholds)[j]]])))[1])
        x <- c(x, grep(names(panel$thresholds)[j], names(found.mols)))
      }
      idx <- order(x)
      points(x[idx], y[idx], type="o", col=panel$min.nr+1, ...)
    }
  }
}

plot.thr.stability.exhcv <- function(exhcv, data, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
  if (is.null(found.mols)) {
    found.mols <- table.mol.stability(exhcv)
    found.mols <- found.mols[found.mols>0]
    found.mols <- sort(found.mols, decreasing=TRUE)
  }
  if (!add) {
    plot.new.thr.stability(found.mols, data, exhobject=exhcv, ...)
  }
  if (plot) {
    k <- length(exhcv)
    for (i in 1:k) {
      plot.thr.stability(exhcv[[i]], data=data, found.mols=found.mols, add=TRUE, ...)
    }
  }
}

plot.thr.stability.exhcvlist <- function(exhcvlist, data, found.mols=NULL, add=FALSE, plot=TRUE, ...) {
  if (is.null(found.mols)) {
    found.mols <- table.mol.stability(exhcvlist)
    found.mols <- found.mols[found.mols>0]
    found.mols <- sort(found.mols, decreasing=TRUE)
  }
  if (!add) {
    plot.new.thr.stability(found.mols, data, exhobject=exhcvlist, ...)
  }
  if (plot) {
    nreps <- length(exhcvlist)
    for (rep in 1:nreps) {
      plot.thr.stability(exhcvlist[[rep]], data=data, found.mols=found.mols, add=TRUE, ...)
    }
  }
}

