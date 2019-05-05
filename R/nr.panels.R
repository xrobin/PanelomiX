#' Counts the number of panels found
#' @param x the panels as an \code{exhlist}, single \code{exhcv} or \code{exhcvlist}
#' @export
nr.panels <- function(x) {
	UseMethod("nr.panels")
}

#' @export
#' @rdname nr.panels
nr.panels.exhlist <- function(x) {
	if (is.null(names(x)))
		return(length(x))
	else
		return(length(x[names(x)==""]))
}

#' @export
#' @rdname nr.panels
nr.panels.exhcv <- function(x) {
	tot <- 0
	for (i in 1:length(x))
		tot <- tot + nr.panels(x[[i]])
	return(tot)
}

#' @export
#' @rdname nr.panels
nr.panels.exhcvlist <- function(x) {
	tot <- 0
	for (i in 1:length(x))
		tot <- tot + nr.panels(x[[i]])
	return(tot)
}

