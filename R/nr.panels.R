# Number of panels

nr.panels <- function(...) {
	UseMethod("nr.panels")
}

nr.panels.exhlist <- function(exhlist, ...) {
	if (is.null(names(exhlist)))
		return(length(exhlist))
	else
		return(length(exhlist[names(exhlist)==""]))
}

nr.panels.exhcv <- function(exhcv, ...) {
	tot <- 0
	for (i in 1:length(exhcv))
		tot <- tot + nr.panels(exhcv[[i]])
	return(tot)
}

nr.panels.exhcvlist <- function(exhcvlist, ...) {
	tot <- 0
	for (i in 1:length(exhcvlist))
		tot <- tot + nr.panels(exhcvlist[[i]])
	return(tot)
}

