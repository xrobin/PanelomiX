
#' Extraction of an \code{exhlist}
#' @rdname Extract
#' @param x the \code{exhlist} from \code{\link{exh.train}}
#' @param i index to extract
#' @method [[ exhlist
"[[.exhlist" <- function(x, i) {
	ret <- unclass(x)[[i]]
	attr(ret, "exhlist") <- attributes(x)
	return(ret)
}
