#"[[" <- function(i, j, ...) {
#  UseMethod("[[", i)
#}

#"[[.default" <- function(i, j, ...) {
#  i[[j, ...]]
#}

"[[.exhlist" <- function(i, j) {
  ret <- unclass(i)[[j]]
  attr(ret, "exhlist") <- attributes(i)
  return(ret)
}
