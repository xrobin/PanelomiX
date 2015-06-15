calcColors <- function(x, levels = base::levels(as.factor(x)), col=c("green", "red")) {
  colorVec <- rep(NA, length(x))
  colorVec[x == levels[1]] <- col[1]
  colorVec[x == levels[2]] <- col[2]
  return(colorVec)
}
