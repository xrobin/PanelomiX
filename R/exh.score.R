# data: data frame with data (predictors vectors only)
# thresholds: thresholds for predictors
# Return value: an integer vector of scores
#exh.score <- function(data, thresholds) {
#  s <- rep(0, length(data[,1]))
#  mol.scores <- data>thresholds # WRONG? when called from outside exh.test
#  for(i in 1:length(data[,1])) {
#    #s[i] <- sum(data[i,]>thresholds)
#    s[i] <- sum(mol.scores[i,]) # WRONG? when called from outside exh.test
#  }
#  return(s)
#}
#exh.score2 <- function(data, thresholds) { # Cleaner (no loop) but slower # WRONG
#  mol.scores <- data>thresholds
#  apply(mol.scores, 1, sum)
#}
# to be used from outside exh.test...
## WARNING: exh.score3 does only work when all directions are >.
## Please switch to exh.score4 that should handle directions < correctly too.
#exh.score3 <- function(data, thresholds, direction) {
#  s <- rep(0, length(data[,1]))
#  # Take data opposite for markers with direction == "<"
#  data[,direction == "<"] <- - data[,direction == "<"]
#  thresholds[direction == "<"] <- - thresholds[direction == "<"]
#  
#  for(i in 1:length(data[,1])) {
#    s[i] <- sum(data[i,]>thresholds)
#  }
#  return(s)
#}


exh.score3gt <- function(data, thresholds) {
	s <- rep(0, length(data[,1]))
	for(i in 1:length(data[,1])) {
		s[i] <- sum(data[i,]>thresholds)
	}
	return(s)
}

exh.score3lt <- function(data, thresholds) {
	s <- rep(0, length(data[,1]))
	for(i in 1:length(data[,1])) {
		s[i] <- sum(data[i,]<thresholds)
	}
	return(s)
}

exh.score4 <- function(data, thresholds, direction) {
	# Send biomarkers with direction > to exh.score3gt 
	slt <- sgt <- 0
	if (any(direction == ">"))
		sgt <- exh.score3gt(data[direction == ">"], thresholds[direction == ">"])
	# Send biomarkers with direction < to exh.score3lt
	if (any(direction == "<"))
		slt <- exh.score3lt(data[direction == "<"], thresholds[direction == "<"])
	return(sgt + slt)
}

