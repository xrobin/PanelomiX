# Used by predict

exh.score3gt <- function(data, thresholds) {
	s <- rep(0, nrow(data))
	for(i in 1:nrow(data)) {
		s[i] <- sum(data[i,]>thresholds)
	}
	return(s)
}

exh.score3lt <- function(data, thresholds) {
	s <- rep(0, nrow(data))
	for(i in 1:nrow(data)) {
		s[i] <- sum(data[i,]<thresholds)
	}
	return(s)
}

exh.score4 <- function(data, thresholds, direction) {
	if(is.null(data)) {
		stop("Data to score is NULL")
	}
	# Send biomarkers with direction > to exh.score3gt
	slt <- sgt <- 0
	if (any(direction == ">"))
		sgt <- exh.score3gt(data[direction == ">"], thresholds[direction == ">"])
	# Send biomarkers with direction < to exh.score3lt
	if (any(direction == "<"))
		slt <- exh.score3lt(data[direction == "<"], thresholds[direction == "<"])
	return(sgt + slt)
}

