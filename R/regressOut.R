# TODO: rewrite this function
regressOutAggr <- function(sentence_align, gaze_duration){
	sentdiff <- diff(sentence_align)	

	numb <- init <- FALSE
	for(i in length(sentdiff):1){
		if(!is.na(sentdiff[i])){
			if(sentdiff[i] < 0) init <- TRUE else if(sentdiff[i] > 0) init <- FALSE 
		}
		numb <- c(init, numb)
	}

	nCount <- nDuration <- c()
	nSentence <- sort(unique(sentence_align))
	for(i in nSentence){
		inde <- numb[sentence_align == i]
		coun <- sum(inde, na.rm = T)
		dura <- sum(gaze_duration[inde], na.rm = T)
		nCount <- c(nCount, coun)
		nDuration <- c(nDuration, dura)
	}


	r <- data.frame(nCount, nDuration)
	names(r) <- c('regress_out_count', 'regress_out_duration')
	return(r)
}

