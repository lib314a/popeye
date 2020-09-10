# TODO: rewrite this function
# This function returns which interest area is regressed in from other ia.
regressIn <- function(aligns, durations = integer()){
	nDiffs <- diff(aligns)
  r <- c(F, sapply(nDiffs, function(x) if(!is.na(x)) if(x < 0) T else F))
  # if *durations* is a vector of the length equal to *aligns*
  # return the aggregation results
  if(length(aligns) == length(durations)){
    nCount <- nDuration <- c()
    nArea <- sort(unique(aligns))
    for(i in nArea){
      inde <- numb & (aligns == i)
      coun <- sum(inde, na.rm = T)
      dura <- sum(durations[inde], na.rm = T)
      nCount <- c(nCount, coun)
      nDuration <- c(nDuration, dura)
    }
    r <- data.frame(
      regress_in_count = nCount,
      regress_in_duration = nDuration
    )
  }
  # else return the boolean vector 
  return(r)
}

#regressInAggr <- function(sentence_align, gaze_duration){
#	sentdiff <- diff(sentence_align)
#
#	numb <- init <- FALSE
#	for(i in 1:length(sentdiff)){
#		if(!is.na(sentdiff[i])){
#			if(sentdiff[i] < 0) init <- TRUE else if(sentdiff[i] > 0) init <- FALSE
#		}
#		numb <- c(numb, init)
#	}
#	
#	nCount <- nDuration <- c()
#	nSentence <- sort(unique(sentence_align))
#	for(i in nSentence){
#		inde <- numb & (sentence_align == i)
#		coun <- sum(inde, na.rm = T)
#		dura <- sum(gaze_duration[inde], na.rm = T)
#		nCount <- c(nCount, coun)
#		nDuration <- c(nDuration, dura)
#	}
#
#	r <- data.frame(nCount, nDuration)
#	names(r) <- c('regress_in_count', 'regress_in_duration')
#	return(r)
#}
