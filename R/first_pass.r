first_pass <- function(aligns)
{
  # First pass
  #
  # Return:
  #   T/F value

  nArea <- sort(unique(aligns))
  inde <- sapply(
    nArea, 
    function(y){
      worddiff <- c(diff(aligns == y))
      worddiff <- c(0, worddiff) # compensate start using zero 

      nGaze <- 1:length(worddiff)
      switcher <- FALSE
      inde <- c()
      for(i in nGaze){
        if(!is.na(worddiff[i])){
          if(worddiff[i] == 1) switcher <- TRUE
          if(worddiff[i] == -1) break
          if(switcher) inde <- c(inde, i)
        }
      }
      inde 
    }
  )
  inde <- c(1, unlist(inde))

  r <- rep(FALSE, length(aligns))
  r[inde] <- TRUE
	return(r)
}

## FIRST PASS for aggregation
## returns the duration and counts of regr
#firstPassCalc <- function(word_align, sentence_align, gaze_duration){
#	r <- c(NULL, NULL)
#	nSentence <- sort(unique(sentence_align))
#	for(currsent in nSentence){
#		nWordCurrSent <- word_align[sentence_align == currsent]
#		nWord <- sort(unique(nWordCurrSent))
#		wordFP <- sapply(
#			nWord, 
#			function(y){
#				worddiff <- c(diff(word_align == y))
#				worddiff <- c(0, worddiff) # compensate start using zero 
#
#				nGaze <- 1:length(worddiff)
#				switcher <- FALSE
#				coun <- dura <- 0
#				for(i in nGaze){
#					if(!is.na(worddiff[i])){
#						if(worddiff[i] == 1) switcher <- TRUE
#						if(worddiff[i] == -1) break
#						if(switcher){
#							coun <- coun + 1
#							dura <- dura + gaze_duration[i]
#						}
#					}
#				}
#				c(coun, dura)
#			}
#		)
#		sentFP <- apply(wordFP, 1, sum, na.rm = T)
#		r <- cbind(r, sentFP)
#	}
#
#	r <- data.frame(t(r), row.names = NULL)
#	names(r) <- c('first_pass_count', 'first_pass_duration')
#	return(r)
#}
