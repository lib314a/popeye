go_past <- function(word_align, sentence_align, gaze_duration){
    r <- c(NULL, NULL)
    nSentence <- sort(unique(sentence_align))
    for(currsent in nSentence){
        nWord <- sort(unique(word_align[sentence_align == currsent]))
        wordGP <- sapply(
            nWord, 
            function(x){
                wordposi <- word_align - x
                nGaze <- 1:length(wordposi)
                switcher <- FALSE
                coun <- dura <- 0
                for(i in nGaze){
                  if(!is.na(wordposi[i])){
                    if(wordposi[i] <= 0){
                        if(wordposi[i] == 0) switcher <- TRUE
                        if(switcher){
                            coun <- coun + 1
                            dura <- dura + gaze_duration[i]
                        }
                    }else{
                        break
                    }
                  }
                }
                c(coun, dura)
            }
        )
        sentGP <- apply(wordGP, 1, sum, na.rm = T)
        r <- cbind(r, sentGP)
    }

    r <- data.frame(t(r), row.names = NULL)
    names(r) <- c('reg_path_count', 'reg_path_duration')
    return(r)
}


