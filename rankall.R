## Rankall function
rankall <- function(outcome, num = "best") {
        ## Temporary handling of rank
                rank <- num
        ## Read outcome data
        outcome.index <- getoutcomeindex(outcome)
        ## check arguments state and ouctome
        if (is.na(outcome.index)) {
                stop("invalid outcome")
        }
        outcome.data <- readoutcomedata("data/outcome-of-care-measures.csv",outcome.index)
       ## if (!validstate(state,outcome.data$State)) {
       ##                stop("invalid state")    
       ## }
        ## initialize the dataframe we will be returning
        outcome.all.rank <- data.frame(Hospital=character(),State=character())
        ## For each state, find the hospital of the given rank
        for (state in unique(outcome.data$State)) {
                ## get the outcome data for the state
                outcome.data.state <- outcome.data[outcome.data$State == state,]
          
                ## get and order the outcome values
                outcome.data.ranked <-sort(outcome.data.state$Outcome,na.last=NA,decreasing=FALSE)
                ## get the value the rank
                outcome.rank.value <- outcome.data.ranked[rank]
                if (!is.na(outcome.rank.value  )){
                        ## filter the outcome.data to the specific hospital
                        outcome.filtered <- outcome.data.state [outcome.data.state$Outcome==outcome.rank.value & !(is.na(outcome.data.state$Outcome)),]
                        outcome.all.rank <- rbind(outcome.all.rank, outcome.filtered[1,c(1,2)])
                } else {
                        outcome.all.rank <- rbind(outcome.all.rank,c(NA,state))
                }

                
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ## Spiff up the data frame (remove NAs and sort)
        
        return(outcome.all.rank)
}
