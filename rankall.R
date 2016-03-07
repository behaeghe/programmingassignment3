## Rankall function
rankall <- function(outcome, num = "best") {
                rank <- num
          if (!is.numeric(num)){
               if (num=="best") {
                       rank <- 1
               } 
                else if(num=="worst") { 
                     #do nothing for now   
                }
                else  {
                     return(NA)   
                }
        } 
        

                
        ## Read outcome data
        outcome.index <- getoutcomeindex(outcome)
        ## check arguments state and ouctome
        if (is.na(outcome.index)) {
                stop("invalid outcome")
        }
        outcome.data <- readoutcomedata("data/outcome-of-care-measures.csv",outcome.index)
        outcome.all.rank <- data.frame(Hospital=character(),State=character())
        ## For each state, find the hospital of the given rank
        for (state in sort(unique(outcome.data$State))) {
                ## get the outcome data for the state
                outcome.data.state <- outcome.data[outcome.data$State == state,]
                ## Here's how to order a data frame, call the order function on the cols you need to sort and apply to the rows of the data frame
                ## Remember data frames are addressed row(aka observations), variables (col)
                outcome.data.state <- outcome.data.state[order(outcome.data.state$Outcome,outcome.data.state$Hospital,na.last=NA,decreasing=FALSE),]
                ## get and order the outcome values
                outcome.data.ranked <-sort(outcome.data.state$Outcome,na.last=NA,decreasing=FALSE)
                if (num=="worst"){rank <- nrow(outcome.data.state)}
                 ##get the value the rank
                # option 1: outcome.rank.value <- outcome.data.ranked[rank]
                ## Option 2:
                        outcome.rank.value <- outcome.data.state$Outcome[rank]
                if (!is.na(outcome.rank.value )){
                        ## filter the outcome.data to the specific hospital
                        #outcome.filtered <- outcome.data.state [outcome.data.state$Outcome==outcome.rank.value & !(is.na(outcome.data.state$Outcome)),]
                        outcome.filtered <- outcome.data.state[rank,]
                        outcome.all.rank <- rbind(outcome.all.rank, outcome.filtered[1,c(1,2)])
                } else {
                        outcome.all.rank <- rbind(outcome.all.rank,c(NA,state))
                }

                
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ## Spiff up the data frame (sort by state)
        ##outcome.all.rank <- outcome.all.rank[sort(outcome.all.rank$State),]
        return(outcome.all.rank)
}
