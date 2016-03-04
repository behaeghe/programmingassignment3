best <- function(state, outcome) {
        ## State: 7
        ## HeartAttack: 11  
        ## Hearth Failure: 17
        ## Pneumonia: 23
        
        ## Read outcome data
        outcome.data <- read.csv("data/outcome-of-care-measures.csv",colClasses ="character")
        outcome.state <- unique(outcome.data[[7]])
        outcomes <- c("heart attack","heart failure","pneumonia")
        ## check if state is valid 
        if (!sum(outcome.state==state)) {
                stop("invalid state")
                
        }
        ## check if outcome arg is valid
        if (!sum(outcomes==outcome)){
                stop("invalid outcome")
        }
        ## associate outcome to the correct column index
        if (outcome == "heart attack") {
             ouctomes.index <- 11
         }
        else if(outcome=="heart failure"){
             outcomes.index <- 17
        }
        else if (outcome=="pneumonia"){
             outcomes.index <- 23
        }
        ## splicing the dataframe to retain the hospital name, state and outcome value
        outcome.filtered <- outcome.data[,c(2,7,outcomes.index)]
        colnames(outcome.filtered) <- c("Hospital","State","Outcome")
        ## coercing the type for outcome from Char to numeric (NAs will be introduced but that's OK)
        suppressWarnings(outcome.filtered$Outcome <- as.numeric(outcome.filtered$Outcome))
        ## selecting the state observations
        outcome.filtered <- outcome.filtered[outcome.filtered[[2]]==state,]
        ## we now have a data frame outcome.filtered with 3 variables
        ## let's find the min
        outcome.min <- min(outcome.filtered[[3]],na.rm=TRUE)
        ## let's filter the dataframe to get all possible hospitals meeting the min
        outcome.filtered <- outcome.filtered[outcome.filtered[[3]]==outcome.min,]
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        return(outcome.filtered[[1,1]])
        
}