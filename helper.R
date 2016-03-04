## Helpfer functions for assignment 3 exercise
validoutcome <- function(outcome){
        ## check that the ouctome argument matches one of "heart attack","heart failure","pneumonia"
        outcomes <- c("heart attack","heart failure","pneumonia")
        return(as.logical(sum(outcome==ouctomes)))
}
validstate <- function(state,state.list) {
        ## check that the state vairable is in state.list
        return(as.logical(sum(state.list==state)))
}
readoutcomedata <- function(file.path,oindex) {
        ## read and prep the ouctome data
        df <- read.csv(file.path,colClasses ="character")
        df <- df[,c(2,7,oindex)]
        colnames(df) <- c("Hospital","State","Outcome")
        ## coercing the type for outcome from Char to numeric (NAs will be introduced but that's OK)
        suppressWarnings(df$Outcome <- as.numeric(df$Outcome))
        return(df)
}
getoutcomeindex <- function(outcome){
        if (outcome == "heart attack") {
               return(11) 
        }
        else if(outcome=="heart failure"){
                return(17)
        }
        else if (outcome=="pneumonia"){
                return(23)
        }
        return(NA)
}