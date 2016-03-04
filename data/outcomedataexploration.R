## Data exploration
outcome.data <- read.csv("data/outcome-of-care-measures.csv",colClasses ="character")
outcome.state <- unique(outcome.data[[7]])
outcomes <- c("heart attack","heart failure","pneumonia")
outcome.filtered <- outcome.data[,c(2,7,17)]
colnames(outcome.filtered) <- c("Hospital","State","Outcome")
