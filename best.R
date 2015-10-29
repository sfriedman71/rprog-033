best <- function(p.state, p.outcome) {
  
  ## read data from file
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  acceptable.p.outcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  if( ! any( names(acceptable.p.outcome) == p.outcome)) {
    stop("invalid outcome")  
  }
  
  if( ! any(unique(outcome[,"State"]) == p.state) ) {
    stop("invalid state")
  } 
  
  ## Read outcome data
  outcome.index <- acceptable.p.outcome[p.outcome]
  
  ## coerce chars to nums
  ## outcome[,outcome.index] <- as.numeric(outcome[,outcome.index])
  
  outcome.state <- outcome[outcome[,"State"]==p.state, ]
  
  ## get rows with min value
  min.row <- which.min(outcome.state[,outcome.index])
  
  ## Return hospital name in that state with lowest 30-day death rate
  ## return first match by alphabetic sort on Hospital Name
  result <- sort( outcome.state[min.row,2] )
  result[1]
}

test.best <- function() {
  ## run tests
  source("best.R")
  best("TX", "heart attack")
  ## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
  
  best("TX", "heart failure")
  ## [1] "FORT DUNCAN MEDICAL CENTER"
  
  best("MD", "heart attack")
  ## [1] "JOHNS HOPKINS HOSPITAL, THE"
  
  best("MD", "pneumonia")
  ## [1] "GREATER BALTIMORE MEDICAL CENTER"
  
  best("BB", "heart attack")
  ## Error in best("BB", "heart attack") : invalid state
  
  best("NY", "hert attack")
  ## Error in best("NY", "hert attack") : invalid outcome  
}
