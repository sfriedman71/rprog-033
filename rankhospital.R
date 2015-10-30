rankhospital <- function(p.state, p.outcome, p.num = "best") {
  
  ## Read outcome data
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that outcome is valid
  acceptable.p.outcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  if( ! any( names(acceptable.p.outcome) == p.outcome)) {
    stop("invalid outcome")  
  }
  
  ## Check that state is valid
  if( ! any(unique(outcome[,"State"]) == p.state) ) {
    stop("invalid state")
  } 
  
  ## Read outcome data
  outcome.index <- acceptable.p.outcome[p.outcome]
  
  outcome.state <- outcome[outcome[,"State"]==p.state, ]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}




test.rankhospital <- function() {
  
  rankhospital("TX", "heart failure", 4)
  # [1] "DETAR HOSPITAL NAVARRO"
  
  rankhospital("MD", "heart attack", "worst")
  #[1] "HARFORD MEMORIAL HOSPITAL"

  rankhospital("MN", "heart attack", 5000)
  #[1] NA
}