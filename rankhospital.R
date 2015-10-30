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
  
  ## Index and index outcome data
  outcome.index <- acceptable.p.outcome[p.outcome]
  
  ## Coerce characters into numbers
  outcome[,outcome.index] <- as.numeric(outcome[,outcome.index])
  
  hospital.index <- 2
  state.index <- 7
  
  outcome <- subset(outcome
                    , State == p.state & !is.na(outcome[,outcome.index])
                    , select = c(hospital.index, outcome.index) )
  
  ## Reset indexes based on order in anon vector of select param above
  hospital.index <- 1
  outcome.index <- 2
  
  ## Check that p.num is valid
  if( is.numeric(p.num) & p.num > nrow(outcome) ) {
    return(NA)
  }
  else if(!is.numeric(p.num) ) {
    if( !any( c("best","worst" ) == p.num) ) { return(NA) }
    else if( p.num == "best" ) { p.num <- 1 }
    else if( p.num == "worst" ) { p.num <- nrow(outcome) }
  }
  
  # Rank outcome
  outcome <- outcome[order(outcome[,outcome.index], outcome[,hospital.index]), ]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  return(outcome[p.num, hospital.index])
}




test.rankhospital <- function() {
  
  rankhospital("TX", "heart failure", 4)
  # [1] "DETAR HOSPITAL NAVARRO"
  
  rankhospital("MD", "heart attack", "worst")
  #[1] "HARFORD MEMORIAL HOSPITAL"

  rankhospital("MN", "heart attack", 5000)
  #[1] NA
}