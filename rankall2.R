get.num <- function(outcome, p.num) {
  if( is.numeric(p.num) & p.num > nrow(outcome) ) {
    return(NA)
  }
  else if(!is.numeric(p.num) ) {
    if( !any( c("best","worst" ) == p.num) ) { return(NA) }
    else if( p.num == "best" ) { p.num <- 1 }
    else if( p.num == "worst" ) { p.num <- nrow(outcome) }
  }
  p.num
}


rankall <- function(p.outcome, p.num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Read outcome data
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that outcome is valid
  acceptable.p.outcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  if( ! any( names(acceptable.p.outcome) == p.outcome)) {
    stop("invalid outcome")  
  }
  
  ## Index and index outcome data
  outcome.index <- acceptable.p.outcome[p.outcome]
  
  ## Coerce characters into numbers
  ## Does this work?
  # outcome[,outcome.index] <- as.numeric(outcome[,outcome.index])
  
  hospital.index <- 2
  state.index <- 7
  
  ## declare return object
  rank.final <- data.frame(
    hospital=character(),
    state=character()
  )
  
  ## for each state
  state.list <- unique( outcome[state.index] )

  for( i in 1:length(state.list) ) {

    st.name <- state.list[i]
    
    outcome.state <- subset(outcome
                     , st.name %in% outcome[state.index] & !is.na(outcome[,outcome.index]) 
    )
    
    ## Check that p.num is valid
    p.num <- get.num(outcome.state, p.num)
    
    # Rank outcome
    outcome.state <- outcome.state[order(outcome.state[,outcome.index], outcome.state[,hospital.index]), ]
    
    str(outcome.state)
    stop()
    #outcome.state
    
    h <- outcome.state[p.num, hospital.index]
    print(h)
    #str(s)
    
    
    #str(outcome.state)
    #c(outcome[p.num, hospital.index], s)
    
    #rank.final <- rbind(rank.final, c(outcome.state[p.num, s]) )
      
  }
  
  
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  #return(outcome[p.num, hospital.index])
}


qc <- function() {
head(rankall("heart attack", 20), 10)

  #hospital state
  #AK <NA> AK
  #AL D W MCMILLAN MEMORIAL HOSPITAL AL
  #AR ARKANSAS METHODIST MEDICAL CENTER AR
  #AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
  #CA SHERMAN OAKS HOSPITAL CA
  #CO SKY RIDGE MEDICAL CENTER CO
  #CT MIDSTATE MEDICAL CENTER CT
  #DC <NA> DC
  #DE <NA> DE
  #FL SOUTH FLORIDA BAPTIST HOSPITAL FL

  tail(rankall("pneumonia", "worst"), 3)
  #hospital state
  #WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
  #WV PLATEAU MEDICAL CENTER WV
  #WY NORTH BIG HORN HOSPITAL DISTRICT WY

  tail(rankall("heart failure"), 10)
  #hospital state
  #TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
  #TX FORT DUNCAN MEDICAL CENTER TX
  #UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
  #VA SENTARA POTOMAC HOSPITAL VA
  #VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
  #VT SPRINGFIELD HOSPITAL VT
  #WA HARBORVIEW MEDICAL CENTER WA
  #WI AURORA ST LUKES MEDICAL CENTER WI
  #WV FAIRMONT GENERAL HOSPITAL WV
  #WY CHEYENNE VA MEDICAL CENTER WY

}