rankall <- function(p.outcome, p.num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Read outcome data
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  
  ## Check that state and outcome are valid
  acceptable.p.outcome = c("heart attack","heart failure","pneumonia")
  if (!p.outcome %in% acceptable.p.outcome) { stop("invalid outcome")}
  
  state.list = sort(unique(outcome.all[,"State"]))
  
  ## convert outcome name into column name
  acceptable.v.outcome <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcome.column <- acceptable.v.outcome[match(p.outcome, acceptable.v.outcome)]
  
  ## For each state, find the hospital of the given rank
  hospital.state.list <- character(0)
  
  for (i in seq_along(state.list)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    outcome.state <- outcome.all[outcome.all$State==state.list[i],]
    
    # order data by outcome
    #outcome.state <- outcome.state[order(as.numeric(outcome.state[[outcome.column]]),outcome.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    outcome.state <- outcome.state[order(outcome.state[[outcome.column]], outcome.state[["Hospital.Name"]]), ]
    
    #handle num input
    this.num = p.num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(outcome.state)
    
    hospital.state.list[i] <- outcome.state[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the state name
  data.frame(hospital=hospital.state.list,state=state.list,row.names=state.list)
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