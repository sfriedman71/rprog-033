rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv"
                   , colClasses = "character"
                   , na.strings = c("Not Available"))
  
  ## Map of outcome params and data columns
  outcome.map <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                   , "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                   , "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )  
  
  ## Check whether outcome if valid
  if( ! outcome %in% names(outcome.map) ) { stop( "invalid outcome" ) }
  
  outcome.name <- outcome.map[[outcome]]
  
  ## Coerce characters into numbers only for the outome we are interested in
  data[,outcome.name] <- as.numeric(data[,outcome.name])
  
  ## For each state, find the hospital of the given rank
  ## The sort order is a requirement for the assignment
  state.list <- sort( unique( data$State ) )
  
  ## capture return data here, later put it in a data frame
  hospital.vector <- character(0)
  
  for( i in seq_along(state.list) ) {
    
    state.name <- state.list[i]
    
    state.data <- subset( data
                          ##, data$State == state.name & !is.na( data[outcome.name] )
                          , data$State == state.name 
                          , c("Hospital.Name", outcome.name)  )
    
    ## Sort state data by outcome values for rank
    ## And by hospital name to break ties
    state.data.sorted <- state.data[order( state.data[outcome.name]
                                           , state.data[["Hospital.Name"]]
                                           ,  decreasing = FALSE
                                           , na.last = NA)
                                    , ]
    
    ## check num param
    ## I should later add logic handle bad values of num (i.e. num<0, num>nrow, bad chars)
    state.num = num
    if( num == "best") { state.num <- 1 }
    if( num == "worst" ) { state.num <- nrow( state.data.sorted ) }
    
    ## Use the num (rank) to pick the row
    ## Use the Hospital Names as the value
    ## Add it to a list
    hospital.vector[i] <- state.data.sorted[state.num,"Hospital.Name"] 
    
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## This is why we needed the two vectors to be in the same order
  data.frame( hospital = hospital.vector
              , state  = state.list
              , row.names = state.list )
}

qc <- function() {
  print("test 1")
  print(head( rankall("heart attack", 20), 10))
  
  print("test 2")
  print(tail(rankall("pneumonia", "worst"), 3))
  
  print("test 3")
  print(tail(rankall("heart failure"), 10))
  
}
