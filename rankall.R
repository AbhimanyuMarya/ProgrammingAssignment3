
rm(list=ls())
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Note that argument «colClasses = "character"» coerces all into charcter class
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  OUTCOMES <- c("heart attack", "heart failure","pneumonia")
  
  if (is.element(outcome, OUTCOMES) == FALSE){
    stop("invalid outcome")
  }

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
  if (outcome=="heart attack"){
    colnum <- 11
  }
  
  if (outcome == "heart failure"){
    colnum <- 17
  }
  
  if (outcome == "pneumonia"){
    colnum <- 23
  }
  
  ## reduce dataframe to needed data two columns Hospital Name and 
  data.na<-outcome_data[,c(2,7,colnum)]
  
  
  ## unique(outcome_data$State) --» vector with all 54 states
  ## create vector with sorted states
  sorted.states<-sort(unique(outcome_data$State))
  
  ## Creates empty dataframe
  dfr <- data.frame(hospital = rep(NA, 54), state = 1:54, row.names = sorted.states)
  
#   Hospitals that do not have data on a particular outcome should be excluded from the set of
#   hospitals when deciding the rankings. 
  
  for (i in sorted.states){
    dfr[i,2] <- i
    
    data.state <- data.na[data.na$State==i,]
    suppressWarnings(data.state[,3]<- as.numeric (data.state[,3]))
    data <- na.omit(data.state)
    
    ## find hospital of rank num in each state
    if (num == "worst"){
      ## sort first by Mortality Rates, then Hospital, place NAs in the end
      sorted<-data[order(data[,3], 
                         data$Hospital.Name, na.last = TRUE, decreasing = TRUE),]
      dfr[i,1] <- sorted[1,1]
    }
    
    else {
      sorted<-data[order(data[,3], 
                         data$Hospital.Name, na.last = TRUE, decreasing = FALSE),]
      
      if (num == "best"){
        dfr[i,1] <- sorted[1,1]
      }
      
      else if (num <= nrow(sorted)){
        dfr[i,1] <- sorted[num,1]
      }
      
      else{
        NA
      }  
    }
  }
  dfr
}
