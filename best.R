# rm(list=ls())

# The best function reads the outcome-of-care-measures.csv file and 
# returns a character vector with the name of the hospital that has the best (i.e. lowest) 
# 30-day mortality for the specified outcome in that state. 

# The hospital name is the name provided in the Hospital.Name variable. 

# The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 

# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
# and "f" are tied for best, then hospital "b" should be returned).

rm(list=ls())
best <- function(state, outcome) {
  ## Read outcome data
  ## Note that argument «colClasses = "character"» coerces all into charcter class
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
   if(all(state != outcome_data$State)){ 
     stop("invalid state")
     #print("This is not a correct State designation. 
     #Use 2-letter designation in upper-case, within quotation marks.") 
   } 
  
  OUTCOMES <- c("heart attack", "heart failure","pneumonia")
  
  if (is.element(outcome, OUTCOMES) == FALSE){
    stop("invalid outcome")
   # print ("This is not an acceptable outcome. Choose 'heart attack', 
   #'heart failure' or'pneumonia' ")  
  }
    
  
  if (outcome=="heart attack"){
    
    ## Return hospital name in that state with lowest 
    #  $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack [,11]
    
    ## reduce dataframe to needed data two columns Hospital Name and 
    data<-outcome_data[outcome_data$State==state,c(2,11)]
    
    #Motality rates are a character data, to order numerically must coerce, use supressWarning to avoid warning
    #when coering not the numbers but the "Not avaliable" entries
    suppressWarnings(data[,2]<- as.numeric (data[,2]))
    
    ## sort first by Mortality Rates, then Hospital, place NAs in the end
    sorted<-data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data$Hospital.Name, na.last = TRUE),]
    
    #print first Hospital Name
    sorted[1,1]
  }
  
  else if (outcome=="heart failure"){
   
    ## Return hospital name in that state with lowest 30-day death rate
    # $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure [,17]
  
    ## reduce dataframe to needed data, two columns Hospital Name and Mortality..Rates.from.Heart.Failure [,17]
    data<-outcome_data[outcome_data$State==state,c(2,17)]
    
    #Motality rates are a character data, to order numerically must coerce, use supressWarning to avoid warning
    #when coering not the numbers but the "Not avaliable" entries
    suppressWarnings(data[,2]<- as.numeric (data[,2]))
    
    #file.state.sub[,3]<- as.numeric file.state.sub[,3])
    sorted<-data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.last = TRUE),]
    
    ## sort first by Mortality Rates, then Hospital, place NAs in the end
#     sorted<-data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data$Hospital.Name, na.last = TRUE),]
    
    #print first Hospital Name, the fist row of first column
    sorted[1,1]
  }
  
  else {
    
    ## Return hospital name in that state with lowest 30-day death rate
    # $Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia [,23]
    
    data<-outcome_data[outcome_data$State==state,c(2,23)]
    
    #Motality rates are a character data, to order numerically must coerce, use supressWarning to avoid warning
    #when coering not the numbers but the "Not avaliable" entries
    suppressWarnings(data[,2]<- as.numeric (data[,2]))
    
    ## sort first by Mortality Rates, then Hospital, place NAs in the end
    sorted<-data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data$Hospital.Name, na.last = TRUE),]
    
    #print first Hospital Name, the fist row of first column
    sorted[1,1]
    
  }
  
}

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message "invalid outcome".
# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"                    OK
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"                 
# > best("MD", "heart attack") 
# [1] "JOHNS HOPKINS HOSPITAL, THE"                         OK
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"          
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
