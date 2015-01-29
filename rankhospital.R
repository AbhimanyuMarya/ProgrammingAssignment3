
rm(list=ls())
rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
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
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
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
    data.na<-outcome_data[outcome_data$State==state,c(2,colnum)]
    
    #Motality rates are a character data, to order numerically must coerce, use supressWarning to avoid warning
    #when coering not the numbers but the "Not avaliable" entries
    suppressWarnings(data.na[,2]<- as.numeric (data.na[,2]))
    
    data <- na.omit(data.na)
   
    if (num == "worst"){
      ## sort first by Mortality Rates, then Hospital, place NAs in the end
      sorted<-data[order(data[,2], 
                         data$Hospital.Name, na.last = TRUE, decreasing = TRUE),]
      sorted[1,1]
    }
    
    else {
      sorted<-data[order(data[,2], 
                         data$Hospital.Name, na.last = TRUE, decreasing = FALSE),]
      
          if (num == "best"){
            sorted[1,1]
          }
      
          else if (num <= nrow(sorted)){
            sorted[num,1]
          }
          
          else{
            NA
          }  
  }
  
}
