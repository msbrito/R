best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  dframe   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(dframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% dframe[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data_of_states <- which(dframe[, "state"] == state)
    hdata_of_states <- dframe[data_of_states, ]    
    values <- as.numeric(hdata_of_states[, eval(outcome)])
    min_val <- min(values, na.rm = TRUE)
    result  <- hdata_of_states[, "hospital"][which(values == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}