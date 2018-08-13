rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  dframe   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(dframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  dframe[, eval(outcome)] <- as.numeric(dframe[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    for_state <- with(dframe, split(dframe, state))
    ordered  <- list()
    for (i in seq_along(for_state)){
      for_state[[i]] <- for_state[[i]][order(for_state[[i]][, eval(outcome)], for_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(for_state[[i]][num, "hospital"], for_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      for_state <- with(dframe, split(dframe, state))
      ordered  <- list()
      for (i in seq_along(for_state)){
        for_state[[i]] <- for_state[[i]][order(for_state[[i]][, eval(outcome)],for_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(for_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      for_state <- with(dframe, split(dframe, state))
      ordered  <- list()
      for (i in seq_along(for_state)){
        for_state[[i]] <- for_state[[i]][order(for_state[[i]][, eval(outcome)],for_state[[i]][, "hospital"], decreasing = TRUE), ]
        ordered[[i]]  <- c(for_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
        stop('invalid num')
    }
  }
  return(output)
}