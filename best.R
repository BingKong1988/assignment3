best <- function(state, outcome){
      rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      state = toupper(state)
      outcome = tolower(outcome)
      state_valid = rawdata[["State"]] == state
      if( sum(state_valid) == 0) stop("invalid state")
      names(rawdata)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
      outcome_valid = names(rawdata) == outcome
      if( sum(outcome_valid) == 0) stop("invalid outcome")
      state_data <- rawdata[state_valid, ] #select data only for one state
      state_data[[outcome]] <- as.numeric(state_data[[outcome]])
      state_data[order(state_data[, outcome], state_data[, "Hospital.Name"], na.last = NA), ][["Hospital.Name"]][1]
}
