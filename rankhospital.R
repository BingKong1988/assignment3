rankhospital <- function(state, outcome, num = "best"){
      rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      state = toupper(state)
      outcome = tolower(outcome)
      state_valid = rawdata[["State"]] == state
      if( sum(state_valid) == 0) stop("invalid state")
      names(rawdata)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
      outcome_valid = names(rawdata) == outcome
      if( sum(outcome_valid) == 0) stop("invalid outcome")
      
      rawdata[[outcome]] <- as.numeric(rawdata[[outcome]])
      state_data <- rawdata[state_valid & !is.na(rawdata[[outcome]]), ] #select data only for one state with valida outcome
      if(class(num) == "character"){
            if(tolower(num) == "best") num <- 1
            else if(tolower(num) == "worst") num <- as.numeric(nrow(state_data))
            else stop("invalid num")
      } 
      if(class(num) == "numeric"){
            if(num < 1) stop("invalid num")
            else if(num > nrow(state_data)) thehospital <- NA
            else{
                  thehospital <- state_data[order(state_data[, outcome], state_data[, "Hospital.Name"], na.last = NA), ][["Hospital.Name"]][num]
            }
      }
      thehospital
      
}