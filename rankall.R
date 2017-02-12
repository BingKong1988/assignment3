rankall <- function(outcome, num = "best"){
      rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      outcome = tolower(outcome)
      names(rawdata)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
      outcome_valid = names(rawdata) == outcome
      if( sum(outcome_valid) == 0) stop("invalid outcome")
      rawdata[[outcome]] <- as.numeric(rawdata[[outcome]])
      valid_data <- rawdata[!is.na(rawdata[[outcome]]), ] #select data only for one state with valida outcome
      sorted_data <- valid_data[order(valid_data[, outcome], valid_data[, "Hospital.Name"]), ]
      split_sorted_data <- split(sorted_data, sorted_data$State)
      state_name <- names(split_sorted_data)
      if(class(num) == "character"){
            if(tolower(num) == "best") num <- 1
            else if(tolower(num) == "worst"){
                  hospital_name <- sapply(split_sorted_data, function(x) tail(x["Hospital.Name"], 1))
            }
            else stop("invalid num")
      } 
      if(class(num) == "numeric"){
            if(num < 1) stop("invalid num")
            else{
                  split(state_data)
                  thehospital <- state_data[order(state_data[, outcome], state_data[, "Hospital.Name"], na.last = NA), ][["Hospital.Name"]][num]
            }
      }
      thehospital
      
}