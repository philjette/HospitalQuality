best<-function(state,outcome) {
  outcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #create a vector of valid states
  valid_states<-unique(outcomes[,7])
  #check if state arg is valid
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  #create a vector of valid outcomes
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  field_names<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  best_hosp<-character()
  
  subset_outcomes<-subset(outcomes, State==state)
  
  #coerce to numeric the rate cols
  subset_outcomes[, 11] <- suppressWarnings(as.numeric(subset_outcomes[, 11]))
  subset_outcomes[, 17] <- suppressWarnings(as.numeric(subset_outcomes[, 17]))
  subset_outcomes[, 23] <- suppressWarnings(as.numeric(subset_outcomes[, 23]))
  
  best_rates<-suppressWarnings(as.numeric(apply(subset_outcomes[,field_names],2,min, na.rm=TRUE)))
  
  if (outcome=="heart attack") {
    best_hosp<-subset(subset_outcomes, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==best_rates[1])
  } else if (outcome=="heart failure") {
    best_hosp<-subset(subset_outcomes, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==best_rates[2])
  } else if (outcome=="pneumonia") {
    best_hosp<-subset(subset_outcomes, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==best_rates[3])
  } else {
    stop("invalid outcome")
  }
  #in case of ties we sort by name, then take the first hospital
  sort(best_hosp$Hospital.Name,decreasing = FALSE)
  best_hosp$Hospital.Name[1]
}