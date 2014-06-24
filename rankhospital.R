rankhospital <- function(state, outcome, num = "best") {
  outcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #create a vector of valid states
  valid_states<-unique(outcomes[,7])
  #check if state arg is valid
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  #create a vector of valid outcomes
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  
  #identify field names we care about
  field_names<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  best_hosp<-character()
  
  subset_outcomes<-subset(outcomes, State==state)
  #get the subset ordered alpha
  subset_outcomes<-subset_outcomes[order(subset_outcomes[,2],decreasing=FALSE),]
  
  #coerce to numeric the rate cols
  subset_outcomes[, 11] <- suppressWarnings(as.numeric(subset_outcomes[, 11]))
  subset_outcomes[, 17] <- suppressWarnings(as.numeric(subset_outcomes[, 17]))
  subset_outcomes[, 23] <- suppressWarnings(as.numeric(subset_outcomes[, 23]))
    
  if (outcome=="heart attack") {
    #strip hospitals with no NA for the given outcome
    subset_outcomes<-subset(subset_outcomes, !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    ranked<-cbind(subset_outcomes,rank(subset_outcomes[,11],ties.method= "first"))
    if (num=="best") {
      num<-as.numeric(1)
    } else if (num=="worst") {
      num<-as.numeric(length(subset_outcomes$Hospital.Name))
    }
    colnames(ranked)[47] <- "rank"
    best_hosp<-subset(ranked, rank==num)
  } else if (outcome=="heart failure") {
    subset_outcomes<-subset(subset_outcomes, !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    ranked<-cbind(subset_outcomes,rank(subset_outcomes[,17],ties.method= "first"))
    if (num=="best") {
      num<-as.numeric(1)
    } else if (num=="worst") {
      num<-as.numeric(length(subset_outcomes$Hospital.Name))
    }
    colnames(ranked)[47] <- "rank"
    best_hosp<-subset(ranked, rank==num)
  } else if (outcome=="pneumonia") {
    subset_outcomes<-subset(subset_outcomes, !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    ranked<-cbind(subset_outcomes,rank(subset_outcomes[,23],ties.method= "first"))
    if (num=="best") {
      num<-as.numeric(1)
    } else if (num=="worst") {
      num<-as.numeric(length(subset_outcomes$Hospital.Name))
    }
    colnames(ranked)[47] <- "rank"
    best_hosp<-subset(ranked, rank==num)
  } else {
    stop("invalid outcome")
  }
  #in case of num is greater than number of hospitals, return NA
  if (num>length(subset_outcomes$Hospital.Name)) {
    best_hosp<-NA
  } else {
    best_hosp<-best_hosp$Hospital.Name
  }
  best_hosp
}