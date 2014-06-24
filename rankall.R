rankall <- function(outcome, num = "best") {
  outcomes<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #create a vector of valid states
  valid_states<-unique(outcomes[,7])
  best_hosp <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE) 
  
  #order alphabetically 
  outcomes<-outcomes[order(outcomes[,2],decreasing=FALSE),]
  
  #coerce to numeric the rate cols
outcomes[, 11] <- suppressWarnings(as.numeric(outcomes[, 11]))
outcomes[, 17] <- suppressWarnings(as.numeric(outcomes[, 17]))
outcomes[, 23] <- suppressWarnings(as.numeric(outcomes[, 23]))
  
  #check if outcome arg is valid
  valid_outcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
    
  #loop through each state and get the hospital of specified ranking
  for (i in 1:length(valid_states)){
  subset_outcomes<-outcomes

    if (outcome=="heart attack") {
      #strip hospitals with no NA for the given outcome
      subset_outcomes<-subset(subset_outcomes, State==valid_states[i])
      subset_outcomes<-subset(subset_outcomes,!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      ranked<-cbind(subset_outcomes,rank(subset_outcomes[,11],ties.method= "first"))
      if (num=="best") {
        num<-as.numeric(1)
      } else if (num=="worst") {
        num<-as.numeric(length(subset_outcomes$Hospital.Name))
      }
      colnames(ranked)[47] <- "rank"
      best_hosp[i,]<-c(subset(ranked, rank==num)[1,2],valid_states[i])
    } else if (outcome=="heart failure") {
      #strip hospitals with no NA for the given outcome
      subset_outcomes<-subset(subset_outcomes, State==valid_states[i])
      subset_outcomes<-subset(subset_outcomes,!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      ranked<-cbind(subset_outcomes,rank(subset_outcomes[,17],ties.method= "first"))
      if (num=="best") {
        num<-as.numeric(1)
      } else if (num=="worst") {
        num<-as.numeric(length(subset_outcomes$Hospital.Name))
      }
      colnames(ranked)[47] <- "rank"
      best_hosp[i,]<-c(subset(ranked, rank==num)[1,2],valid_states[i])
    } else {
      #strip hospitals with no NA for the given outcome
      subset_outcomes<-subset(subset_outcomes, State==valid_states[i])
      subset_outcomes<-subset(subset_outcomes,!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      ranked<-cbind(subset_outcomes,rank(subset_outcomes[,23],ties.method= "first"))
      colnames(ranked)[47] <- "rank"
      if (num=="best") {
        best_hosp[i,]<-c(subset(ranked, rank==1)[1,2],valid_states[i])
      } else if (num=="worst") {
        #num<-as.numeric(max(ranked[,47]))
        best_hosp[i,]<-c(subset(ranked, rank==max(ranked[,47]))[1,2],valid_states[i])
      } else {
        best_hosp[i,]<-c(subset(ranked, rank==num)[1,2],valid_states[i])
      }
      
    }
  }
best_hosp<-best_hosp[order(best_hosp[,2],decreasing=FALSE),]
best_hosp
}