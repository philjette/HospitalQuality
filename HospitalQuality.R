best<-function(state,outcome) {
  outcomes<-read.csv("outcome-of-care-measures.csv")
  #create a vector of valid states
  states<-unique(outcomes[,7])
  #create a vector of valid outcomes
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  
  subset_outcomes<-subset(outcomes, State==state)
  
  if (outcome=outcomes[1]) {
  ranking<-subset(subsetoutcomes, !is.na(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))  
  } else if (outcome=outcomes[2]) {
    
  } else {
    
  }
  
}