setwd('~/github/local/CourseraCompData/Class3/')

rankall <- function(outcome, rank = "best") {
  options(warn=-1)
  ## Read outcome data
  oco <- read.csv('./hospital/outcome-of-care-measures.csv',
                 colClasses='character')
  statesu <- sort(unique(oco$State))
  
  oc2 <- data.frame(hospital = character(0), state=character(0))
  
  for (state in statesu) {
    oc <- subset(oco, oco$State == state)
 
    if (outcome == 'heart attack') {
      oc$measure <- as.numeric(oc[, 11])
    }
    else if (outcome == 'heart failure') {
      oc$measure <- as.numeric(oc[, 17])
    }
    else if (outcome == 'pneumonia') {
      oc$measure <- as.numeric(oc[, 23])
    }
    else {
      stop('invalid outcome')
    }
    
    oc <- subset(oc, !is.na(oc$measure))
    
    oc$rank <- rank(oc$measure, ties.method='min')
    
    if (rank == "best") {
      nrank = 1
    }
    else if (rank == "worst") {
      nrank <- max(oc$rank)
    }
    else {
      nrank <- rank
    }
    
    hospital <- oc$Hospital.Name[oc$rank == nrank]
    
    #print(state)
    #print(hospital)
    
    hospital <- sort(hospital)
    hospitalf <- hospital[1]
    #print(hospitalf)
    rowf <- data.frame(hospital = as.character(hospitalf),
                       state = as.character(state))
    oc2 <- rbind(oc2, rowf)
  }
    
  oc2
  ## Return hospital name in that state with lowest 30-day death rate
}