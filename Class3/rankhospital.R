setwd('~/github/local/CourseraCompData/Class3/')

rankhospital <- function(state, outcome, rank = "best") {
  options(warn=-1)
  ## Read outcome data
  # hd <- read.csv('./hospital/hospital-data.csv')
  oc <- read.csv('./hospital/outcome-of-care-measures.csv',
                 colClasses='character')
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
  oc <- subset(oc, oc$State == state)
  if (nrow(oc) == 0) {
    stop("invalid state")
  }
  oc$rank <- rank(oc$measure, ties.method='min')
  
  if (rank == "best") {
    nrank = 1
  }
  else if (rank == "worst") {
    nrank = max(oc$rank)
  }
  else {
    nrank = rank
  }
  
  #print(nrank)
  
  rankhospital <- as.character(oc$Hospital.Name[oc$rank == nrank])
  rankhospital <- sort(rankhospital, decreasing = FALSE)
  print(rankhospital[1])
  ## Return hospital name in that state with lowest 30-day death rate
}