setwd('~/github/local/CourseraCompData/Class3/')

best <- function(state, outcome) {
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
  tophospital <- as.character(oc$Hospital.Name[oc$measure == min(oc$measure)])
  tophospital <- sort(tophospital, decreasing = FALSE)
  print(tophospital[1])
  ## Return hospital name in that state with lowest 30-day death rate
}