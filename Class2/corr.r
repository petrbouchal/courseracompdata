corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  corrlist <- as.integer(integer())
  yesfiles  <- 0
  for (filenum in 1:332) {
    file <- paste("./", directory ,"/",
                      as.character(sprintf("%03d",as.integer(filenum))),
                      ".csv", sep='')
    d <- read.csv(file)
    d$compl <- complete.cases(d)
    d <- d[d$compl==TRUE,]
    if (nrow(d) > threshold) {
      correl <- cor(d$sulfate, d$nitrate)
      corrlist <- c(corrlist, correl)
      yesfiles =+ 1
    }
  }
  if(yesfiles == 0) {
    integer(length=0)
  } else {
    corrlist
  }
}