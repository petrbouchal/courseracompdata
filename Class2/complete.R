complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  output <- data.frame('id'=as.integer(integer()),
                       'nobs'=as.integer(integer()))
  print(id)
  for (i in id) {
    filename <- paste("./", directory ,"/",
                      as.character(sprintf("%03d",as.integer(i))),
                      ".csv", sep='')
    numobs <- sum(complete.cases(read.csv(filename)))
    print(numobs)
    row <- data.frame('id'=i, 'nobs'=numobs)
    print(row)
    output <- rbind(output, row)
  }
  print(output)
  output
}