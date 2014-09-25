setwd('~/github/local/CourseraCompData/Class4/')

agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {
    stop('')
  }
  ## Read "homicides.txt" data file
  homicides <- readLines('./homicides.txt')
  homidf <- data.frame(text = homicides, cause = length(0))
  ## Extract ages of victims; ignore records where no age is given
  agepattern = paste(age, ' years old',sep='')
  homidf$agematch <- grepl(agepattern, homidf$text,
                        ignore.case=TRUE)  
  ## Return integer containing count of homicides for that age
  length(homidf$agematch[homidf$agematch==TRUE])
}
