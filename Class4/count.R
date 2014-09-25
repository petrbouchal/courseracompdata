setwd('~/github/local/CourseraCompData/Class4/')

count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop('Error: no cause specified')
  }
  ## Check that specific "cause" is allowed; else throw error
  causes <- c("asphyxiation", "blunt force", "other", "shooting",
              "stabbing", "unknown")
  homidf <- data.frame(text = homicides, cause = length(0))
  if (!(cause %in% causes)) {
    stop('Error: cause not allowed')
  }
  ## Read "homicides.txt" data file
  homicides <- readLines('./homicides.txt')
  ## Extract causes of death
  causepattern = paste('Cause: ',cause,sep='')
  homidf$cause <- grepl(causepattern, homidf$text,
                        ignore.case=TRUE)
  ## Return integer containing count of homicides for that cause
  #str(homidf)
  length(homidf$cause[homidf$cause==TRUE])
}