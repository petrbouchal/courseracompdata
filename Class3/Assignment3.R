setwd('./Class3/hospital/')

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
#str(outcome)
colnames(outcome[, 1:23])
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

par(mfrow = c(3, 1))
hist(outcome[, 11], xlab="30-day Death Rate", main="Heart Attack", prob=T,
   xlim=c(5,20), axes=F)
axis(side=1, at=c(10,15,20))
axis(side=2)
lines(abline(v=median(outcome[, 11], na.rm=TRUE)))
hist(outcome[, 17], xlab="30-day Death Rate", main="Heart Failure", prob=T,
   xlim=c(5,20), axes=F)
lines(abline(v=median(outcome[, 17], na.rm=TRUE)))
axis(side=1, at=c(10,15,20))
axis(side=2)
hist(outcome[, 23], xlab="30-day Death Rate", main="Pneumonia", prob=T,
   xlim=c(5,20), axes=F)
lines(abline(v=median(outcome[, 23], na.rm=TRUE)))
lines(density(outcome[, 23], na.rm=T))
axis(side=1, at=c(10,15,20))
axis(side=2)

library(plyr)
outcome2 <- ddply(outcome, .(State), summarize,
                  medianHA30ddeath=median(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                          na.rm=T))
outcome3 <- ddply(outcome, .(State), nrow)

outcome2 <- merge(outcome2, outcome3)
outcome2 <- merge(outcome, outcome2)

outcome2$hosp_count <- outcome2$V1
outcome2$V1 <- NULL

attach(outcome2)
outcome2 <- subset(outcome2, outcome2$hosp_count >= 21)
outcome2 <- outcome2[order(medianHA30ddeath), ]
death <- outcome2[, 11]
state <- outcome2$State
par(mfrow=c(1,1), cex.axis=.6, las=2)
boxplot(death ~ state, ylab="30-day Death Rate")