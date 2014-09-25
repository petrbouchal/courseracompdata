setwd("~/github/local/CourseraCompData/")
data <- read.csv("./Class1/hw1_data.csv")
str(data)
data[1:2,]
data[152:153,]
data$Ozone[47]
sum(is.na(data$Ozone))
mean(data$Ozone[!is.na(data$Ozone)])
data$noSolar <- !is.na(data$Solar.R)
mean(data$Solar.R[data$Temp>90 & data$Ozone>31 & data$noSolar==FALSE])
mean(data$Temp[data$Month==6])