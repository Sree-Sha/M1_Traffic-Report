install.packages("tidyverse")
install.packages("reshape2")

library(ggplot2)
library(naniar)
library(tidyverse)
library(dplyr)
library(reshape2)

options(warn=-1)
View(Traffic)

# Saving the original copy and working on the new data set # Taking backup
traffic <- Traffic
View(traffic)

dim(traffic) # Check the dimensionality of the data
str(traffic) # To find the structure of the data

traffic 
names(traffic)[5]<-'NORTHSPEED'
names(traffic)[6]<-'SOUTHSPEED'
names(traffic)[4]<-'NORTHJUNCTION'
names(traffic)[7]<-'SOUTHJUNCTION'

View(traffic)
summary(traffic)


## DATA CLEANING
missing_data <- colSums(is.na(traffic))
missing_data

# To view the missing data
vis_miss(traffic)
mcar_test(traffic)


ggplot(aes(x = NORTHSPEED), data = traffic) + geom_bar(stat = "count", fill = 'orange') + ggtitle("Northbound Speed")
ggplot(aes(x = SOUTHSPEED), data = traffic) + geom_bar(stat = "count", fill = 'black') + ggtitle("Southbound Speed")

## Box plot for speed versus Days
boxplot(NORTHSPEED~DAY,data=traffic, main="Boxplot for DAY VS Northbound Speed", xlab="DAY", ylab="NorthSpeed" , col=(c("violet", "blue", "green", "yellow", "orange", "red")))
boxplot(SOUTHSPEED~DAY,data=traffic, main="Boxplot for DAY VS Southbound Speed", xlab="DAY", ylab="SouthSpeed" , col=(c("violet", "blue", "green", "yellow", "orange", "red")))



###### Time Samples
plot(traffic$TIME, traffic$NORTHSPEED, main = "Scatter plot Time VS Northbound Speed", xlab = "Time", ylab = "Northbound speed")
boxplot(NORTHSPEED~TIME,data=traffic, main="Boxplot for Time VS Northbound Speed", xlab="Time", ylab="Northbound Speed" , col=(c("violet", "blue", "green", "yellow", "orange", "red")))

plot(traffic$TIME, traffic$SOUTHSPEED, main = "Scatter plot Time VS Southbound Speed", xlab = "Time", ylab = "Southbound Speed")
boxplot(SOUTHSPEED~TIME,data=traffic, main="Boxplot for Time VS Southbound Speed", xlab="Time", ylab="Southbound Speed", col=(c("violet", "blue", "green", "yellow", "orange", "red")))


TenPMTraffic <- subset(traffic, TIME == 22)
TwoAMTraffic <- subset(traffic, TIME == 2)
SixAMTraffic <- subset(traffic, TIME == 6)
TenAMTraffic <- subset(traffic, TIME == 10)
TwoPMTraffic <- subset(traffic, TIME == 14)
SixPMTraffic <- subset(traffic, TIME == 18)

summary(TenPMTraffic)
View(TenPMTraffic)
View(TwoPMTraffic)

boxplot(SOUTHSPEED~DAY,data=TenPMTraffic, main="Boxplot for 10PM Time VS Southbound Speed", xlab="Time:10 PM", ylab="Southbound", col="violet")
boxplot(SOUTHSPEED~DAY,data=TenAMTraffic, main="Boxplot for 10AM Time VS Southbound Speed", xlab="Time:10 AM", ylab="Southbound", col="blue")
boxplot(SOUTHSPEED~DAY,data=TwoPMTraffic, main="Boxplot for 2PM Time VS Southbound Speed", xlab="Time:2 PM", ylab="Southbound", col="green")
boxplot(SOUTHSPEED~DAY,data=TwoAMTraffic, main="Boxplot for 2AM Time VS Southbound Speed", xlab="Time:2 AM", ylab="Southbound", col="yellow")
boxplot(SOUTHSPEED~DAY,data=SixPMTraffic, main="Boxplot for 6PM Time VS Southbound Speed", xlab="Time:6 PM", ylab="Southbound", col="orange")
boxplot(SOUTHSPEED~DAY,data=SixAMTraffic, main="Boxplot for 6AM Time VS Southbound Speed", xlab="Time:6 AM", ylab="Southbound", col="red")


boxplot(SOUTHSPEED~DAY,data=SixAMTraffic)

boxplot(NORTHSPEED~DAY,data=TenPMTraffic, main="Boxplot for 10PM Time VS Northbound Speed", xlab="Time:10 PM", ylab="Northbound", col="violet")
boxplot(NORTHSPEED~DAY,data=TenAMTraffic, main="Boxplot for 10AM Time VS Northbound Speed", xlab="Time:10 AM", ylab="Northbound", col="blue")
boxplot(NORTHSPEED~DAY,data=TwoPMTraffic, main="Boxplot for 2PM Time VS Northbound Speed", xlab="Time:2 PM", ylab="Northbound", col="green")
boxplot(NORTHSPEED~DAY,data=TwoAMTraffic, main="Boxplot for 2AM Time VS Northbound Speed", xlab="Time:2 AM", ylab="Northbound", col="yellow")
boxplot(NORTHSPEED~DAY,data=SixPMTraffic, main="Boxplot for 6PM Time VS Northbound Speed", xlab="Time:6 PM", ylab="Northbound", col="orange")
boxplot(NORTHSPEED~DAY,data=SixAMTraffic, main="Boxplot for 6AM Time VS Northbound Speed", xlab="Time:6 AM", ylab="Northbound", col="red")

# Daywise splitup
MondayTraffic <- subset(traffic, DAY == "Monday")
TuesdayTraffic <- subset(traffic, DAY == "Tuesday")
WednesdayTraffic <- subset(traffic, DAY == "Wednesday")
ThursdayTraffic <- subset(traffic, DAY == "Thursday")
FridayTraffic <- subset(traffic, DAY == "Friday")
SaturdayTraffic <- subset(traffic, DAY == "Saturday")
SundayTraffic <- subset(traffic, DAY == "Sunday")

# to find mean
summary(MondayTraffic)
summary(TuesdayTraffic)
summary(WednesdayTraffic)
summary(ThursdayTraffic)
summary(FridayTraffic)
summary(SaturdayTraffic)
summary(SundayTraffic)

# Data frame for days and average speed
Day <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
Mean_NorthSpeed <- c(64.34, 63.57, 64.96, 64.27, 63.82, 64.79, 65.28)
Mean_SouthSpeed <- c(61.77, 62.97, 62.34, 63.29, 62.50, 64.43, 63.65)
MeanSpeedDay <- data.frame(Day , Mean_NorthSpeed, Mean_SouthSpeed)

MeanSpeedDay
barplot(MeanSpeedDay$Mean_NorthSpeed~MeanSpeedDay$Day, xlab = 'DAY',ylab = 'Mean Speed', ylim = c(60,65), main = 'Daily Average Northbound speed', col = "yellow")
barplot(MeanSpeedDay$Mean_SouthSpeed~MeanSpeedDay$Day, xlab = 'DAY',ylab = 'Mean Speed', ylim = c(60,65), main = 'Daily Average Southbound speed', col = "pink")



lr <- lm(Mean_NorthSpeed.Y ~ Day.X, data = MeanSpeedDay)
