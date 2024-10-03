install.packages("lubridate", "dplyr", "ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv", 
                    na.strings = "#N/A")
weather$dateF <- mdy_hm(weather$Date)

interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval

#set up time intervals in a vector of dates
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}
timeInterval(weather$dateF)

#for loop
#i = interating over different observations
#curly brackets indicate new function/for loop
for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1, 4, 6)
for(i in seqEx){
  print(paste("example", i))
}

#turn example into character string
chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

numEx <- numeric()
for(i in 2:6){
  numEx[i] <- 6*i
}

#in-class prompt 1
#calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) for January of 2022 using a for loop. 
#make a plot of the 15 minute air temperature and the rolling average.
jan22 <- weather %>%
  filter(month == 1 & year == 2022)
mean(jan22$AirTemp[1:8])

roll_avg_temp <- numeric()
for(i in 8:nrow(jan22)){
  roll_avg_temp[i] <- mean(jan22$AirTemp[(i-7):i])
}

jan22$roll_avg_temp <- roll_avg_temp

#in-class prompt 2
#you want to see if the solar radiation measurements experienced any issues with build up or accumulation on the sensor in May and June of 2021. 
#make an assessment with your group.

#in-class prompt 3
#check for any date time issues using the function created in the tutorial. 
#Investigate instances of date time issues. What happens around daylight savings? Are there issues with the time zone assumption?

#Question 1
#ensure that there are no issues with the bird excrement or frozen precipitation
#exclude any precipitation that occurs when the air temperature is below zero
#check that no precipitation measurements are used if the X and Y level observations are > 2 degrees
#indicate how many missing precipitation values are in your data

#Question 2
#create a data flag that warns a user if the battery voltage falls below 8.5 Volts

#Question 3
#create a function that checks for observations that are in unrealistic data ranges in air temperature and solar radiation

#Question 4
#make a plot of winter air temperatures in Jan - Mar of 2021
#check for persistence issues that might indicate snow accumulation on the sensor

#Question 5
#total daily precipitation in March and April of 2021
#use a for loop to exclude (convert to NA) any days that include temperatures less than 35 degrees F on that day or the day prior 
#to ensure that measurements are not likely to be affected by snow accumulation on the sensor
#how many daily observations have precipitation observations (not a NA) in your final data table?