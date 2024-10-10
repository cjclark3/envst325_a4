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

#create month and year column
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

#calculate rolling avg
jan22 <- weather %>%
  filter(month == 1 & year == 2022)
mean(jan22$AirTemp[1:8])

roll_avg_temp <- numeric()
for(i in 8:nrow(jan22)){
  roll_avg_temp[i] <- mean(jan22$AirTemp[(i-7):i])
}

jan22$roll_avg_temp <- roll_avg_temp

#plot rolling avg
ggplot(jan22, aes(x = dateF, y = roll_avg_temp)) +
  geom_line(na.rm = T) +
  labs(x = "Date", y = "Rolling Average Temperature (C)") +
  theme_classic()

#in-class prompt 2
#you want to see if the solar radiation measurements experienced any issues with 
#build up or accumulation on the sensor in May and June of 2021. 
mj21 <- weather %>%
  filter(month %in% c(5,6) & year == 2021)

ggplot(mj21, aes(x = dateF, y = SolRad)) +
  geom_line() +
  labs(x = "Date", y = "Solar Radiation (W/m^2)") +
  theme_classic()

#in-class prompt 3
#check for any date time issues using the function created in the tutorial. 
#Investigate instances of date time issues. What happens around daylight savings? Are there issues with the time zone assumption?

#Question 1
#ensure that there are no issues with the bird excrement or frozen precipitation
#exclude any precipitation that occurs when the air temperature is below zero
#check that no precipitation measurements are used if the X and Y level observations are > 2 degrees
#indicate how many missing precipitation values are in your data
weather$precip.filter <- ifelse((weather$AirTemp < 0) |
                                  abs(weather$XLevel) > 2 | 
                                   abs(weather$YLevel) > 2, NA, weather$Precip)
sum(as.integer(is.na(weather$precip.filter)))

#Question 2
#create a data flag that warns a user if the battery voltage falls below 8.5 Volts
weather$batflag <- ifelse(weather$BatVolt <= 8.5, "Below 8.5", "Above 8.5")

#Question 3
#create a function that checks for observations that are in unrealistic data ranges 
#in air temperature and solar radiation
unrealisticObs() <- function(x){
  mean_x <- mean(x, na.rm = T)
  sd_x <- sd(x, na.rm = T)
  z_score <- (x - mean_x) / sd_x
  outliers <- ifelse(abs(z_score) > 2 , 1, 0)
}

weather$airtempOutliers = unrealisticObs(weather$AirTemp)
weather$solradOutliers = unrealisticObs(weather$SolRad)

#Question 4
#make a plot of winter air temperatures in Jan - Mar of 2021
#check for persistence issues that might indicate snow accumulation on the sensor
janmar21 <- weather %>%
  filter(month %in% c(1,2,3) & year == 2021)

ggplot(janmar21, aes(x = dateF, y = AirTemp)) +
  geom_line() +
  labs(x = "Date", y = "Air Temperature (C)") +
  theme_classic()

#Question 5
#total daily precipitation in March and April of 2021
#use a for loop to exclude (convert to NA) any days that include temperatures 
#less than 35 degrees F on that day or the day prior 
#to ensure that measurements are not likely to be affected by snow accumulation on the sensor
#how many daily observations have precipitation observations (not a NA) in your final data table?
ma21 <- weather %>%
  filter(month %in% c(3, 4) & year == 2021)

ma21$airtempF <- ma21$AirTemp * (9/5) + 32

for(i in 2:length(ma21$airtempF)){
  ma21$Precip[i]<- ifelse(ma21$airtempF[i] < 35 | ma21$airtempF[i-1] < 35,
                          NA, ma21$Precip[i])
}

sum(as.integer(!is.na(ma21$Precip)))
