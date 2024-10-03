install.packages("lubridate", "dplyr", "ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv")
weather$dateF <- mdy_hm(weather$Date)

interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval

#set up time intervals in a vector of dates
timeInterval <- function(x){x[-length(x)] %--% x[-1]}
timeInterval(weather$dateF)
