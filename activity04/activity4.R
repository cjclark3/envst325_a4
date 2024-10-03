install.packages("lubridate", "dplyr", "ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv")
weather$dateF <- mdy_hm(weather$Date)

timeInterval <- function(x){}