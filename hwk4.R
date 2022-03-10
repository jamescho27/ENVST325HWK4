library(dplyr)
library(lubridate)
library(ggplot2)

weather <- na.omit(read.csv("activity04/campus_weather.csv", na.strings = "#N/A"))
meta_data <- read.csv("activity04/meter_weather_metadata.csv", na.strings = "#N/A")
sensor <- read.csv("activity04/Sensor log.csv", na.strings = "#N/A")

weather$datetime <- mdy_hm(weather$Date)
weather$day <- yday(weather$datetime)
weather$year <- year(weather$datetime)

ggplot(data = weather[weather$day >= 121 & weather$day <= 151,], aes(x = datetime, y = SolRad))+
  geom_line()


timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}

timeCheck900(weather$datetime)


#Question 1

weather$Precip = ifelse(weather$AirTemp < 0 | abs(weather$XLevel) > 2 | abs(weather$YLevel) > 2 , NA, weather$Precip)

missing <- sum(is.na(weather$Precip))
missing


#Question 2

weather$LowBatteryFlag <- ifelse(weather$BatVolt < 8500, 1, 0)
max(weather$AirTemp)
min(weather$AirTemp)
max(weather$SolRad)
min(weather$SolRad)


#Question 3


check_for_abnormal <- function(data, temp_low=-30, temp_high = 40, sol_low=0, sol_high=1100){
  print(head(data))
  data %>% filter(AirTemp < temp_low 
              | AirTemp > temp_high 
              | SolRad < sol_low 
              | SolRad > sol_high)
  
}
check_for_abnormal(weather)

#Question 4

weather %>% subset(year == 2021 & day <= 90) %>%
  ggplot(aes(x=datetime, y=AirTemp)) +
  geom_line()
