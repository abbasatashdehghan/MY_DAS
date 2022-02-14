library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)
library(lubridate)
library(zoo)
#############################
#Example 1
anderston <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/anderstonpm10.csv"))
str(anderston)
anderston$Date <- as.Date(anderston$Date, "%d/%m/%Y")

anderston %>% ggplot(aes(Date, Glasgow.Anderston)) + 
  geom_line(color = "#41ab5d") + 
  scale_x_date(date_labels = "%d-%m-%y", date_breaks = "2 week") + 
  labs(x = "Date", y = "Particulate matter") + 
  ggtitle("Pollution concentrations in Glasgow")

#Example 2
airtraffic<-read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/airtraffic.csv"))
str(airtraffic)
airtraffic$Date <- as.yearqtr(paste(airtraffic$Year, airtraffic$Quarter),
                              format = "%Y %q")
airtraffic %>% ggplot(aes(x = Date, y = passengers)) + geom_line(color = "#41b6c4") + 
  scale_x_yearqtr(format = "%Y-Q%q") + 
  labs(x = "Date", y = "Passengers", 
       title = "Number of air travellers into the UK per quarter")

#Example 3 (Share price).
