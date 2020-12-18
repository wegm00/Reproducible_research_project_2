#Load Principle libraries
library(ggplot2)
library(dplyr)
library(R.utils)
library(lubridate)

#Download data
# Checking if folder exists
if (!file.exists("StormData.csv")) { 
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")
bunzip2("StormData.csv.bz2", "StormData.csv")
}

rawinfo <- read.csv("StormData.csv")

str(rawinfo)

finedata<- select(rawinfo, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, FATALITIES, INJURIES)

finedata$BGN_DATE <- as.Date(finedata$BGN_DATE, "%m/%d/%Y")
finedata$YEAR <- year(finedata$BGN_DATE)

# Tornado 1950 - 1954
# Tornado, Thunderstorm Wind, Hail 1955 - 1995
# 48 Events since 1996
# Only use events since 1996
finedata <- filter(finedata, YEAR >= 1996)

# Only use events with either health impact or economic damage
finedata <- filter(finedata, PROPDMG > 0 | CROPDMG > 0 | FATALITIES > 0 | INJURIES > 0)

