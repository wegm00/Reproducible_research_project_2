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


finedata$PROPDMGEXP <- toupper(finedata$PROPDMGEXP)
finedata$CROPDMGEXP <- toupper(finedata$CROPDMGEXP)

finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "")] <- 1
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "?")] <- 1
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "0")] <- 1
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "2")] <- 100
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "K")] <- 1000
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "M")] <- 10^6
finedata$CROPDMGFACTOR[(finedata$CROPDMGEXP == "B")] <- 10^9

finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "")] <- 1
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "-")] <- 1
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "?")] <- 1
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "+")] <- 1
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "0")] <- 1
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "1")] <- 10
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "2")] <- 100
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "3")] <- 1000
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "4")] <- 10^4
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "5")] <- 10^5
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "6")] <- 10^6
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "7")] <- 10^7
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "8")] <- 10^8
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "H")] <- 100
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "K")] <- 1000
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "M")] <- 10^6
finedata$PROPDMGFACTOR[(finedata$PROPDMGEXP == "B")] <- 10^9

finedata<- mutate(finedata, HEALTHIMP = FATALITIES + INJURIES)
finedata<- mutate(finedata, ECONOMICCOST = PROPDMG * PROPDMGFACTOR + CROPDMG * CROPDMGFACTOR)


healthImpact <- with(finedata, aggregate(HEALTHIMP ~ EVTYPE, FUN = sum))
subset(healthImpact, HEALTHIMP > quantile(HEALTHIMP, prob = 0.95))

finedata$EVTYPE[(finedata$EVTYPE == "THUNDERSTORM WINDS")] <- "THUNDERSTORM WIND"
finedata$EVTYPE[(finedata$EVTYPE == "TSTM WIND")] <- "THUNDERSTORM WIND"
finedata$EVTYPE[(finedata$EVTYPE == "HURRICANE/TYPHOON")] <- "HURRICANE (TYPHOON)"
finedata$EVTYPE[(finedata$EVTYPE == "RIP CURRENTS")] <- "RIP CURRENT"
finedata$EVTYPE[(finedata$EVTYPE == "WILD/FOREST FIRE")] <- "WILDFIRE"


