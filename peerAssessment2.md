---
title: "Reproducible Research: Peer Assessment 2"
by: "James Chen"
---

## Weather Events causing Public Health and Economic Problems

#### Data source: U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database [Storm data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

1. National Weather Service Storm [Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

2. National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

### Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

This project address following questions:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?

### Data Processing
#### Settings


```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
Sys.setenv(LANG="EN") # Set language to English
library(R.utils)
library(ggplot2)
library(plyr)
require(gridExtra)
setwd("C:/Users/jchen.RESPONSYS/Documents/GitHub/RepData_PeerAssessment2")
```

#### Download and load data

```r
if (!"repdata-data-StormData.csv.bz2" %in% dir("./")) {
    print("Downloading data file")
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "repdata-data-StormData.csv.bz2")
}

storm_0 <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
```

#### What event type cause most fatalities and injuries?
1. Add up counts of FATALITIES and INJURIES by event types
2. Sort the count of events in decendent order
3. List the first 15 of them


```r
harm_by_etype <- aggregate( (FATALITIES + INJURIES) ~ EVTYPE, storm_0, sum)
names(harm_by_etype)[2] = "HarmfulEvent"
rank_by_etype_h <- arrange(harm_by_etype,harm_by_etype$HarmfulEvent,decreasing=T)
```
#### what event type cause the greatest economic consequences?
1. Sum up propertities damages and crop damages
2. Sort the sum of damages in decendent order
3. List the first 15 of them


```r
damage_by_etype <- aggregate( (PROPDMG + CROPDMG) ~ EVTYPE, storm_0, sum)
names(damage_by_etype)[2] = "EcoDmg"
rank_by_etype_d <- arrange(damage_by_etype,damage_by_etype$EcoDmg,decreasing=T)
```

### Results
#### The most harmful event types to population health:

```r
top15_h <- head(rank_by_etype_h,15)
hplot<-qplot(EVTYPE,data=top15_h,weight=HarmfulEvent,geom="bar",binwidth = 1)
hplot<-hplot+ggtitle("Harmful Events by Severe Weather\n Events in the U.S. from 1995 - 2011")
hplot<-hplot+labs(x="Event Type",y="Harmful Event Count")
hplot<-hplot+theme(axis.text.x=element_text(angle=45, size=10, vjust=0.5))
hplot
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
names(top15_h)[1] <-"Event Type"
names(top15_h)[2] <-"Harmful Event Count"
top15_h
```

```
##           Event Type Harmful Event Count
## 1            TORNADO               96979
## 2     EXCESSIVE HEAT                8428
## 3          TSTM WIND                7461
## 4              FLOOD                7259
## 5          LIGHTNING                6046
## 6               HEAT                3037
## 7        FLASH FLOOD                2755
## 8          ICE STORM                2064
## 9  THUNDERSTORM WIND                1621
## 10      WINTER STORM                1527
## 11         HIGH WIND                1385
## 12              HAIL                1376
## 13 HURRICANE/TYPHOON                1339
## 14        HEAVY SNOW                1148
## 15          WILDFIRE                 986
```

#### The event types with the greatest economic consequences:

```r
top15_d <- head(rank_by_etype_d,15)
dplot<-qplot(EVTYPE,data=top15_d,weight=EcoDmg,geom="bar",binwidth = 1)
dplot<-dplot+ggtitle("Economic Damage by Severe Weather\n Events in the U.S. from 1995 - 2011")
dplot<-dplot+labs(x="Event Type",y="Economic Damage (in K$)")
dplot<-dplot+theme(axis.text.x=element_text(angle=45, size=10, vjust=0.5))
dplot
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
names(top15_d)[1] <-"Event Type"
names(top15_d)[2] <-"Economic Damage (K$)"
top15_d
```

```
##            Event Type Economic Damage (K$)
## 1             TORNADO           3312276.68
## 2         FLASH FLOOD           1599325.05
## 3           TSTM WIND           1445168.21
## 4                HAIL           1268289.66
## 5               FLOOD           1067976.36
## 6   THUNDERSTORM WIND            943635.62
## 7           LIGHTNING            606932.39
## 8  THUNDERSTORM WINDS            464978.11
## 9           HIGH WIND            342014.77
## 10       WINTER STORM            134699.58
## 11         HEAVY SNOW            124417.71
## 12           WILDFIRE             88823.54
## 13          ICE STORM             67689.62
## 14        STRONG WIND             64610.71
## 15         HEAVY RAIN             61964.94
```
### Conclusion  
From these data, we found that **tornado** is the most harmful event type to population health, and also have the greatest economic consequences.
