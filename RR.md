---
title: "Reproducible Research"
author: "ri"
date: "30/05/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Title: Exploring the health and economic effects of U.S NOAA Storm Database

## Synopsis:

This synopsis is a part of Reproducible Research Course project.

Weather events like storms, tsunamis, floods can have various impacts on health and economy of a country. A few of them being spread of flues, death, property damage. Hence, preventing such fatalities is of major concern.

In this project, we have analyzed the U.S. NOAA storm database and  the final analysis gives us these results:

- Tornadoes seem to have the highest health impact, in terms of fatalities and injuries whereas,
- Floods damage the properties and crops by a significant amount, hence having the greatest economic impact.


## Data Processing:

### 1. Loading the libraries
```{r}

library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
```

### 2. Loading the data
```{r}
if(!exists("StormData")) {
    StormData <- read.csv(bzfile("repdata_data_StormData.csv.bz2"),header = TRUE)
}
str(StormData)
```

### 3. Examining the data set
```{r}
dim(StormData)

#Extracting variables of interest from the original data set
variables <- c( "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
my.data <- StormData[, variables]

#seeing the last few rows in data set instead of head (because of missing values in earlier years)
tail(my.data)
```

### 4. Checking if there are any missing values in data set
```{r}

#in health variables:
sum(is.na(my.data$FATALITIES))
sum(is.na(my.data$INJURIES))

#in economic variables:
sum(is.na(my.data$PROPDMG))
sum(is.na(my.data$CROPDMG))
sum(is.na(my.data$PROPDMGEXP))
sum(is.na(my.data$CROPDMGEXP))
```

### 5. Transforming the variables extracted
```{r}
sort(table(my.data$EVTYPE), decreasing = TRUE)[1:10]
```
We will group various events based on similarity,
For eg, events containing the word 'wind' will be grouped as one event.
New variable EVENTS is the transform variable of EVTYPE that has 10 various types of events like: FLOOD, HEAT and type OTHER for events in which name the keyword is not found.

```{r}
# create a new variable EVENT to transform variable EVTYPE in groups
my.data$EVENT <- "OTHER"
# group by keyword in EVTYPE
my.data$EVENT[grep("HAIL", my.data$EVTYPE, ignore.case = TRUE)] <- "HAIL"
my.data$EVENT[grep("HEAT", my.data$EVTYPE, ignore.case = TRUE)] <- "HEAT"
my.data$EVENT[grep("FLOOD", my.data$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
my.data$EVENT[grep("WIND", my.data$EVTYPE, ignore.case = TRUE)] <- "WIND"
my.data$EVENT[grep("STORM", my.data$EVTYPE, ignore.case = TRUE)] <- "STORM"
my.data$EVENT[grep("SNOW", my.data$EVTYPE, ignore.case = TRUE)] <- "SNOW"
my.data$EVENT[grep("TORNADO", my.data$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
my.data$EVENT[grep("WINTER", my.data$EVTYPE, ignore.case = TRUE)] <- "WINTER"
my.data$EVENT[grep("RAIN", my.data$EVTYPE, ignore.case = TRUE)] <- "RAIN"
# listing the transformed event types 
sort(table(my.data$EVENT), decreasing = TRUE)
```
Checking the values for variables that represent units od dollars:

```{r}
sort(table(my.data$PROPDMGEXP), decreasing = TRUE)[1:10]
sort(table(my.data$CROPDMGEXP), decreasing = TRUE)[1:10]
```
We will transform the variables in one unit (dollar) variable by the following rule to overcome the problems that are there in these units:
* K or k: a thousand dollar (10^3)
* M or m: a million dollar (10^6)
* B or b: a billion dollar (10^9)
* rest of the units will just be dolar

New variable(s) is (value of damage) x (dollar unit)
```{r}
my.data$PROPDMGEXP <- as.character(my.data$PROPDMGEXP)
my.data$PROPDMGEXP[is.na(my.data$PROPDMGEXP)] <- 0 # NA's considered as dollars
my.data$PROPDMGEXP[!grepl("K|M|B", my.data$PROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
my.data$PROPDMGEXP[grep("K", my.data$PROPDMGEXP, ignore.case = TRUE)] <- "3"
my.data$PROPDMGEXP[grep("M", my.data$PROPDMGEXP, ignore.case = TRUE)] <- "6"
my.data$PROPDMGEXP[grep("B", my.data$PROPDMGEXP, ignore.case = TRUE)] <- "9"
my.data$PROPDMGEXP <- as.numeric(as.character(my.data$PROPDMGEXP))
my.data$property.damage <- my.data$PROPDMG * 10^my.data$PROPDMGEXP

my.data$CROPDMGEXP <- as.character(my.data$CROPDMGEXP)
my.data$CROPDMGEXP[is.na(my.data$CROPDMGEXP)] <- 0 # NA's considered as dollars
my.data$CROPDMGEXP[!grepl("K|M|B", my.data$CROPDMGEXP, ignore.case = TRUE)] <- 0 # everything exept K,M,B is dollar
my.data$CROPDMGEXP[grep("K", my.data$CROPDMGEXP, ignore.case = TRUE)] <- "3"
my.data$CROPDMGEXP[grep("M", my.data$CROPDMGEXP, ignore.case = TRUE)] <- "6"
my.data$CROPDMGEXP[grep("B", my.data$CROPDMGEXP, ignore.case = TRUE)] <- "9"
my.data$CROPDMGEXP <- as.numeric(as.character(my.data$CROPDMGEXP))
my.data$crop.damage <- my.data$CROPDMG * 10^my.data$CROPDMGEXP
```
First 10 values in property damage (in dollars) that appears the most:
```{r}
sort(table(my.data$property.damage), decreasing = TRUE)[1:10]
```
First 10 values in crop damage (in dollars) that appears the most:

```{r}
sort(table(my.data$crop.damage), decreasing = TRUE)[1:10]
```

## Analysis
### 1. Aggregate of events for various public health variables
```{r}
# aggregate FATALITIES and INJURIES by type of EVENT
agg.fatalites.and.injuries <- ddply(my.data, .(EVENT), summarize, Total = sum(FATALITIES + INJURIES,  na.rm = TRUE))
agg.fatalites.and.injuries$type <- "fatalities and injuries"
  
# aggregate FATALITIES by type of EVENT
agg.fatalities <- ddply(my.data, .(EVENT), summarize, Total = sum(FATALITIES, na.rm = TRUE))
agg.fatalities$type <- "fatalities"

# aggregate INJURIES by type of EVENT
agg.injuries <- ddply(my.data, .(EVENT), summarize, Total = sum(INJURIES, na.rm = TRUE))
agg.injuries$type <- "injuries"

# combine all
agg.health <- rbind(agg.fatalities, agg.injuries)

health.by.event <- join (agg.fatalities, agg.injuries, by="EVENT", type="inner")
health.by.event
```
### 2. Aggregating events for economic variables
```{r}
# aggregate PropDamage and CropDamage by type of EVENT
agg.propdmg.and.cropdmg <- ddply(my.data, .(EVENT), summarize, Total = sum(property.damage + crop.damage,  na.rm = TRUE))
agg.propdmg.and.cropdmg$type <- "property and crop damage"

# aggregate PropDamage by type of EVENT
agg.prop <- ddply(my.data, .(EVENT), summarize, Total = sum(property.damage, na.rm = TRUE))
agg.prop$type <- "property"

# aggregate INJURIES by type of EVENT
agg.crop <- ddply(my.data, .(EVENT), summarize, Total = sum(crop.damage, na.rm = TRUE))
agg.crop$type <- "crop"

# combine all
agg.economic <- rbind(agg.prop, agg.crop)


economic.by.event <- join (agg.prop, agg.crop, by="EVENT", type="inner")
economic.by.event
```
## Results
### 1. Across the United States, which types of events are most harmful with respect to population health?
```{r}
# transform EVENT to factor variable for health variables
agg.health$EVENT <- as.factor(agg.health$EVENT)

# plot FATALITIES and INJURIES by EVENT
health.plot <- ggplot(agg.health, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") + scale_fill_manual(values=c("#ffc500", "#c21500")) +
  coord_flip() +
  xlab("Type of event") + 
  ylab("Total number of health fatalities and injuries") +
  ggtitle("Public health impact of various weather events") +
  theme(plot.title = element_text(hjust = 0.5))
print(health.plot)  
[plot 1](https://raw.githubusercontent.com/riddhigarg2605/Reproducible-Research-PGA-2/main/Rplot1.png)
```

As can be seen in the graph, tornado is clearly the most harmful weather event for health which includes parameters like total fatalities and injuries.

### 2. Across the United States, which types of events have the greatest economic consequences?
```{r}
# # transform EVENT to factor variable for economic variables
agg.economic$EVENT <- as.factor(agg.economic$EVENT)

# plot PROPERTY damage and CROP damage by EVENT
economic.plot <- ggplot(agg.economic, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") + scale_fill_manual(values=c("pink", "#C8A2C8")) +
  coord_flip() +
  xlab("Type of event") + 
  ylab("Total damage occured (in dollars)") +
  ggtitle("Property and crop damage occured due to various weather events") +
  theme(plot.title = element_text(hjust = 0.5))  
 
print(economic.plot) 
[plot 2](https://raw.githubusercontent.com/riddhigarg2605/Reproducible-Research-PGA-2/main/Rplot2.png)
```

As can be seen in the graph, floods are clearly the most devastating weather event in terms of economic consequence which includes parameters like damage to crop and property.
