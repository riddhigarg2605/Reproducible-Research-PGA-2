## Title: Exploring the health and economic effects of U.S NOAA Storm Database

## Synopsis:

This synopsis is a part of Reproducible Research Course project.

Weather events like storms, tsunamis, floods can have various impacts on
health and economy of a country. A few of them being spread of flues,
death, property damage. Hence, preventing such fatalities is of major
concern.

In this project, we have analyzed the U.S. NOAA storm database and the
final analysis gives us these results:

-   Tornadoes seem to have the highest health impact, in terms of
    fatalities and injuries whereas,
-   Floods damage the properties and crops by a significant amount,
    hence having the greatest economic impact.

## Data Processing:

### 1. Loading the libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
```

### 2. Loading the data

``` r
if(!exists("StormData")) {
    StormData <- read.csv(bzfile("repdata_data_StormData.csv.bz2"),header = TRUE)
}
str(StormData)
```

    ## 'data.frame':    902297 obs. of  37 variables:
    ##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
    ##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
    ##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
    ##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
    ##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
    ##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
    ##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
    ##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ BGN_AZI   : chr  "" "" "" "" ...
    ##  $ BGN_LOCATI: chr  "" "" "" "" ...
    ##  $ END_DATE  : chr  "" "" "" "" ...
    ##  $ END_TIME  : chr  "" "" "" "" ...
    ##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
    ##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ END_AZI   : chr  "" "" "" "" ...
    ##  $ END_LOCATI: chr  "" "" "" "" ...
    ##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
    ##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
    ##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
    ##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
    ##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
    ##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
    ##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CROPDMGEXP: chr  "" "" "" "" ...
    ##  $ WFO       : chr  "" "" "" "" ...
    ##  $ STATEOFFIC: chr  "" "" "" "" ...
    ##  $ ZONENAMES : chr  "" "" "" "" ...
    ##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
    ##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
    ##  $ LATITUDE_E: num  3051 0 0 0 0 ...
    ##  $ LONGITUDE_: num  8806 0 0 0 0 ...
    ##  $ REMARKS   : chr  "" "" "" "" ...
    ##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...

### 3. Examining the data set

``` r
dim(StormData)
```

    ## [1] 902297     37

``` r
#Extracting variables of interest from the original data set
variables <- c( "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
my.data <- StormData[, variables]

#seeing the last few rows in data set instead of head (because of missing values in earlier years)
tail(my.data)
```

    ##                EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
    ## 902292 WINTER WEATHER          0        0       0          K       0          K
    ## 902293      HIGH WIND          0        0       0          K       0          K
    ## 902294      HIGH WIND          0        0       0          K       0          K
    ## 902295      HIGH WIND          0        0       0          K       0          K
    ## 902296       BLIZZARD          0        0       0          K       0          K
    ## 902297     HEAVY SNOW          0        0       0          K       0          K

### 4. Checking if there are any missing values in data set

``` r
#in health variables:
sum(is.na(my.data$FATALITIES))
```

    ## [1] 0

``` r
sum(is.na(my.data$INJURIES))
```

    ## [1] 0

``` r
#in economic variables:
sum(is.na(my.data$PROPDMG))
```

    ## [1] 0

``` r
sum(is.na(my.data$CROPDMG))
```

    ## [1] 0

``` r
sum(is.na(my.data$PROPDMGEXP))
```

    ## [1] 0

``` r
sum(is.na(my.data$CROPDMGEXP))
```

    ## [1] 0

### 5. Transforming the variables extracted

``` r
sort(table(my.data$EVTYPE), decreasing = TRUE)[1:10]
```

    ## 
    ##               HAIL          TSTM WIND  THUNDERSTORM WIND            TORNADO 
    ##             288661             219940              82563              60652 
    ##        FLASH FLOOD              FLOOD THUNDERSTORM WINDS          HIGH WIND 
    ##              54277              25326              20843              20212 
    ##          LIGHTNING         HEAVY SNOW 
    ##              15754              15708

We will group various events based on similarity, For eg, events
containing the word ‘wind’ will be grouped as one event. New variable
EVENTS is the transform variable of EVTYPE that has 10 various types of
events like: FLOOD, HEAT and type OTHER for events in which name the
keyword is not found.

``` r
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

    ## 
    ##    HAIL    WIND   STORM   FLOOD TORNADO   OTHER  WINTER    SNOW    RAIN    HEAT 
    ##  289270  255362  113156   82686   60700   48970   19604   17660   12241    2648

Checking the values for variables that represent units od dollars:

``` r
sort(table(my.data$PROPDMGEXP), decreasing = TRUE)[1:10]
```

    ## 
    ##             K      M      0      B      5      1      2      ?      m 
    ## 465934 424665  11330    216     40     28     25     13      8      7

``` r
sort(table(my.data$CROPDMGEXP), decreasing = TRUE)[1:10]
```

    ## 
    ##             K      M      k      0      B      ?      2      m   <NA> 
    ## 618413 281832   1994     21     19      9      7      1      1

We will transform the variables in one unit (dollar) variable by the
following rule to overcome the problems that are there in these units:
\* K or k: a thousand dollar (10^3) \* M or m: a million dollar (10^6)
\* B or b: a billion dollar (10^9) \* rest of the units will just be
dolar

New variable(s) is (value of damage) x (dollar unit)

``` r
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

``` r
sort(table(my.data$property.damage), decreasing = TRUE)[1:10]
```

    ## 
    ##      0   5000  10000   1000   2000  25000  50000   3000  20000  15000 
    ## 663123  31731  21787  17544  17186  17104  13596  10364   9179   8617

First 10 values in crop damage (in dollars) that appears the most:

``` r
sort(table(my.data$crop.damage), decreasing = TRUE)[1:10]
```

    ## 
    ##      0   5000  10000  50000  1e+05   1000   2000  25000  20000  5e+05 
    ## 880198   4097   2349   1984   1233    956    951    830    758    721

## Analysis

### 1. Aggregate of events for various public health variables

``` r
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

    ##      EVENT Total       type Total     type
    ## 1    FLOOD  1524 fatalities  8602 injuries
    ## 2     HAIL    15 fatalities  1371 injuries
    ## 3     HEAT  3138 fatalities  9224 injuries
    ## 4    OTHER  2626 fatalities 12224 injuries
    ## 5     RAIN   114 fatalities   305 injuries
    ## 6     SNOW   164 fatalities  1164 injuries
    ## 7    STORM   416 fatalities  5339 injuries
    ## 8  TORNADO  5661 fatalities 91407 injuries
    ## 9     WIND  1209 fatalities  9001 injuries
    ## 10  WINTER   278 fatalities  1891 injuries

### 2. Aggregating events for economic variables

``` r
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

    ##      EVENT        Total     type       Total type
    ## 1    FLOOD 167502193929 property 12266906100 crop
    ## 2     HAIL  15733043048 property  3046837473 crop
    ## 3     HEAT     20325750 property   904469280 crop
    ## 4    OTHER  97246712337 property 23588880870 crop
    ## 5     RAIN   3270230192 property   919315800 crop
    ## 6     SNOW   1024169752 property   134683100 crop
    ## 7    STORM  66304415393 property  6374474888 crop
    ## 8  TORNADO  58593098029 property   417461520 crop
    ## 9     WIND  10847166618 property  1403719150 crop
    ## 10  WINTER   6777295251 property    47444000 crop

## Results

### 1. Across the United States, which types of events are most harmful with respect to population health?

``` r
# transform EVENT to factor variable for health variables
agg.health$EVENT <- as.factor(agg.health$EVENT)

# plot FATALITIES and INJURIES by EVENT
health.plot <- ggplot(agg.health, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") + scale_fill_manual(values=c("#e4affd", "#d47efc")) +
  coord_flip() +
  xlab("Type of event") + 
  ylab("Total number of health fatalities and injuries") +
  ggtitle("Public health impact of various weather events") +
  theme(plot.title = element_text(hjust = 0.5))
print(health.plot)  
```

![](RR_files/figure-markdown_github/unnamed-chunk-13-1.png)

As can be seen in the graph, tornado is clearly the most harmful weather
event for health which includes parameters like total fatalities and
injuries.

### 2. Across the United States, which types of events have the greatest economic consequences?

``` r
# # transform EVENT to factor variable for economic variables
agg.economic$EVENT <- as.factor(agg.economic$EVENT)

# plot PROPERTY damage and CROP damage by EVENT
economic.plot <- ggplot(agg.economic, aes(x = EVENT, y = Total, fill = type)) + geom_bar(stat = "identity") + scale_fill_manual(values=c("#e4affd", "#d47efc")) +
  coord_flip() +
  xlab("Type of event") + 
  ylab("Total damage occured (in dollars)") +
  ggtitle("Property and crop damage occured due to various weather events") +
  theme(plot.title = element_text(hjust = 0.5))  
 
print(economic.plot) 
```

![](RR_files/figure-markdown_github/unnamed-chunk-14-1.png)

As can be seen in the graph, floods are clearly the most devastating
weather event in terms of economic consequence which includes parameters
like damage to crop and property.
