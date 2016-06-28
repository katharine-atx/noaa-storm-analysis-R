---
title: "NOAA Storm Event Impact by Type"
author: "Katharine Rovinsky"
date: "June 26, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Synopsis:**

This analysis provides a high-level summary of the health impact and economic impact of storm events in the United States using the National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage from 1950 through November 2011.

Results show that.........

For additional data notes, refer to the NOAA documentation:
-[Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
-[Data FAQs](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

**Data Processing:**

R session information:
```{r}
sessionInfo()
```

Downloading the NOAA storm database...
```{r}
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
connection = tempfile()
download.file(url, connection)
```

Reading the .csv database...
We'll be looking at health impact and economic impact by storm event type. So let's treat event type as a category.
```{r}
stormData = read.csv(connection, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
stormData$Event = as.factor(stormData$EVTYPE)
unlink(connection)
```

Preview database contents...
```{r}
str(stormData)
```

We'll look at our impact metrics by decade (50s, 60s, 70s, 80s, 90s, 2000-2011). Creating a "decade" field from the date...
```{r}
stormData$year = as.numeric(format(as.Date(stormData$BGN_DATE, "%m/%d/%Y "), "%Y"))
stormData$decade = as.character(stormData$year-(stormData$year %% 10))
stormData$decade = paste(stormData$decade,"'s", sep = "")
stormData$decade[stormData$decade=="2010's"|stormData$decade=="2000's"] = "2000-2011"
stormData$decade = as.factor(stormData$decade)
```                                 

Count of NOAA storm event records by decade:
```{r}
summary(stormData$decade)
```

From the data preview, we can see that there are over 900 types of events. Here are the top 5 event types for casualties (all decades), found by summing and ranking the count of injuries and fatalities across event types.
```{r}
fatalities = with(stormData, tapply(FATALITIES, Event, sum))
injuries = with(stormData, tapply(INJURIES, Event, sum))
casualties = fatalities + injuries
casualtiesDF <- data.frame(Event = names(casualties), casualties = casualties)
healthTop5 = names(head(sort(casualtiesDF$casualties, decreasing = TRUE)))
healthTop5
```

Similarly, here are the top 5 most destructive event types for economic impact (all decades), found by summing the estimated financial damages for property and crops, converting the "k", "m" and "b" indicators to relative dollar units.
```{r}
stormData$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
stormData$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
property = stormData[(stormData$PROPDMGEXP %in% c("k", "m", "b")),]
gsub("k", 1, property$PROPDMGEXP)
gsub("m", 1000, property$PROPDMGEXP)
gsub("b", 1000000, property$PROPDMGEXP)
crops = stormData[(stormData$CROPDMGEXP %in% c("k", "m", "b")),]
gsub("k", 1, crops$CROPDMGEXP)
gsub("m", 1000, crops$CROPDMGEXP)
gsub("b", 1000000, crops$CROPDMGEXP)
property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
cropDamage = with(crops, tapply(damage, Event, sum))
propDamage = with(property, tapply(damage, Event, sum))
damage = cropDamage + propDamage
damageDF <- data.frame(Event = names(damage), damage = damage)
econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE)))
econTop5
```

We're now ready to look at the most destructive event types in terms of health impact and economic impact over the past 60 years.

**Results:**

Summarizing our health related metrics by decade:
### how to add decade????

```{r}
# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
  xlab="Number of Gears", col=c("darkblue","red"),
  legend = rownames(counts)) 
```


Summarizing our economic impact metrics by category:
#### how to add decade????
```{r}
c = tapply(crops$damage[crops$Event %in% econTop5], Event, sum)
p = tapply(property$damage[property$Event %in% econTop5], Event, sum)
par(mfrow(1,2))
hist(c)
hist(p)
```





