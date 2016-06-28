**Synopsis:**

This analysis provides a high-level summary of the health impact and economic impact of storm events in the United States using the National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage from 1950 through November 2011.

Results show that.........

For additional data notes, refer to the NOAA database documentation:
- [Database documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
- [Database FAQs](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

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
stormData$Event = as.factor(tolower(stormData$EVTYPE))
unlink(connection)
```

Preview database contents...
```{r}
str(stormData)
```

We have 985 types of events. Let's look at our impact metrics by decade (50s, 60s, 70s, 80s, 90s, 2000-2011) and understand the most destructive event types in terms of health impact and in terms of economic impact.
```{r}
stormData$year = as.numeric(year, from date)
stormData$decade = as.character(round(year, down))
stormData$decade = as.factor(concatenate(stormData$decade,"'s"))
stormData$decade[with stormData(decade=="2010's" OR decade=="2000's"] = "2000-2011"
```                                 

Summarizing our health related metrics by category:
```{r}
library(ggplot2)
sorted (sum of deaths, sum of injuries, by 2000-2011 column)
take top 5
```

Summarizing our economic impact metrics by category:
```{r}
library(ggplot2)
sorted (sum of property damage, sum of crop damage, by 2000-2011 column)
take top 5
```

Results:

Summarizing our health related metrics by category:
```{r}
bar chart across decades injuries v. fatalities for each of the 5
```

Summarizing our economic impact metrics by category:
```{r}
bar chart across decades injuries v. fatalities for each of the 5
```

stormData$BGN_DATE = as.Date(stormData$BGN_DATE)
stormData$year = year(stormData$BGN_DATE)> stormData = read.csv(connection, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'C:\Users\KatyRovi\AppData\Local\Temp\Rtmp6TeavQ\file170051f43ab0': No such file or directory
> stormData$Event = as.factor(tolower(stormData$EVTYPE))
> unlink(connection)
> url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
> connection = tempfile()
> download.file(url, connection)
trying URL 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
Content type 'application/bzip2' length 49177144 bytes (46.9 MB)
downloaded 46.9 MB

> stormData = read.csv(connection, header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
> stormData$Event = as.factor(tolower(stormData$EVTYPE))
> unlink(connection)
> str(stormData)

> stormData$year = as.numeric(format(as.Date(stormData$BGN_DATE, "%m/%d/%Y "), "%Y"))
> stormData$decade = as.character(stormData$year-(stormData$year %% 10))
> stormData$decade = paste(stormData$decade,"'s", sep = "")
> stormData$decade[stormData$decade=="2010's"|stormData$decade=="2000's"] = "2000-2011"
> stormData$decade = as.factor(stormData$decade)

> summary(stormData$decade)

   1950's    1960's    1970's    1980's    1990's 2000-2011 
    11191     25065     39110     75191    228577    523163 
	
> stormData$casualties = with(stormData,FATALITIES + INJURIES)
> casualties = with(stormData, tapply(casualties, Event, sum))
> casualtiesDF <- data.frame(Event = names(casualties), casualties = casualties)
> healthTop5 = names(head(sort(casualtiesDF$casualties, decreasing = TRUE), n=5))
> healthTop5

[1] "tornado"        "excessive heat" "tstm wind"      "flood"          "lightning"  
   
> stormData$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
> stormData$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
> property = stormData[(stormData$PROPDMGEXP %in% c("k", "m", "b")),]
> property$PROPDMGEXP = sub("k", 1, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("m", 1000, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("b", 1000000, property$PROPDMGEXP)
> crops = stormData[(stormData$CROPDMGEXP %in% c("k", "m", "b")),]
> crops$CROPDMGEXP = sub("k", 1, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("m", 1000, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("b", 1000000, crops$CROPDMGEXP)
> property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
> crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
> cropDamage = with(crops, tapply(damage, Event, sum))
> propDamage = with(property, tapply(damage, Event, sum))
> damage = cropDamage + propDamage
> damageDF <- data.frame(Event = names(damage), damage = damage)
> econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE), n=5))
> econTop5

[1] "flood"             "hurricane/typhoon" "tornado"           "storm surge"      
[5] "hail"    
         
> stormData2000 = stormData[stormData$decade == "2000-2011"]
Error in `[.data.frame`(stormData, stormData$decade == "2000-2011") : 
  undefined columns selected
> stormData$casualties = with(stormData2000, FATALITIES + INJURIES)
Error in with(stormData2000, FATALITIES + INJURIES) : 
  object 'stormData2000' not found
> casualties = with(stormData, tapply(casualties, Event, sum))
> casualtiesDF <- data.frame(Event = names(casualties), casualties = casualties)
> healthTop5 = names(head(sort(casualtiesDF$casualties, decreasing = TRUE), n=5))
> healthTop5
[1] "tornado"        "excessive heat" "tstm wind"      "flood"          "lightning"     
> stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
Error in stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP) : 
  object 'stormData2000' not found
> stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
Error in stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP) : 
  object 'stormData2000' not found
> property = stormData2000[(stormData2000$PROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> property$PROPDMGEXP = sub("k", 1, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("m", 1000, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("b", 1000000, property$PROPDMGEXP)
> crops = stormData2000[(stormData2000$CROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> crops$CROPDMGEXP = sub("k", 1, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("m", 1000, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("b", 1000000, crops$CROPDMGEXP)
> property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
> crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
> cropDamage = with(crops, tapply(damage, Event, sum))
> propDamage = with(property, tapply(damage, Event, sum))
> damage = cropDamage + propDamage
> damageDF <- data.frame(Event = names(damage), damage = damage)
> econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE), n=5))
> econTop5
[1] "flood"             "hurricane/typhoon" "tornado"           "storm surge"      
[5] "hail"             
> tormData2000 = stormData[stormData$decade == "2000-2011"]
Error in `[.data.frame`(stormData, stormData$decade == "2000-2011") : 
  undefined columns selected
> stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
Error in stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP) : 
  object 'stormData2000' not found
> stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
Error in stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP) : 
  object 'stormData2000' not found
> property = stormData2000[(stormData2000$PROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> property$PROPDMGEXP = sub("k", 1, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("m", 1000, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("b", 1000000, property$PROPDMGEXP)
> crops = stormData2000[(stormData2000$CROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> crops$CROPDMGEXP = sub("k", 1, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("m", 1000, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("b", 1000000, crops$CROPDMGEXP)
> property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
> crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
> cropDamage = with(crops, tapply(damage, Event, sum))
> propDamage = with(property, tapply(damage, Event, sum))
> damage = cropDamage + propDamage
> damageDF <- data.frame(Event = names(damage), damage = damage)
> econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE), n=5))
> econTop5
[1] "flood"             "hurricane/typhoon" "tornado"           "storm surge"      
[5] "hail"             
> stormData2000 = stormData[stormData$decade == "2000-2011"]
Error in `[.data.frame`(stormData, stormData$decade == "2000-2011") : 
  undefined columns selected
> stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
Error in stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP) : 
  object 'stormData2000' not found
> stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
Error in stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP) : 
  object 'stormData2000' not found
> property = stormData2000[(stormData2000$PROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> property$PROPDMGEXP = sub("k", 1, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("m", 1000, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("b", 1000000, property$PROPDMGEXP)
> crops = stormData2000[(stormData2000$CROPDMGEXP %in% c("k", "m", "b")),]
Error: object 'stormData2000' not found
> crops$CROPDMGEXP = sub("k", 1, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("m", 1000, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("b", 1000000, crops$CROPDMGEXP)
> property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
> crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
> cropDamage = with(crops, tapply(damage, Event, sum))
> propDamage = with(property, tapply(damage, Event, sum))
> damage = cropDamage + propDamage
> damageDF <- data.frame(Event = names(damage), damage = damage)
> econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE), n=5))
> econTop5
[1] "flood"             "hurricane/typhoon" "tornado"           "storm surge"      
[5] "hail"             
> stormData2000 = stormData[stormData$decade == "2000-2011",]
> stormData$casualties = with(stormData2000, FATALITIES + INJURIES)
Error in `$<-.data.frame`(`*tmp*`, "casualties", value = c(0, 0, 0, 0,  : 
  replacement has 523163 rows, data has 902297
> casualties = with(stormData, tapply(casualties, Event, sum))
> casualtiesDF <- data.frame(Event = names(casualties), casualties = casualties)
> healthTop5 = names(head(sort(casualtiesDF$casualties, decreasing = TRUE), n=5))
> healthTop5
[1] "tornado"        "excessive heat" "tstm wind"      "flood"          "lightning"     
> stormData2000$PROPDMGEXP = tolower(stormData$PROPDMGEXP)
Error in `$<-.data.frame`(`*tmp*`, "PROPDMGEXP", value = c("k", "k", "k",  : 
  replacement has 902297 rows, data has 523163
> stormData2000$CROPDMGEXP = tolower(stormData$CROPDMGEXP)
Error in `$<-.data.frame`(`*tmp*`, "CROPDMGEXP", value = c("", "", "",  : 
  replacement has 902297 rows, data has 523163
> property = stormData2000[(stormData2000$PROPDMGEXP %in% c("k", "m", "b")),]
> property$PROPDMGEXP = sub("k", 1, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("m", 1000, property$PROPDMGEXP)
> property$PROPDMGEXP = sub("b", 1000000, property$PROPDMGEXP)
> crops = stormData2000[(stormData2000$CROPDMGEXP %in% c("k", "m", "b")),]
> crops$CROPDMGEXP = sub("k", 1, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("m", 1000, crops$CROPDMGEXP)
> crops$CROPDMGEXP = sub("b", 1000000, crops$CROPDMGEXP)
> property$damage = property$PROPDMG * as.numeric(property$PROPDMGEXP)
> crops$damage = crops$CROPDMG * as.numeric(crops$CROPDMGEXP)
> cropDamage = with(crops, tapply(damage, Event, sum))
> propDamage = with(property, tapply(damage, Event, sum))
> damage = cropDamage + propDamage
> damageDF <- data.frame(Event = names(damage), damage = damage)
> econTop5 = names(head(sort(damageDF$damage, decreasing = TRUE), n=5))
> econTop5
[1] "flood"             "hurricane/typhoon" "storm surge"       "tornado"          
[5] "hail"             
> health = stormData2000[stormData2000$Event %in% healthTop5,]
> healthSum = head(sort(with(health, tapply(casualties, Event, sum))), n=5)
> healthSum
         flood      tstm wind      lightning excessive heat        tornado 
           581           1869           3459           4721          16406 
> health = stormData2000[stormData2000$Event %in% healthTop5,]
> healthSum = head(sort(with(health, tapply(casualties, Event, sum))), n=5)
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011",
+   ylab="Total Casualties (fatalities + injuries)", col=c("darkblue","red", "lightblue", "gray", "black"),
+   legend = colnames(healthSum))
Error in dn[[2L]] : subscript out of bounds
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011",
+   ylab="Total Casualties (fatalities + injuries)", col=c("darkblue","red", "lightblue", "gray", "black"),
+   legend = colnames(healthSum))
Error in dn[[2L]] : subscript out of bounds
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011",
+   ylab="Total Casualties (fatalities + injuries)", legend = colnames(healthSum))
Error in dn[[2L]] : subscript out of bounds
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011", ylab="Total Casualties (fatalities + injuries)", legend = colnames(healthSum))
Error in dn[[2L]] : subscript out of bounds
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011", ylab="Total Casualties (fatalities + injuries)", legend = rownames(healthSum))
> barplot(healthSum, main="Storm Events with Greatest Casualties, 2000-2011", ylab="Total Casualties (fatalities + injuries)", col = "darkblue")
> econ = stormData2000[stormData2000$Event %in% econTop5,]
> healthSum = head(sort(with(econ, tapply(damage, Event, sum))), n=5)
Error in tapply(damage, Event, sum) : arguments must have same length
> barplot(healthSum, main="Storm Events with Greatest Economic Damage, 2000-2011", ylab="$1000's of dollars in property or crop damage", col = "darkblue")