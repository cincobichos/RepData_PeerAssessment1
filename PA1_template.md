# Reproducible research - Course project 1
Beatriz Beisiegel  
13 de março de 2016  


###1. Loading and preprocessing the data

```r
unzip ("activity.zip")
activity <- read.csv("activity.csv")
library (dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
meansteps<- group_by (activity, date)
```

###2. What is mean total number of steps taken per day?


```r
totalstepsday <- summarize (meansteps, tsd = sum(steps, na.rm = TRUE))
hist (totalstepsday$tsd, xlab= "Steps", ylab = "Number of days", main = "Number of steps/day")
```

![](PA1_template_files/figure-html/hist meantotalsteps-1.png)

```r
meants <- filter (totalstepsday, tsd !=0) # removing days with no measures of step numbers
mnstp <- mean (meants$tsd)
mdstp <- median(meants$tsd)
```

Mean total steps taken per day = 1.0766189\times 10^{4}

Median total steps taken per day = 10765

###3. What is the average daily activity pattern?


```r
act2 <- activity [!is.na (activity$steps), ]
act3 <- group_by(act2, interval)
act4<- summarize(act3, mean (steps)) 
plot (act4, type="l", ylab= "Average number of steps", xlab= "5-min interval")
```

![](PA1_template_files/figure-html/ts average steps-1.png)

```r
names (act4) <- (c("Int", "meansteps"))
x<- which.max(act4$meansteps)
y<- act4$Int [x] 
```

The 5-minutes interval with maximum average number of steps is 835, which corresponds to 8:35 AM. 

###4. Inputing missing values


```r
missing <- sum (is.na (activity$steps))
```

There are 2304 rows of missing values.

The code below creates a new "act2" object in which all the missing values are replaced by the mean number of steps.


```r
activity2 <- activity
msteps <- mean (activity2$steps, na.rm = TRUE)
msteps
```

```
## [1] 37.3826
```

```r
idx <- which(is.na (activity2$steps))
activity2$steps[idx] <- msteps
```


```r
meansteps2<- group_by (activity2, date)
totalstepsday2 <- summarize (meansteps2, tsd= sum(steps))
hist (totalstepsday2$tsd, xlab= "Steps", ylab = "Number of days", main = "Number of steps/day")
```

![](PA1_template_files/figure-html/meantotalsteps completed-1.png)

```r
mnst2<- mean (totalstepsday2$tsd)
mdst2 <- median(totalstepsday2$tsd)
```

Mean total steps taken per day = 1.0766189\times 10^{4}

Median total steps taken per day = 1.0766189\times 10^{4}

These values are very similar to the estimates from the first part of the assignment. Thus, it may be concluded that the impact of imputing missing data on the estimates of the total daily number of steps is negligible.


###5. Are there differences in activity patterns between weekdays and weekends?


```r
dias<- strptime (activity$date, "%Y-%m-%d")
semanas <- weekdays (dias)
weekend <- which (semanas == "sábado" | semanas == "domingo" ) 
weekday <- which (semanas == "segunda-feira" | semanas == "terça-feira" | semanas == "quarta-feira" | semanas == "quinta-feira" | semanas == "sexta-feira")
semanas [weekend] <- "weekend"
semanas [weekday] <- "weekday"
workday <- as.factor(semanas)
activity3 <- cbind (activity2, workday)
wd <- filter (activity3, workday == "weekday")
we <- filter (activity3, workday == "weekend")
wd2 <- group_by(wd, interval)
wd3<- summarize(wd2, mean (steps)) 
we2 <- group_by(we, interval)
we3<- summarize(we2, mean (steps)) 
par (mfrow = c (2,1), mar= c(4,4,1,1))
plot (wd3, type="l", ylab= "Number of steps", xlab= "Interval", main = "Weekdays", cex.main =1)
plot (we3, type="l", ylab= "Number of steps", xlab= "Interval", main = "Weekends", cex.main = 1)
```

![](PA1_template_files/figure-html/dif weekend weekday-1.png)

