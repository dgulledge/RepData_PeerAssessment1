---
output: html_document
---
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### Read the raw data

```r
activity <- read.csv('activity.csv')
```
### Filter out unrecorded intervals (steps = NA)

```r
recorded.activity <- activity[!is.na(activity$steps),]
```
### Calculate steps/day for days containing at least one recorded interval

```r
steps.per.day <- tapply(recorded.activity$steps, recorded.activity$date, sum)
steps.per.day.filtered <- steps.per.day[!is.na(steps.per.day)]
```
### Calculate steps/interval across all days

```r
steps.per.interval <- tapply(recorded.activity$steps, recorded.activity$interval, mean)
steps.per.interval.filtered <- steps.per.interval[!is.na(steps.per.interval)]
```
### Calculate interval times in minutes since midnight

Graphing the activity by interval causes the intervals between 55 minutes past one hour
and the start of the next hour to appear to be 45 minutes long instead of only 5 minutes.
The simplest way to change this is to convert the times to minutes past midnight.  This
means that 10:00 am, which was previously labeled 1000, will be labeled as 600 minutes
past midnight (60 minutes/hour * 10 hours).  This labeling sacrifices some clarity for
greater accuracy.

```r
intervals <- unique(activity$interval)
interval.times <- (intervals%/%100)*60 + intervals%%100
```


## What is mean total number of steps taken per day?
### Histogram of steps per day

```r
hist(steps.per.day.filtered, breaks = 15)
```

![plot of chunk histogram.steps.per.day](figure/histogram.steps.per.day.png) 
### Mean and median steps per day

```r
mean(steps.per.day.filtered)
```

```
## [1] 10766
```

```r
median(steps.per.day.filtered)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
plot(x = interval.times, y = steps.per.interval.filtered, type = "l", main = "Mean steps per interval", xlab = "Minute from midnight", ylab = "Steps")
```

![plot of chunk timeseries.steps.per.interval](figure/timeseries.steps.per.interval.png) 
### Find interval with maximum average steps

```r
steps.per.interval.filtered[steps.per.interval.filtered == max(steps.per.interval.filtered)]
```

```
##   835 
## 206.2
```

## Imputing missing values
### Impute missing data from mean value for corresponding interval across all days

```r
imputed.activity <- activity
for ( r in 1:nrow(imputed.activity) )
  if ( is.na(imputed.activity[r,]$steps) )
    imputed.activity[r,]$steps <- steps.per.interval.filtered[toString(imputed.activity[r,]$interval)]
```
### Recalculate steps per day from imputed data

```r
imputed.steps.per.day <- tapply(imputed.activity$steps, imputed.activity$date, sum)
```
### Histogram of steps per day from imputed data

```r
hist(imputed.steps.per.day, breaks = 15)
```

![plot of chunk imputed.histogram](figure/imputed.histogram.png) 
### Mean and median steps per day from imputed data

```r
mean(imputed.steps.per.day)
```

```
## [1] 10766
```

```r
median(imputed.steps.per.day)
```

```
## [1] 10766
```

Imputing the missing data using the mean for the corresponding interval didn't significantly impact the mean and median calculations.  The median changed to match the mean exactly.  Since the effect of imputing several entire days of missing data from the means of the intervals was to impute several days which were exactly equal to the mean, that makes sense.  It can be seen in the histogram where the number of observations in the range containing the mean and median increased by the number of days for which the data was imputed.

## Are there differences in activity patterns between weekdays and weekends?
### Create a factor for weekdays

```r
wd <- weekdays(as.Date(imputed.activity$date))
is.weekday <- !(wd == "Saturday" | wd == "Sunday")
imputed.activity <- cbind(imputed.activity, is.weekday)
```

### Create panel plot of weekdays and weekends

```r
steps.per.interval.weekdays <- tapply(imputed.activity$steps[imputed.activity$is.weekday], imputed.activity$interval[imputed.activity$is.weekday], mean)
steps.per.interval.weekends <- tapply(imputed.activity$steps[!imputed.activity$is.weekday], imputed.activity$interval[!imputed.activity$is.weekday], mean)
par(mfrow=c(2,1))
plot(x = interval.times, y = steps.per.interval.weekends, type = "l", main = "weekend", ylab = "Number of steps", xlab = "")
plot(x = interval.times, y = steps.per.interval.weekdays, type = "l", main = "weekday", ylab = "Number of steps", xlab = "")
```

![plot of chunk create.weekday.weekend.plot](figure/create.weekday.weekend.plot.png) 
