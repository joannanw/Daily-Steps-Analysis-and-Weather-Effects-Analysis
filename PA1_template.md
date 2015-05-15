---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---
## By Joanna Widjaja, 15 Feb 2015 

## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
aggActivityDate <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(aggActivityDate$steps, main = "Total Number of Steps", xlab = "Total Number of Steps by Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Below is the mean and median of the total number of steps taken per day:

```r
mean(aggActivityDate$steps)
```

```
## [1] 10766.19
```

```r
median(aggActivityDate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
aggActivityInt <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
library(ggplot2)
ggplot(aggActivityInt, aes(interval, steps)) + geom_line(colour="#BB0000") + labs(title="Mean of Steps by Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Below is the interval that has the maximum mean number of steps:

```r
aggActivityInt[aggActivityInt$steps == max(aggActivityInt$steps), 1]
```

```
## [1] 835
```

## Inputing missing values
Below is the number of missing rows:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Below is the new dataset with missing values replaced:

```r
library(plyr)
newActivity <- join(activity, aggActivityInt, by = 'interval')
newActivity[is.na(newActivity$steps),1] <- newActivity[is.na(newActivity$steps),4]
newActivity <- newActivity[,c(1,2,3)]
head(newActivity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Graph new histogram.

```r
aggNewActivity <- aggregate(steps ~ date, data = newActivity, FUN = sum)
hist(aggNewActivity$steps, main = "Total Number of Steps", xlab = "Total Number of Steps by Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Calculate mean and median.

```r
mean(aggNewActivity$steps)
```

```
## [1] 10766.19
```

```r
median(aggNewActivity$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
newActivity$wday <- ifelse(0 < as.POSIXlt(newActivity$date)$wday & as.POSIXlt(newActivity$date)$wday < 6, 1, 0)
newActivity$wday <- factor(newActivity$wday, levels = c(0,1), labels = c("weekend", "weekday"))
aggNewActivity <- aggregate(steps ~ interval + wday, data = newActivity, FUN = mean)
library(lattice)
xyplot(steps ~ interval | factor(wday), data = aggNewActivity, aspect = 1/2,type = "l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

