
Reproducible Reserach: Peer Assessment 1
==============================================

```r
opts_chunk$set(echo = TRUE)
library(lattice)
```
## Loading and preprocessing the data

```r
rawdata<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
## What is mean total number of steps taken per day?

1.Histogram of the total number of steps taken each day


```r
data<-rawdata[!is.na(rawdata$steps), ]  
```

```
## Error in eval(expr, envir, enclos): object 'rawdata' not found
```

```r
stepAgg<-aggregate(steps~date, data, sum)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
xLabels<-seq(1, nrow(stepAgg), by = 12)
```

```
## Error in nrow(stepAgg): object 'stepAgg' not found
```

```r
scalesList<-list(x = list(labels = stepAgg$date, at = xLabels))
```

```
## Error in eval(expr, envir, enclos): object 'stepAgg' not found
```

```r
barchart(steps~date, stepAgg, scales=scalesList)
```

```
## Error in barchart.formula(steps ~ date, stepAgg, scales = scalesList): object 'stepAgg' not found
```

2.Calcuate and report the mean and median total number of steps taken per day

```r
mean(stepAgg$steps)
```

```
## Error in mean(stepAgg$steps): object 'stepAgg' not found
```

```r
median(stepAgg$steps)
```

```
## Error in median(stepAgg$steps): object 'stepAgg' not found
```

What is the average daily activity patter?
1. Make a time series plot(i.e. type='l) of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis)

```r
intervalAgg<-aggregate(steps~interval, data, mean)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
xyplot(steps~interval, intervalAgg, type="l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'intervalAgg' not found
```

### 2.Which 5-mintue interval, on average across all days in the dataset, contains the maximum number of steps?

```r
row<-which.max(intervalAgg$steps)
```

```
## Error in which.max(intervalAgg$steps): object 'intervalAgg' not found
```

```r
intervalAgg[row,]$interval
```

```
## Error in eval(expr, envir, enclos): object 'intervalAgg' not found
```

## Inputing missing values
1.Calcuate and report total number of missing values in dataset

```r
sum(is.na(rawdata$steps))
```

```
## Error in eval(expr, envir, enclos): object 'rawdata' not found
```

2.Device a strategy for filling in all of the missing values in the dataset.

Use the mean for that interval across days to fill the missing value in the dataset.

3.create a new dataset that is equal to the origianl dataset but with the missing data filled in


```r
meanAgg<-aggregate(steps~interval, data, mean)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
numDates<-length(unique(rawdata$date))
```

```
## Error in unique(rawdata$date): object 'rawdata' not found
```

```r
newAgg<-meanAgg[rep(seq_len(nrow(meanAgg)), numDates),]
```

```
## Error in eval(expr, envir, enclos): object 'meanAgg' not found
```

```r
misRows<-is.na(rawdata$steps)
```

```
## Error in eval(expr, envir, enclos): object 'rawdata' not found
```

```r
rawdata[misRows,]$steps<-newAgg[misRows, ]$steps
```

```
## Error in eval(expr, envir, enclos): object 'newAgg' not found
```

```r
filledData<-rawdata
```

```
## Error in eval(expr, envir, enclos): object 'rawdata' not found
```

4.Make a histogram of the total number of steps taken each day

```r
stepAgg<-aggregate(steps~date, filledData, sum)
```

```
## Error in eval(expr, envir, enclos): object 'filledData' not found
```

```r
xLabels<-seq(1, nrow(stepAgg), by = 12)
```

```
## Error in nrow(stepAgg): object 'stepAgg' not found
```

```r
scalesList<-list(x = list(labels = stepAgg$date, at = xLabels))
```

```
## Error in eval(expr, envir, enclos): object 'stepAgg' not found
```

```r
barchart(steps~date, stepAgg, scales=scalesList)
```

```
## Error in barchart.formula(steps ~ date, stepAgg, scales = scalesList): object 'stepAgg' not found
```

Calculate and report the **mean** and **median** total number of steps taken per day

```r
mean(stepAgg$steps)
```

```
## Error in mean(stepAgg$steps): object 'stepAgg' not found
```

```r
median(stepAgg$steps)
```

```
## Error in median(stepAgg$steps): object 'stepAgg' not found
```

**Do these values differ from the estimates from the first part of the assignment?**

The mean value is different, median is the same.

**What is the impact of the inputing missing data on the estimates of the total daily number of steps?**

If there is no missing steps for that day, the total steps would be same. If there are steps missing for a day, the total daily number of steps would be set to the average total steps
across days.


## Are there differience in actitivy patterns between weekdays and weekend?

1.create a new factor variable in dataset with two levels--"weekday" and "weekend" indicating whether a given day is a weekday or weekend day.

```r
days<-!(weekdays(as.Date(filledData$date)) %in% c('Saturday','Sunday'))
```

```
## Error in as.Date(filledData$date): object 'filledData' not found
```

```r
wday<-c("weekend", "weekday")
f<-factor(days, labels = wday)
```

```
## Error in factor(days, labels = wday): object 'days' not found
```

2.Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
tdata<-transform(filledData, weekday = f)
```

```
## Error in transform(filledData, weekday = f): object 'filledData' not found
```

```r
aggWeekday<-aggregate(steps~interval, tdata[f==wday[2],], mean)
```

```
## Error in eval(expr, envir, enclos): object 'tdata' not found
```

```r
aggWeekend<-aggregate(steps~interval, tdata[f==wday[1],], mean)
```

```
## Error in eval(expr, envir, enclos): object 'tdata' not found
```

```r
numRows<-nrow(aggWeekday);
```

```
## Error in nrow(aggWeekday): object 'aggWeekday' not found
```

```r
aggWeekday<-transform(aggWeekday, day = rep(wday[2], numRows))
```

```
## Error in transform(aggWeekday, day = rep(wday[2], numRows)): object 'aggWeekday' not found
```

```r
aggWeekend<-transform(aggWeekend, day = rep(wday[1], numRows))
```

```
## Error in transform(aggWeekend, day = rep(wday[1], numRows)): object 'aggWeekend' not found
```

```r
aggData<-rbind(aggWeekday, aggWeekend);
```

```
## Error in rbind(aggWeekday, aggWeekend): object 'aggWeekday' not found
```

```r
xyplot(steps~interval | day, aggData, type="l", layout=c(1, 2), xlab="Interval", ylab="Number of steps")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'aggData' not found
```
