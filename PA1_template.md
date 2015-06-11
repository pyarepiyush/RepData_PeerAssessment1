---
title: "Reproducible Research: Peer Assignment 1"
output: html_document
---




## Global Options

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

## Read in the data & check for missing values


```r
activity <- read.csv("C:/Users/pneupane/Documents/study topics/coursera/rep_research/indata/activity.csv",header=T)

sapply(activity,function (x) sum(is.na(x)) )
```

```
##    steps     date interval 
##     2304        0        0
```




## What is mean total number of steps taken per day?


###  For this part of the assignment, you can ignore the missing values in the dataset.

  * Calculate the total number of steps taken per day

  * If you do not understand the difference between a histogram and a barplot, research the difference between them. 
  * Make a histogram of the total number of steps taken each day

  * Calculate and report the mean and median of the total number of steps taken per day


```r
activity1 <- activity[!is.na(activity),]
tot.steps <- aggregate(steps~date,data=activity1,sum)
hist(tot.steps$steps, breaks=30, col="orange" , xlab="Total number of steps per day", main=" Histogram of total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

  * Calculate and report the mean and median of the total number of steps taken per day


```r
mean(tot.steps$steps)
```

```
## [1] 10766.19
```

```r
median(tot.steps$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?



* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
* the average number of steps taken, averaged across all days (y-axis)
* Total number of steps for each day


```r
avg.interval <- aggregate(steps~interval,data=activity1,mean)
library(ggplot2)
with(avg.interval, qplot(x=interval,y=steps,geom = "line"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
with(avg.interval, avg.interval[which(steps==max(steps)),1])
```

```
## [1] 835
```




## Imputing missing values

* Note that there are a number of days/intervals where there are missing values (coded as NA). 
* The presence of missing days may introduce bias into some calculations or summaries of the data.


* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

* Calculate and report the mean and median of the total number of steps taken per day

```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
mean.interval <- aggregate(steps~interval,data=activity1,mean)
library(plyr)
mean.interval <- rename(mean.interval, c("steps"="mean.steps"))


activity.imputed<- merge(activity,mean.interval, by="interval",all.x=T)
activity.imputed$steps1 <- with(activity.imputed, ifelse(is.na(steps),mean.steps,steps) )


tot.steps1 <- aggregate(steps1~date,data=activity.imputed,sum)


hist(tot.steps1$steps1, breaks=30, col="red", xlab="Total number of steps per day", main=" Histogram of total number of steps taken per day (Imputed)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
mean(tot.steps1$steps1)
```

```
## [1] 10766.19
```

```r
median(tot.steps1$steps1)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


* For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
* Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
table(weekdays(as.Date(activity.imputed$date)))
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##      2592      2592      2304      2304      2592      2592      2592
```

```r
activity.imputed$weekend <- with(activity.imputed, ifelse(weekdays(as.Date(activity.imputed$date)) %in% c("Saturday",    "Sunday"), "Weekend","Weekday") )


avg.interval1 <- aggregate(steps1~weekend+interval,data=activity.imputed,mean)
library(ggplot2)

a <- ggplot(data = avg.interval1, aes(x = interval, y = steps1, color=weekend))
a <- a + geom_line(size=1)
a <- a + facet_grid(weekend ~. )
a <- a + xlab("5-minute Intervals") + ylab("Average number of steps taken") + ggtitle("Steps averaged for Intervals across all weekday days or weekend days")
a
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
