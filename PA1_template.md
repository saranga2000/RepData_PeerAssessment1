# Reproducible Research: Peer Assessment 1
Vinod Kannan  
Thursday, March 12, 2015  


## Loading and preprocessing the data

Show any code that is needed to 

1. Load the data (i.e. read.csv()) 


```r
unzip("activity.zip")
activityData <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityData$month <- as.numeric(format(activityData$date, "%m"))
head(activityData)
```

```
##   steps       date interval month
## 1    NA 2012-10-01        0    10
## 2    NA 2012-10-01        5    10
## 3    NA 2012-10-01       10    10
## 4    NA 2012-10-01       15    10
## 5    NA 2012-10-01       20    10
## 6    NA 2012-10-01       25    10
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset. 

**We will Remove NA's**

```r
activityDataWithoutNA <- na.omit(activityData)
head(activityDataWithoutNA)
```

```
##     steps       date interval month
## 289     0 2012-10-02        0    10
## 290     0 2012-10-02        5    10
## 291     0 2012-10-02       10    10
## 292     0 2012-10-02       15    10
## 293     0 2012-10-02       20    10
## 294     0 2012-10-02       25    10
```

1. Calculate the total number of steps taken per day
      

```r
stepsPerDay <- aggregate(activityDataWithoutNA$steps, by=list(Date = activityDataWithoutNA$date), FUN = "sum")$x
head(stepsPerDay)
```

```
## [1]   126 11352 12116 13294 15420 11015
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
      
**http://www.shodor.org/interactivate/discussions/HistogramsVsBarGraph/ gives a very nice description of differences. **
      

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
ggplot(activityDataWithoutNA, aes(date, steps)) + geom_bar(stat = "identity") + facet_grid(. ~ month, scales = "free") + labs(x = "Date", y = "Daily total steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsPerDay)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
avgStepsInInterval <- aggregate(activityDataWithoutNA$steps, list(interval = as.numeric(as.character(activityDataWithoutNA$interval))), FUN = "mean")
names(avgStepsInInterval)[2] <- "avgSteps"
head(avgStepsInInterval)
```

```
##   interval  avgSteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
ggplot(avgStepsInInterval, aes(interval, avgSteps)) + geom_line() + labs(x = "Interval", y = "Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgStepsInInterval$interval[which.max(avgStepsInInterval$avgSteps)]
```

```
## [1] 835
```



## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(activityData))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**We can use mean for the 5-minute interval as filler for missing values**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDataWithFillerForNAs <- activityData
for (i in 1:nrow(activityDataWithFillerForNAs)) {
    if (is.na(activityDataWithFillerForNAs$steps[i])) {
        activityDataWithFillerForNAs$steps[i] <- avgStepsInInterval[which(activityDataWithFillerForNAs$interval[i] == avgStepsInInterval$interval), ]$avgSteps
    }
}
head(activityDataWithFillerForNAs)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(activityDataWithFillerForNAs))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ggplot(activityDataWithFillerForNAs, aes(date, steps)) + geom_bar(stat = "identity") + facet_grid(. ~ month, scales = "free") + labs(x = "Date", y = "Total steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
revisedTotalSteps <- aggregate(activityDataWithFillerForNAs$steps, 
                           list(Date = activityDataWithFillerForNAs$date), 
                           FUN = "sum")$x
revisedMean <- mean(revisedTotalSteps)
revisedMean
```

```
## [1] 10766.19
```

```r
revisedMedian <- median(revisedTotalSteps)
revisedMedian
```

```
## [1] 10766.19
```

```r
avgStepsWithoutNA <- aggregate(activityDataWithoutNA$steps, list(Date = activityDataWithoutNA$date), FUN = "sum")$x
oldMean <- mean(avgStepsWithoutNA)
oldMedian <- median(avgStepsWithoutNA)

revisedMean - oldMean
```

```
## [1] 0
```

```r
revisedMedian - oldMedian
```

```
## [1] 1.188679
```




## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityDataWithFillerForNAs$weekdays <- factor(format(activityDataWithFillerForNAs$date, "%A"))
levels(activityDataWithFillerForNAs$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(activityDataWithFillerForNAs$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
levels(activityDataWithFillerForNAs$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(activityDataWithFillerForNAs$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgStepsInIntervalWithFillerForNA <- aggregate(activityDataWithFillerForNAs$steps, 
                      list(interval = as.numeric(as.character(activityDataWithFillerForNAs$interval)), 
                           weekdays = activityDataWithFillerForNAs$weekdays),
                      FUN = "mean")
names(avgStepsInIntervalWithFillerForNA)[3] <- "avgSteps"
library(lattice)
xyplot(avgStepsInIntervalWithFillerForNA$avgSteps ~ avgStepsInIntervalWithFillerForNA$interval | avgStepsInIntervalWithFillerForNA$weekdays, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

