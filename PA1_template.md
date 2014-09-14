---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
### settings

```r
echo = TRUE  

options(scipen = 1) 
```

## Loading and preprocessing the data
Show any code that is needed to

* Load the data (i.e. read.csv())

* Process/transform the data (if necessary) into a format suitable for your analysis



```r
unzip("activity.zip")
```

```
## Warning: error 1 in extracting from zip file
```

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
data$month <- as.numeric(format(data$date, "%m"))
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
withoutNA <- na.omit(data)
rownames(withoutNA) <- 1:nrow(withoutNA)
```

```
## Error: argument of length 0
```

```r
head(withoutNA)
```

```
## Error: No method for subsetting an XMLInternalDocument with integer
```

```r
dim(withoutNA)
```

```
## NULL
```

```r
library(ggplot2)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
ggplot(withoutNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

```
## Error: ggplot2 doesn't know how to deal with data of class
## XMLInternalDocumentXMLAbstractDocument
```

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalStepsPerDay <- aggregate(withoutNA$steps, list(Date = withoutNA$date), FUN = "sum")$x
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
mean(totalStepsPerDay)
```

```
## Error: object 'totalStepsPerDay' not found
```
Median total number of steps taken per day:

```r
median(totalStepsPerDay)
```

```
## Error: object 'totalStepsPerDay' not found
```

## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageSteps <- aggregate(withoutNA$steps, list(interval = as.numeric(as.character(withoutNA$interval))), FUN = "mean")
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
names(averageSteps)[2] <- "meanOfSteps"
```

```
## Error: object 'averageSteps' not found
```

```r
ggplot(averageSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

```
## Error: object 'averageSteps' not found
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageSteps[averageSteps$meanOfSteps == max(averageSteps$meanOfSteps), ]
```

```
## Error: object 'averageSteps' not found
```




## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(data))
```

```
## Warning: is.na() applied to non-(list or vector) of type 'externalptr'
```

```
## [1] 0
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

using the mean for 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- data 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- averageSteps[which(newData$interval[i] == averageSteps$interval), ]$meanOfSteps
    }
}
```

```
## Error: argument of length 0
```

```r
head(newData)
```

```
## Error: No method for subsetting an XMLInternalDocument with integer
```

```r
sum(is.na(newData))
```

```
## Warning: is.na() applied to non-(list or vector) of type 'externalptr'
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

```
## Error: ggplot2 doesn't know how to deal with data of class
## XMLInternalDocumentXMLAbstractDocument
```

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
NTotalStepsPerDay <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
newMean <- mean(NTotalStepsPerDay)
```

```
## Error: object 'NTotalStepsPerDay' not found
```

```r
newMean
```

```
## Error: object 'newMean' not found
```
Median total number of steps taken per day:

```r
newMedian <- median(NTotalStepsPerDay)
```

```
## Error: object 'NTotalStepsPerDay' not found
```

```r
newMedian
```

```
## Error: object 'newMedian' not found
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalStepsPerDay)
```

```
## Error: object 'totalStepsPerDay' not found
```

```r
oldMedian <- median(totalStepsPerDay)
```

```
## Error: object 'totalStepsPerDay' not found
```

```r
newMean - oldMean
```

```
## Error: object 'newMean' not found
```

```r
newMedian - oldMedian
```

```
## Error: object 'newMedian' not found
```
After imputing the missing data, the new mean of total steps taken per day is equal to the old mean. the new median of total steps taken per day is greater thanthe old median.



## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
## Error: No method for subsetting an XMLInternalDocument with integer
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
levels(newData$weekdays)
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
levels(newData$weekdays)
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
table(newData$weekdays)
```

```
## Error: object of type 'externalptr' is not subsettable
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
averageSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
```

```
## Error: object of type 'externalptr' is not subsettable
```

```r
names(averageSteps)[3] <- "meanOfSteps"
```

```
## Error: object 'averageSteps' not found
```

```r
library(lattice)
xyplot(averageSteps$meanOfSteps ~ averageSteps$interval | averageSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error: object 'averageSteps' not found
```
