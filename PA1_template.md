# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
# Download, unzip and load the data. 
destFile<-"activity.zip"
if(!file.exists(destFile)) {
  fileURL<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL,destFile)
  unzip(destFile)
}

data <- read.csv("activity.csv")

# Page settings
figHeight<-4
figWidth<-6
options(scipen = 1, digits = 2)
```

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data, sum)

# Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, breaks = 10, main = "Total Steps Each Day", col="red", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
mean_data <- mean(steps_per_day$steps)
median_data <- median(steps_per_day$steps)
```

Regarding the total number of steps taken per day, the mean is 10766.19 and the median is 10765.

## What is the average daily activity pattern?


```r
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 
steps_per_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_per_interval$interval,steps_per_interval$steps, type="l", xlab="5-minute Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Show which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Imputing missing values


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
narows <- sum(!complete.cases(data))
na_step<-sum(is.na(data$steps))
na_date<-sum(is.na(data$date))
na_interval<-sum(is.na(data$interval))
```
The total number of missing values in the dataset is 2304. 


```r
# Replace the missing values with the rounded mean for that 5-minute interval, and create the new dataset. 
data_impute <- transform(data, steps = ifelse(is.na(data$steps), round(steps_per_interval$steps[match(data$interval, steps_per_interval$interval)]), data$steps))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
steps_per_day_impute <- aggregate(steps ~ date, data_impute, sum)
hist(steps_per_day_impute$steps, breaks = 10, main = "Total Steps Each Day (Imputed data)", col="red", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean_impute <- mean(steps_per_day_impute$steps)
median_impute <- median(steps_per_day_impute$steps)
```

Regarding the total number of steps taken per day (imputed data), the mean is 10765.64 and the median is 10762.


```r
# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
mean_diff <- mean_data - mean_impute
median_diff <- median_data - median_impute
```

- The difference of the mean of the total number of steps taken per day between original data 10766.19 and imputed data 10765.64 is: 0.55;
- The difference of the median of the total number of steps taken per day between original data 10765 and imputed data 10762 is: 3;
- The impact of imputing missing data with the scheme of using the rounded mean for that 5-minute interval is: because of the rounded mean, which leads to a slight difference between the total daily number of steps. Whether the change is to increase or descrease the number of steps depends on the data. In this case, the change is decreasing the total daily number of steps. 

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data_impute$day = as.factor(ifelse(weekdays(as.Date(data_impute$date)) %in% c('Saturday','Sunday'), "Weekend", "Weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
steps_by_interval_i <- aggregate(steps ~ interval + day, data_impute, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$day, main="Average Number of Steps per Day by Interval",xlab="Interval", ylab="Number of steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
