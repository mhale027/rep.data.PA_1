---
title: "Reproducable Research: Peer Assessment 1"
author: "Matthew Hale"
output: html_document
---



##Loading and preprocessing the data

The read.csv function takes care of unzipping the files, and the lubridate package easily converts dates to POSIX.


```r
library(lubridate)
data <- read.csv("~/Coursera/rep.data.PA_1/activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day?

In this section I will ignore the NA values in the steps column of the dataset and determine the total number of steps taken each day using the tapply function with the date column as my splitting factor. The histogram shows the frequency of steps taken per day.


```r
day.sums <- tapply(data$steps, data$date, sum, na.rm=TRUE)
hist(day.sums, main="Histogram of Total Steps Taken per Day", xlab="Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean number of steps per day is:

```r
mean(day.sums)
```

```
## [1] 9354.23
```
The median number of steps per day is: 

```r
median(day.sums)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

Here I use the tapply function with the interval column as my splitting factor to determine the mean number of steps taken during each individual time interval. 


```r
time.means <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
```
The interval with the maximum mean number of steps taken is:

```r
data[which.max(time.means),]$interval
```

```
## [1] 835
```

##Imputing missing values

To eliminate the NA values I first substitute the average value for the interval in question with a for loop, creating a new dataset with no missing values. I then use another tapply split with the date to determine again the number of steps taken per day without the affect of the NA values. The histogram clearly shows a more normal distribution, with the peak in the same position, but with more symmetry abouht the mean. 


```r
data.no.nas <- data
time.sums <- NULL
num.nas <- length(is.na(data$steps))
for (i in which(is.na(data$steps))) {
        data.no.nas[i,]$steps <- time.means[as.character(data[i,]$interval)]}
day.sums.no.nas <- tapply(data.no.nas$steps, data.no.nas$date, sum)
hist(day.sums.no.nas, main="Histogram of Total Steps Taken per Day", xlab="Total Steps per Day", ylab="Frequency")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

The number of NA values can is:

```r
num.nas
```

```
## [1] 17568
```
The new mean and median are:

```r
print(mean(day.sums.no.nas)); median(day.sums.no.nas)
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```
With differences of: 

```r
print(mean(day.sums.no.nas) - mean(day.sums)); median(day.sums.no.nas) - median(day.sums)
```

```
## [1] 1411.959
```

```
## [1] 371.1887
```

##Are there differences in activity patterns between weekdays and weekends?

Since the date column in the dataset is already a POSIX object, a simple call to weekdays() converts the date into a day of the week. Then using the grep function I separated the days into either Weekday or Weekend. Using the new factor variable, the data is easy to subset into individual objects ready to plot. 


```r
data.no.nas$weekday <- weekdays(data.no.nas$date)
weekdays <- grep("Monday|Tuesday|Wednesday|Thursday|Friday", data.no.nas$weekday)
weekends <- grep("Saturday|Sunday", data.no.nas$weekday)
data.no.nas[weekdays,]$weekday <- "Weekday"
data.no.nas[weekends,]$weekday <- "Weekend"
wd <- data.no.nas[data.no.nas$weekday=="Weekday",]
we <- data.no.nas[data.no.nas$weekday=="Weekend",]
days.mean <- tapply(wd$steps, wd$interval, mean)
ends.mean <- tapply(we$steps, we$interval, mean)


par(mfrow=c(2,1), pin=c(5,1.2))
plot(days.mean, type="l", ylab="Mean steps per interval", 
     xlab="Weekday interval index", ylim=c(0,250))
plot(ends.mean, type="l", ylab="Mean steps per interval", 
     xlab="Weekend interval index", ylim=c(0,250))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

These plots would suggest that during the week, in the beginning of the day, the subject is quite active (possibly commuting to work, or some other morning activity), but during the bulk of the day, the weekends are a slightly more active time.
