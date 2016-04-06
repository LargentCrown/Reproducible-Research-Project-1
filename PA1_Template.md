# PA_1 Template
John Daniels  
April 1, 2016  

# **Project 1 for Reproducible Research**

## Introduction
John Hopkins University and Coursera assigned a research assignment that required
the use of data from a personal activity devices like *Fitbit, Nike Fuelband or 
Jawbone Up.* More specifically this device gathers data at 5 minute intervals
during the day. The data consists of two months of information from an
annonymous canidate collected during the months of Oct and Nov 2012 and include the number of steps taken daily in 5 minute intervals. 

## Loading and processing the data into Rstudio, R x64 3.2.4 
 Load the dataset, read.csv() the unzipped file and see what's in it.




```r
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity.zip", mode = "wb")
dateDownloaded <- date()

unzip("activity.zip")
activity = read.csv("activity.csv", header=T, colClasses = c("integer", "character", "integer") ,sep = ",")


dateDownloaded
```

```
## [1] "Tue Apr 05 20:50:16 2016"
```

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## **What is the total number of steps per day?**

1. Calculate the total number of steps per day. 
*By aggregating to total the sum.*



```r
activity$date <- as.Date(activity$date)
totalStepsdays <- aggregate(steps ~ date, activity,  FUN = sum)
colnames(totalStepsdays) <- c("days" ,"steps")

str(totalStepsdays)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ days : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```


*This data.frame shows us 53 observations of the Days and Steps.* 

2. Make a histogram of the total number of steps per day.



```r
par(mar = c(5,8,3,1))

hist(totalStepsdays$steps, breaks = 10, 
   col = "blue", xlab = "Daily Steps", main = "Total Steps In A Day")
```

![](PA1_Template_files/figure-html/Total Steps In A Day-1.png)

3. Calculate and report the mean and median of the total number of steps per day



```r
mean(totalStepsdays$steps)
```

```
## [1] 10766.19
```


*The mean of the total number of steps per day is 10766.19*

```r
median(totalStepsdays$steps)
```

```
## [1] 10765
```


*The median of the total number of steps per day is 10765*

## **What is the average daily activity pattern?**
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
*To get the average daily activity pattern aggregate the date on stepsby interval and use the mean*


```r
AVGsteps <- aggregate(steps ~ interval, activity, FUN = mean, na.rm = TRUE)
colnames(AVGsteps) <- c("interval", "avg")

str(AVGsteps)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ avg     : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```



```r
with(AVGsteps, plot(interval, avg, type ='l', xlab = "Time Intervals", 
ylab = "Average Interval Daily Steps", main = "Average Daily Activity 
Pattern", col = "blue" ))
```

![](PA1_Template_files/figure-html/Average Interval Daily Steps-1.png)


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
MAXsteps <- max(AVGsteps$avg)
AVGsteps[AVGsteps$avg == MAXsteps, ]
```

```
##     interval      avg
## 104      835 206.1698
```
*The interval with the largest average number steps was at 835 and an average of 206 steps.*

## **Imputing missing values**
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


*The total number of NA's | missing values in the dataset is 2304.*

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
*Since there's like 2304 NA's|missing values I'll sub the NA's with the avg for that 5-minute interval.* 



```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
activity2 <- subset(activity, !is.na(activity$steps))
dAta <- activity
imPact <- is.na(dAta$steps)
tAvg <- tapply(activity2$steps, activity2$interval, mean, na.rm = TRUE, simplify = T)
dAta$steps[imPact] <- tAvg[as.character(dAta$interval[imPact])]
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
totalStepsperday2 <- aggregate(steps ~ date, dAta, FUN = sum)
```




```r
 par(mar = c(5,8,3,1))

hist(totalStepsperday2$steps, breaks = 10, col = "green", xlab = "Daily Steps", main = "Total Steps In A Day")
hist(totalStepsdays$steps, breaks = 10, col = "blue", xlab = "Daily Steps", 
main = "Total Steps In A Day", add=T) 
legend("topleft", c("Imputed", "NA"), fill = c("green","blue"))
```

![](PA1_Template_files/figure-html/Total Steps In A Day Imputed-1.png)




```r
mean(totalStepsdays$steps)
```

```
## [1] 10766.19
```

```r
mean(totalStepsperday2$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsdays$steps)
```

```
## [1] 10765
```

```r
median(totalStepsperday2$steps)
```

```
## [1] 10766.19
```


*Yes both means seemed to remain the same but the median after imputing slightly raised.*


## **Are there differences in activity patterns between weekdays and weekends?**
*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*


1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
dAta$date <- as.Date(dAta$date)
dAta$day <- weekdays(dAta$date)
dAta$indicate_day <- as.factor(ifelse(dAta$day == "Saturday" | dAta$day == "Sunday", "weekend", "weekday"))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
library(lattice)
g <- aggregate(steps ~ interval + indicate_day, dAta, mean)
xyplot(steps ~ interval | factor(indicate_day), g, layout=c(1, 2), main 
= "Time Series Plot For Weekends and Weekdays", xlab = "Interval", ylab 
= "Avg Steps Count", lty = 1, typ = "l" )
```

![](PA1_Template_files/figure-html/Time Series Plot For Weekends and Weekdays-1.png)

