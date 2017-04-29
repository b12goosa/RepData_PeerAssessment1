# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Loading and preprocessing the data.Show any code that is needed to Load the data (i.e. read.csv()).
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
##check statement for the file activity.csv
if (file.exists("activity.csv")==FALSE){
        unzip(zipfile = "activity.zip") 
}else ##load the data in a variable called "dat"
        dat<-read.csv(file = "activity.csv",sep = ",",
                      header = TRUE,
                      na.strings = "NA",
                      colClasses = c("numeric","Date","numeric"))
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day.
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
#obtain the sums of total steps per day
total_steps_by_day<-tapply(X = dat$steps,
                           INDEX = dat$date,
                           FUN = function(x) sum(x,na.rm = TRUE))

#Create an histogram with the total number of steps per day.
hist(x = total_steps_by_day, 
     main = "TOTAL NUMBER OF STEPS PER DAY",
     breaks = 50,
     ylab = "Frequency",
     xlab = "Number of steps(with 50 breaks)",
     col = "blue")
```

![](PA1_template_files/figure-html/Mean_number_of_steps_per_day-1.png)<!-- -->

```r
#The mean and median of the total number of steps per day
mean_total_steps_by_day<-mean(total_steps_by_day)
median_total_steps_by_day<-median(total_steps_by_day)
```
3. Calculate and report the mean and median of the total number of steps taken per day
        Mean is 9354.2295082 and Median is 1.0395\times 10^{4}.

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
#aggregate the average steps by interval
average_steps_by_interval<- aggregate(steps ~ interval, data = dat, FUN=mean)
#Time series plot
plot(x = average_steps_by_interval$interval,
     y = average_steps_by_interval$steps,
     type = "l",
     col = "blue",
     main = "AVERAGEN NUMBER OF STEPS PER DAY BY INTERVALS",
     xlab = "Intervals(by 5 minute)",
     ylab = "Mean of steps in each interval")
```

![](PA1_template_files/figure-html/Average number of steps taken-1.png)<!-- -->

```r
#Interval which contains the maximum number of steps in the interval
max_interval<-average_steps_by_interval[which.max(average_steps_by_interval$steps),1]
```
2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        The 5-minute interval which contains the maximum number of steps is 835.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (*i.e. the total number of rows with NAs*)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#1.Total number of row with NAs
nasteps<-length(dat$steps)-length(na.exclude(dat$steps))
#2.AND 3.fill in all the missing values with the mean for that day.For this purpose, transform the missing data into the average of total number of steps in each interval.
fill_dat<-transform(dat, 
                    steps = ifelse(is.na(dat$steps),
                                   average_steps_by_interval$steps[match(dat$interval,average_steps_by_interval$interval)],
                                   dat$steps))
#The first (i.e 2012-10-01) day hasn't any data (all NA values), so the zero value will be assigned.
fill_dat[as.character(fill_dat$date) == "2012-10-01",1]<-0 

#4.Obtain the sums of total steps per day with the fill in data
total_steps_by_day_fill<-tapply(X = fill_dat$steps,
                           INDEX = fill_dat$date,
                           FUN = function(x) sum(x,na.rm = TRUE))
#Create an histogram with the total number of steps per day with the new data(fill in data).
par(mfrow=c(1,2), mar=c(6,3,3,1)) #To compare later with non-imputed data
hist(x = total_steps_by_day_fill, 
     main = "TOTAL NUMBER OF STEPS",
     sub = "Imputed data",
     breaks = 50,
     ylab = "Frequency",
     xlab = "Number of steps(with 50 breaks)",
     col = "blue")
#Compare both histograms
hist(x = total_steps_by_day, 
     main = "TOTAL NUMBER OF STEPS",
     sub = "Non-imputed data",
     breaks = 50,
     ylab = "Frequency",
     xlab = "Number of steps(with 50 breaks)",
     col = "red")
```

![](PA1_template_files/figure-html/Imputing missing values-1.png)<!-- -->

```r
#The mean and median of the total number of steps per day
mean_total_steps_by_day_fill<-mean(total_steps_by_day_fill)
median_total_steps_by_day_fill<-median(total_steps_by_day_fill)
```
The total number of missing values in the dataset 2304.


Mean is 1.0589694\times 10^{4} and Median is 1.0766189\times 10^{4}.

## Are there differences in activity patterns between weekdays and weekends?
Are there differences in activity patterns between weekdays and weekends?

For this part the **weekdays()** function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
#Create a new factor variable in the data set with the two mentioned levels = 'weekdays' and 'weekend'
fill_dat$weekday<-weekdays(fill_dat$date)
fill_dat$weekday<-ifelse(fill_dat$weekday == "sábado" | fill_dat$weekday=="domingo",
       fill_dat$weekday<-"weekend",
       fill_dat$weekday<-"weekday")
#2.Aggregate the average steps by interval
average_steps_by_interval_weekday<- aggregate(steps ~ interval + weekday, data = fill_dat, FUN=mean)
#Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
library("lattice")
xyplot(steps~interval|weekday,layout=c(1,2),
       data = average_steps_by_interval_weekday,
       type = "l",
       main = "AVERAGE STEPS BY WEEKDAYS",
       xlab = "5-minutes interval",
       ylab = "average number of steps taken")
```

![](PA1_template_files/figure-html/activity patterns in weekend-1.png)<!-- -->
