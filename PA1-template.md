##**Loading and preprocessing the data**
title: "FirstMarkdown"
author: "Vishakha"
date: "Tuesday, November 11, 2014"
output: html_document
---
Reproducible Research's Peer Assessment 1 - a single R markdown document that can be processed by knitr and be transformed into an HTML file.
As data was already downloaded and unzipped no need for these steps
Load the data (i.e. read.csv())
Set the zip filename:

filename <- 'activity.zip'
Unzip activity.zip:

unzip(filename)
Set the data-set filename:



```r
setwd("~/Rworkingdir/reproduciblereserach/first/repdata-data-activity")
filename <- 'activity.csv'
#Read the file into a data frame called activity:

column_classes <- c("integer","Date","integer")
activity <- read.csv(filename,colClasses = column_classes)
#Inspect the data:

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
   steps       date interval  
 1    NA 2012-10-01        0  
 2    NA 2012-10-01        5  
 3    NA 2012-10-01       10  
 4    NA 2012-10-01       15  
 5    NA 2012-10-01       20  
 6    NA 2012-10-01       25  

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
 'data.frame':    17568 obs. of  3 variables:  
  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...  
  $ date    : Date, format: "2012-10-01" "2012-10-01" ...  
  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...  

We can see the variables:

steps: Number of steps taken in a 5-minute interval (missing values are coded as NA)  

date: The date on which the measurement was taken in YYYY-MM-DD format  

interval: Identifier for the 5-minute interval in which measurement was taken  

We can also see that we got all 17,568 observations.  
 

activity[285:291,]  
steps       date interval  
 285    NA 2012-10-01     2340  
 286    NA 2012-10-01     2345  
 287    NA 2012-10-01     2350  
 288    NA 2012-10-01     2355  
 289     0 2012-10-02        0  
 290     0 2012-10-02        5  
 291     0 2012-10-02       10  
The intervals are given in the form 830 meaning 8:30 a.m. and not 830 minutes. To plot these times with the correct scale, I convert these to minutes since the start of the day:

```r
activity[,"interval"] <- (floor(activity[,"interval"]/100)*60 
                          + activity[,"interval"] %% 100)
```
Look at the end of the first day again:  

activity[285:291,]  
steps       date interval  
 285    NA 2012-10-01     1420  
 286    NA 2012-10-01     1425  
 287    NA 2012-10-01     1430  
 288    NA 2012-10-01     1435  
 289     0 2012-10-02        0  
 290     0 2012-10-02        5  
 291     0 2012-10-02       10  


#**What is mean total number of steps taken per day?**  
Get the total number of steps per day, ignoring NAs:  


```r
steps_per_day <- aggregate(activity[,"steps"],by=as.list(activity["date"]),
                           FUN=sum,na.rm=TRUE)
names(steps_per_day) <- c("date","total_steps")
#Look at the first few lines:

head(steps_per_day)
```

```
##         date total_steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```
         date total_steps  
 1 2012-10-01           0  
 2 2012-10-02         126  
 3 2012-10-03       11352  
 4 2012-10-04       12116  
 5 2012-10-05       13294  
 6 2012-10-06       15420  
##**Make a histogram of the total number of steps taken each day**
Create a histogram of the total number of steps taken per day:
code

```r
par( mar = c(5,4,2,1) )
hist(steps_per_day$total_steps, breaks=20, 
     xlab="total number of steps per day in thousands",
     main="Histogram of total number of steps taken each day",
     xlim = c(0,22500), axes = FALSE, ylim = c(0,10) )
axis(1, at = seq(0,22000,2000), labels= seq(0,22,2))
axis(2, at = seq(0,10,2), labels = seq(0,10,2) )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
mean_total_number_of_steps <- round(mean(steps_per_day[,"total_steps"]),0)
median_total_number_of_steps <- median(steps_per_day[,"total_steps"])
mean_steps_per_interval <- aggregate(activity[,"steps"],
                                     by = as.list(activity["interval"]),
                                     FUN = mean, na.rm=TRUE)
names(mean_steps_per_interval) <- c("interval","mean_steps")
```
Histogram

```r
par( mar = c(5,4,2,1) )
hist(steps_per_day$total_steps, breaks=20, 
     xlab="total number of steps per day in thousands",
     main="Histogram of total number of steps taken each day",
     xlim = c(0,22500), axes = FALSE, ylim = c(0,10) )
axis(1, at = seq(0,22000,2000), labels= seq(0,22,2))
axis(2, at = seq(0,10,2), labels = seq(0,10,2) )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
#plot of chunk total_number_of_steps_per_day_histogram

#Calculate and report the mean and median total number of steps taken per day
#Get the mean total number of steps per day

mean_total_number_of_steps <- round(mean(steps_per_day[,"total_steps"]),0)
#The person had [code: `r mean_total_number_of_steps`] 9354 as the mean total number of steps per #day.

#Get the median total number of steps per day

median_total_number_of_steps <- median(steps_per_day[,"total_steps"])
#The person had [code: `r median_total_number_of_steps`] 10395 as the median total number of #steps per day.

#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average #number of steps taken, averaged across all days (y-axis)
#Get the number of steps for the 5 minute intervals averaged over all days

mean_steps_per_interval <- aggregate(activity[,"steps"],
                                     by = as.list(activity["interval"]),
                                     FUN = mean, na.rm=TRUE)
names(mean_steps_per_interval) <- c("interval","mean_steps")
#Look at the first few results:

head(mean_steps_per_interval)
```

```
##   interval mean_steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```
*  interval mean_steps    
* 1        0    1.71698    
* 2        5    0.33962    
* 3       10    0.13208 
* 4       15    0.15094    
* 5       20    0.07547    
* 6       25    2.09434    
Plot the result


```r
par( mar = c(5,4,2,1))
plot(mean_steps_per_interval$interval,mean_steps_per_interval$mean_steps,
     type="l", xlab = "start time of interval", 
     ylab = "mean steps per five minute interval",
     main = "mean steps per five minute interval averaged over all days",
     axes = FALSE, xlim = c(0,1440), ylim = c(0,220))
axis(1, at = seq(0,1440,length.out=13), labels = paste0(seq(0,24,length.out=13),":00"))
axis(2, at = seq(0,225,25), labels = seq(0,225,25) )
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
#plot of chunk mean_steps_per_interval_plot

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum #number of steps?
#Get the start of the maximum interval.

interval_start <- mean_steps_per_interval[which.max(mean_steps_per_interval[,"mean_steps"]),"interval"]
interval_start
```

```
## [1] 515
```
## [1] 515

```r
maximum_mean_steps <- round(max(mean_steps_per_interval[,"mean_steps"]),0)
```
[code: 206] 206, the maximum mean number of steps, occurs in the [code: 515] 515 to [code: 520] 520 minute interval. This corresponds to the interval starting at [code: 8] 8:[code: 35] 35.  

Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
Obtain the number of missing values


```r
number_of_na <- sum(is.na(activity$steps))
number_of_na
```

```
## [1] 2304
```
## [1] 2304

```r
total_records <- length(activity$steps)
total_records
```

```
## [1] 17568
```
## [1] 17568
There are [code: 2304] 2304 NAs in the data set. This corresponds to [code: 13.1] 13.1% missing values.  

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The imputation strategy is to replace the NAs with the mean for the interval as calculated above. This introduces non-integer steps. Create a function for that purpose



```r
get_mean_for_interval <- function(interval){
    mean_steps_per_interval[ mean_steps_per_interval[,"interval"] == interval,
                             "mean_steps"]
}
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

Replace NA values with the mean value for that interval as calculated above:


```r
imputed_activity <- activity
imputed_activity[is.na(imputed_activity[,"steps"]), "steps"] <- 
    sapply(imputed_activity[is.na(imputed_activity[,"steps"]), "interval"],
           get_mean_for_interval)
```
Have a look at the result:

head(imputed_activity)  
*     steps       date interval   
* 1 1.71698 2012-10-01        0   
* 2 0.33962 2012-10-01        5   
* 3 0.13208 2012-10-01       10   
* 4 0.15094 2012-10-01       15   
* 5 0.07547 2012-10-01       20   
* 6 2.09434 2012-10-01       25   
Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

**Get the total number of steps per day**  


```r
imputed_steps_per_day <- aggregate(imputed_activity[,"steps"],
                                   by=as.list(imputed_activity["date"]),
                                   FUN=sum)
names(imputed_steps_per_day) <- c("date","total_steps")
head(imputed_steps_per_day)
```

```
##         date total_steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```
*         date total_steps  
* 1 2012-10-01       10766  
* 2 2012-10-02         126  
* 3 2012-10-03       11352  
* 4 2012-10-04       12116  
* 5 2012-10-05       13294  
* 6 2012-10-06       15420  
Create a histogram of the total number of steps taken per day after missing values were imputed:


```r
par(mar = c(5,4,2,2))
hist(imputed_steps_per_day$total_steps, breaks=20,
     xlab="total number of steps per day in thousands",
     main="Histogram of total number of steps taken each day",
     xlim = c(0,22500), axes = FALSE, ylim = c(0,20) )
axis(1, at = seq(0,22000,2000), labels= seq(0,22,2))
axis(2, at = seq(0,20,5), labels = seq(0,20,5) )
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
plot of chunk imputed_total_number_of_steps_per_day_histogram

Get the mean total number of steps per day


```r
imputed_mean_total_number_of_steps <- round(mean(imputed_steps_per_day[,"total_steps"]),0)
```
The person had [code: 10766] 10766 as the mean imputed total number of steps (compared to [code: 9354] 9354 unimputed).

Get the median total number of steps per day


```r
imputed_median_total_number_of_steps <- median(imputed_steps_per_day[,"total_steps"])
```
The person had [code: 10766] 10766 as the median imputed total number of steps (compared to [code: 10395] 10395 unimputed).

Imputation increases the estimates of the average total number of steps per day. The difference between the median and mean vanishes.

min(imputed_steps_per_day["total_steps"] - steps_per_day["total_steps"],na.rm=FALSE)
## [1] 0
max(imputed_steps_per_day["total_steps"] - steps_per_day["total_steps"],na.rm=FALSE)
## [1] 10766
Thus the impact of the imputation strategy is to either increase or not change the number of steps per day.

Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Add a column to imputed_activity indicating if a day was a workday or a weekend. Notice that weekend days (Saturday, Sunday) start with S.


```r
imputed_activity["day_category"] <- "weekday"
imputed_activity[grepl("^[Ss]",weekdays(imputed_activity[,"date"])),"day_category"] <- "weekend"
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
Get the number of steps for the 5 minute intervals averaged over all weekdays and weekend days


```r
imputed_mean_steps_per_interval <- aggregate(imputed_activity[,"steps"],
                                             by=as.list(c(imputed_activity["day_category"],
                                                          imputed_activity["interval"])),
                                             FUN=mean)
names(imputed_mean_steps_per_interval) <- c("day_category","interval","mean_steps")
```
Look at the first few observations:

head(imputed_mean_steps_per_interval)
*   day_category interval mean_steps  
* 1      weekday        0    2.25115  
* 2      weekend        0    0.21462  
* 3      weekday        5    0.44528  
* 4      weekend        5    0.04245   
* 5      weekday       10    0.17317  
* 6      weekend       10    0.01651  
Plot the result


```r
library(ggplot2)
g <- ggplot(imputed_mean_steps_per_interval,aes(interval,mean_steps))
g <- g + geom_line() + facet_wrap(~ day_category, nrow = 2) + theme_bw()
g <- g + ggtitle("Mean steps per interval split into weekdays and weekends")
g <- g + xlab("start time of five minute interval") 
g <- g + ylab("mean steps per interval")
g <- g + scale_x_continuous(breaks = seq(0, 1440, length.out = 13),
                            labels = paste0(seq(0, 24, length.out = 13),":00"))
g
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png) 

plot of chunk imputed_mean_steps_per_interval_plot

Based on the figure, we can see that the start to the day on a weekend is a bit slower than on a weekday. Further there is an additional peak between 8 and 9 p.m. on weekends. The imputation strategy did not differentiate between weekdays and weekends and might have diluted the results.
