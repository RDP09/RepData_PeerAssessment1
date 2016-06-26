# Reproducible Research Project # 1

 Ryan Phillips-
 June 6th, 2016
 
#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Data

The data for this assignment can be downloaded from the course web site:

    Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

Loading and preprocessing the data:

  Load the data:
      

```r
activitydata <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
head(activitydata)
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

Process/transform the data into a format suitable for your analysis:
     Not required for this assignment

###What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day:

  1.Compute total number of steps taken:

```r
 TotalSteps <- tapply(activitydata$steps, activitydata$date, sum)
```
  
  2. Plot histogram:

```r
hist(TotalSteps, col = "red", xlab =  "Total # Steps Taken per Day", ylab = "Daily Frequency",main = "Histogram of Total # of Steps taken per Day")
```

![](Reproducibleresearch_files/figure-html/unnamed-chunk-3-1.png)

Calculate and report the mean and median total number of steps taken per day:

Mean:

```r
mean(TotalSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

Median:

```r
median(TotalSteps,na.rm = TRUE)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

  1. Compute Mean Steps by time interval
  

```r
MeanSteps <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
```

  2.Timeseries plot of of the 5-minute interval and the average number of steps taken, averaged across all days
     

```r
plot(row.names(MeanSteps), MeanSteps, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "red")
```

![](Reproducibleresearch_files/figure-html/unnamed-chunk-7-1.png)

    
    
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- which.max(MeanSteps)
names(max_interval)
```

```
## [1] "835"
```



###Imputing missing values


  1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  

```r
numbernavalues <- sum(is.na(activitydata))
numbernavalues
```

```
## [1] 2304
```

  2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.    3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

   Using Average Interval Value for all days:

```r
na_indices <- which(is.na(activitydata))
imputed_values <- MeanSteps[as.character(activitydata[na_indices, 3])]
names(imputed_values) <- na_indices  
for (i in na_indices) {
    activitydata$steps[i] = imputed_values[as.character(i)]
}
```
   Check:

```r
sum(is.na(activitydata))
```

```
## [1] 0
```

 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalsteps <- tapply(activitydata$steps, activitydata$date, sum)
hist(totalsteps, col = "red", xlab = "Total Steps per Day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken per day")
```

![](Reproducibleresearch_files/figure-html/unnamed-chunk-12-1.png)

New Mean:

```r
mean(totalsteps)
```

```
## [1] 10766.19
```
 Same as previously

New Median:

```r
median(totalsteps)
```

```
## [1] 10766.19
```
 Slightly higher than previous median

Effect on the total daily number of steps:

```r
sum(totalsteps) - sum(TotalSteps,na.rm = TRUE)
```

```
## [1] 86129.51
```

###Are there differences in activity patterns between weekdays and weekends?

  1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
  

```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activitydata$daytype <- as.factor(sapply(activitydata$date, daytype))
```

 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
 

```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activitydata, subset = activitydata$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![](Reproducibleresearch_files/figure-html/unnamed-chunk-17-1.png)
 

