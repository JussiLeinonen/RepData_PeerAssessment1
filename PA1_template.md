### Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading and preprocessing the data

``` r
data <- read.csv("activity.csv")
data$date = as.Date(data$date, "%Y-%m-%d")
summary(data)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

``` r
str(data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
head(data)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
tail(data)
```

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

### What is mean total number of steps taken per day?

``` r
steps_daily <- aggregate(steps~date, data, sum)
plot <- ggplot(data=steps_daily, aes(date,steps))
steps_mean <- mean(steps_daily$steps)
steps_median <- median(steps_daily$steps)
plot <- plot + geom_histogram(stat = "identity") + geom_hline(yintercept=steps_mean, col = "red") + xlab("") + ylab("steps") +ggtitle("Original data")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
plot
```

![](Reprod-1_files/figure-markdown_github/daily-1.png)

Mean (steps)

``` r
steps_mean
```

    ## [1] 10766.19

Median (steps)

``` r
steps_median
```

    ## [1] 10765

### What is the average daily activity pattern?

``` r
steps_interval <- aggregate(steps~interval,data,mean)
plot2 <- ggplot(data=steps_interval,aes(interval,steps))
plot2+geom_line()
```

![](Reprod-1_files/figure-markdown_github/timeseries-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
steps_interval[which.max(steps_interval$steps),1]
```

    ## [1] 835

### Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Total number of missing values in the dataset (i.e. the total number of rows with NAs):

``` r
sum(is.na(data$steps))
```

    ## [1] 2304

Imputations to NAs are created by corresponding 5-min interval averages and the values are incorporated to form a new dataset (data\_imputed):

``` r
data_imputed <- data
NAs <- numeric()
for (i in 1:nrow(data))
{
  t <- data[i,]
  if (is.na(t$steps)) {
    s <- steps_interval[which(steps_interval$interval == t$interval),]$steps
  }
  else{
    s <- t$steps
  }
  NAs <- c(NAs, s)
}
data_imputed$steps <- NAs
```

Comparison of histograms (original vs imputed) of total number of steps taken per day shows only a small difference.

``` r
steps_imputed <- aggregate(steps~date, data_imputed, sum)
plot3 <- ggplot(data=steps_imputed, aes(date,steps))
steps_mean2 <- mean(steps_imputed$steps)
steps_median2 <- median(steps_imputed$steps)
plot3 <- plot3 + geom_histogram(stat = "identity") + geom_hline(yintercept=steps_mean2, col="blue")+ xlab("") + ylab("steps") + ggtitle("Imputed data")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
grid.arrange(plot,plot3, nrow=1)
```

![](Reprod-1_files/figure-markdown_github/comparison-1.png)

Difference of mean (original - imputed) (steps)

``` r
steps_mean-steps_mean2
```

    ## [1] 0

Difference of median (original - imputed) (steps)

``` r
steps_median-steps_mean2
```

    ## [1] -1.188679

Difference of standard deviation (original - imputed) (steps)

``` r
sd(steps_daily$steps)
```

    ## [1] 4269.18

``` r
sd(steps_daily$steps)-sd(steps_imputed$steps)
```

    ## [1] 294.7897

Mean and median values remained similar after imputations, but standard deviation decreased.

### Are there differences in activity patterns between weekdays and weekends?

First a new factor variable is created in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
week <- weekdays(data_imputed$date)
for (i in 1:length(week)) {
    if ((week[i] == "lauantai") | (week[i] == "sunnuntai")) 
    week[i] <- "weekend"  else  week[i] <- "weekday"
}
week <- as.factor(week)
summary(week)
```

    ## weekday weekend 
    ##   12960    4608

``` r
data_imputed$week <- week
data_imputed_weekday <- subset(data_imputed, week == "weekday")
data_imputed_weekend <- subset(data_imputed, week == "weekend")
```

Then the time series plots of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) are compared.

``` r
steps_weekday <- aggregate(steps~interval,data_imputed_weekday,mean)
plot4 <- ggplot(data=steps_weekday,aes(interval,steps)) +geom_line() + ggtitle("Weekday") +ylim(0,250)
steps_weekend <- aggregate(steps~interval,data_imputed_weekend,mean)
plot5 <- ggplot(data=steps_weekend,aes(interval,steps)) +geom_line() + ggtitle("Weekend") +ylim(0,250)
grid.arrange(plot4,plot5, nrow=1)
```

![](Reprod-1_files/figure-markdown_github/compare-1.png)

Activity (step count) is more evenly dispersed during the weekends than during the weekdays.
