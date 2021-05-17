---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

In order to load the data, we first have to unzip it.

```r
unzip('activity.zip')
df <- read.csv('activity.csv', header = T)
```

The date column is `character`, let's convert it into `date` format.

```r
df$date <- as.Date(as.character(df$date, format = '%Y%m%d'))
head(df)
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


## What is mean total number of steps taken per day?

First let's generate the data frame to steps per day. I use `aggregate` function for this purpose.


```r
stepsPerDay <- aggregate(steps~date, data = df, sum)
head(stepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### 1. Draw a histogram of the total steps per day


```r
if(!dir.exists('figures')) dir.create('figures')
hist(stepsPerDay$steps, main = 'Total steps per day'
     , xlab = 'Steps', col = 'yellow', border = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, file = 'figures/1.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### 2. Mean and median of total steps per day

We ignore the missing values, as the instructions mention.


```r
old_mean <- mean(stepsPerDay$steps, na.rm = T)
old_mean
```

```
## [1] 10766.19
```


```r
old_median <- median(stepsPerDay$steps, na.rm = T)
old_median
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Let's generate the data frame for average steps (in all days) for each interval by `aggregate`.


```r
stepsPerInterval <- aggregate(steps~interval, df, mean)
head(stepsPerInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```


### 1. Make a time series plot of internal ~ average of steps


```r
with(stepsPerInterval, plot(interval, steps, type = 'l'
                , col = 'blue'
                , main = 'Average steps (all days) ~ interval'
                , xlab = '5-minute interval'
                , ylab = 'Average steps'))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
dev.copy(png, file = 'figures/2.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### 2. Which interval which maximum average steps?

We use `which.max` to find the index of the interval with maximum average steps, and then use the index to find the interval itself.


```r
maxIndex <- which.max(stepsPerInterval$steps)
stepsPerInterval$interval[maxIndex]
```

```
## [1] 835
```


## Imputing missing values

### 1. Total number of missing values

`is.na` will return a logical value, and if use `sum`, we can calculate the number of missing values.


```r
colSums(is.na(df))
```

```
##    steps     date interval 
##     2304        0        0
```
We can also calculate the proportion of missing values by `mean`.

```r
colMeans(is.na(df))
```

```
##     steps      date  interval 
## 0.1311475 0.0000000 0.0000000
```

### 2. Strategy for filling in the missing values

First, let's find the index for missing values.


```r
missing <- which(is.na(df$steps))
head(df[missing, ])
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

There are 4 possible strategies for a missing value:
1. Data available on both date and interval: use mean of both
2. Data available on date, but not interval: use date
3. Data available on interval, but not date: use interval
4. Data available on neither date, nor interval: use global mean

Let's see which strategies apply to our missing values. First we take a look at dates:



```r
missingDates <- unique(df$date[missing])
head(unique(subset(df, date %in% missingDates)$steps))
```

```
## [1] NA
```
It seems all the data is missing (no value other than NA), so strategy 1 and 2 are ruled out.

How about the intervals?


```r
missingIntervals <- unique(df$interval[missing])
head(unique(subset(df, interval %in% missingIntervals)$steps))
```

```
## [1]  NA   0 117   9   4  36
```
Well, we do have quite a variety of values (strategy 3). But let's see where there are intervals for which we have no value (strategy 4). 


```r
sum(tapply(df$steps, df$interval, function(x) any(is.na(x) & all(is.na(x)))))
```

```
## [1] 0
```
The logical statement I used is `any(is.na) & all(is.na)`, which means if there is an interval for which we have missing values, then all values of that intervals should be missing.

Since the sum is 0, that means we have interval data for all of missing data. Thus, strategy 4 is also ruled out and we should go with strategies 3.

### 3. New equivalent dataset with the missing values filled in

First, let's create a new dataset by merging `df` and `stepsPerInterval`.

```r
new_df <- merge(df, stepsPerInterval, by = 'interval')
head(new_df)
```

```
##   interval steps.x       date  steps.y
## 1        0      NA 2012-10-01 1.716981
## 2        0       0 2012-11-23 1.716981
## 3        0       0 2012-10-28 1.716981
## 4        0       0 2012-11-06 1.716981
## 5        0       0 2012-11-24 1.716981
## 6        0       0 2012-11-15 1.716981
```
The missing values in `steps.x` will be replaced by `steps.y`, and the existing values will remain intact.


```r
new_df$steps <- with(new_df, ifelse(is.na(steps.x), round(steps.y), round(steps.x)))
head(new_df)
```

```
##   interval steps.x       date  steps.y steps
## 1        0      NA 2012-10-01 1.716981     2
## 2        0       0 2012-11-23 1.716981     0
## 3        0       0 2012-10-28 1.716981     0
## 4        0       0 2012-11-06 1.716981     0
## 5        0       0 2012-11-24 1.716981     0
## 6        0       0 2012-11-15 1.716981     0
```

Now the extra columns will be removed.


```r
new_df <- new_df[, c('steps', 'date', 'interval')]
head(new_df)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-11-23        0
## 3     0 2012-10-28        0
## 4     0 2012-11-06        0
## 5     0 2012-11-24        0
## 6     0 2012-11-15        0
```


### 4. Histogram of total steps per day -- compare with previous parts

The process is similar to part 1.

We aggregate steps per day.


```r
new_stepsPerDay <- aggregate(steps~date, data = new_df, sum)
```
We plot the histogram.

```r
hist(new_stepsPerDay$steps, main = 'Total steps per day'
     , xlab = 'Steps', col = 'yellow', border = 'blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
dev.copy(png, file = 'figures/3.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


We calculate the mean/median.

```r
new_mean <- mean(new_stepsPerDay$steps)
new_median <- median(new_stepsPerDay$steps)
old_mean
```

```
## [1] 10766.19
```

```r
old_median 
```

```
## [1] 10765
```

We compare the new values to the old ones.


```r
(old_mean - new_mean) / old_mean
```

```
## [1] 5.102409e-05
```

```r
(old_median - new_median) / old_median
```

```
## [1] 0.0002786809
```

The different is negligible


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create new factor variable -- 'weekday' and 'weekend'

We first use `weekdays` to get the days from `date` column, then create a vector called `weekends` and factor the weekdays based on that.


```r
weekends <- c('Saturday' ,'Sunday')
new_df$dayType <-factor(weekdays(new_df$date) %in% weekends, levels = c(TRUE, FALSE), labels = c('weekend', 'weekday'))
head(new_df)
```

```
##   steps       date interval dayType
## 1     2 2012-10-01        0 weekday
## 2     0 2012-11-23        0 weekday
## 3     0 2012-10-28        0 weekend
## 4     0 2012-11-06        0 weekday
## 5     0 2012-11-24        0 weekend
## 6     0 2012-11-15        0 weekday
```


### 2. Panel plot for interval ~ steps for factors

Similar to previous parts, we use `aggregate` to get mean steps per internal averaged on different types.


```r
new_stepsPerInterval <- aggregate(steps ~ interval + dayType, new_df, mean)
head(new_stepsPerInterval)
```

```
##   interval dayType steps
## 1        0 weekend  0.25
## 2        5 weekend  0.00
## 3       10 weekend  0.00
## 4       15 weekend  0.00
## 5       20 weekend  0.00
## 6       25 weekend  3.50
```

Now we will plot the time series panels for weekdays and weekends using `ggplot2`. We use facets for dayTypes and `legend.position` to hide the legend.


```r
library(ggplot2)
qplot(interval, steps, data = new_stepsPerInterval, facets = dayType ~ .
      , color = dayType
      , geom = 'line'
      , main = 'Average steps (all days) ~ interval + dayType'
      , xlab = '5-minute interval'
      , ylab = 'Average steps') + 
  theme(legend.position = 'none')
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
dev.copy(png, file = 'figures/4.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


