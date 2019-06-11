---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data


```r
cls = c("integer", "character", "integer")
df <- read.csv("activity.csv", head=TRUE, colClasses=cls, na.strings="NA")
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

```r
df$date <- as.Date(df$date)
df_ign <- subset(df, !is.na(df$steps))
```

## What is mean total number of steps taken per day?


```r
tot_steps <- sapply(FUN = sum,X = split(df$steps, df$date),na.rm = FALSE)
tot_steps <- tot_steps[!is.na(tot_steps)]
mean_steps <- mean(tot_steps,na.rm = TRUE)
median_steps <- median(tot_steps,na.rm = TRUE)
hist(tot_steps,breaks = 20,col = "red", xlab = "Daily total steps", ylab = "Frequency", main = "Distribution of total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
interval <- sapply(split(df$steps,df$interval),mean,na.rm = TRUE)
df$interval2=as.integer(df$interval)
plot(df$interval[1:(24*60/5)],interval,type = "l", xlab="5-minute intervals", ylab="average steps in the interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_steps <- max(interval)
idx <- which.max(interval)
max_steps
```

```
## [1] 206.1698
```

```r
df$interval[idx]
```

```
## [1] 835
```

## Imputing missing values


```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
df_impute <- df
ndx <- is.na(df_impute$steps)
int_avg <- tapply(df_ign$steps, df_ign$interval, mean, na.rm=TRUE, simplify=T)
df_impute$steps[ndx] <- int_avg[as.character(df_impute$interval[ndx])]

new_dailysum <- tapply(df_impute$steps, df_impute$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_dailysum,
     col="red",
     breaks=20,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(new_dailysum)
```

```
## [1] 10766.19
```

```r
median(new_dailysum)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
# helper function to decide if a day is a week day or not
is_weekday <- function(d) {
    wd <- weekdays(d)
    if (wd == "Saturday" | wd == "Sunday")
      wx <- "Weekend"
    else 
      wx <- "Weekday"
}
Sys.setlocale("LC_ALL","English") 
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
wx <- sapply(df_impute$date, is_weekday)
df_impute$wk <- as.factor(wx)
head(df_impute)
```

```
##       steps       date interval interval2      wk
## 1 1.7169811 2012-10-01        0         0 Weekday
## 2 0.3396226 2012-10-01        5         5 Weekday
## 3 0.1320755 2012-10-01       10        10 Weekday
## 4 0.1509434 2012-10-01       15        15 Weekday
## 5 0.0754717 2012-10-01       20        20 Weekday
## 6 2.0943396 2012-10-01       25        25 Weekday
```

```r
wk_df <- aggregate(steps ~ wk+interval, data=df_impute, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_df)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
