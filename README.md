---
title: "Reproducible Research Course Project 1"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
```

## 1. Loading and preprocessing the data

### 1.1 Load the data

```{r}
activity <- read.csv(unz("repdata_data_activity.zip", "activity.csv"))
```

### 1.2 Process/transform the data

```{r}
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## 2. What is mean total number of steps taken per day?

### 2.1 Calculate the total number of steps taken per day

```{r}
steps_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
```

### 2.2 Make a histogram of the total number of steps taken each day

```{r}
hist(steps_day$steps, main =  "Total Number of Steps per Day", xlab = "Range of Steps per Day", ylim = c(0, 25), breaks = 10)
```

### 2.3 Calculate and report the mean and median of the total number of steps taken per day

Use the summary() function to calcuate the mean and median of the total number of steps at once.

```{r}
summary(steps_day$steps)
```

## 3. What is the average daily activity pattern?

### 3.1 Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
plot(aggregate(steps ~ interval, data = activity, FUN = mean), type = "l", xlab = "5-minute Intervals", ylab = "Average Number of Steps", main = "Average Steps within Intervals")
```

### 3.2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max(activity$steps, na.rm = TRUE)
```

### 4. Inputing missing values

### 4.1 Calculate and report the total number of missing values in the dataset

```{r}
sum(is.na(activity))
```

### 4.2 Devise a strategy for filling in all of the missing values in the dataset. 

The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I am going to subsitute each NA with the median value of acitivity$steps for that day.

### 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activity_impute <- activity
na <- is.na(activity_impute$steps)
int_avg <- tapply(activity_impute$steps, activity_impute$interval, mean, na.rm = TRUE)
activity_impute$steps[na] <- int_avg[as.character(activity_impute$interval[na])]
```

### 4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
steps_day_impute <- aggregate(steps ~ date, rm.na = TRUE, data = activity_impute, FUN = sum)
par(mfrow = c(1, 2))
hist(steps_day$steps, main =  "Total Number of Steps per Day", cex.main = 0.9, xlab = "Range of Steps per Day", ylim = c(0, 25), breaks = 10)
hist(steps_day_impute$steps, main =  "Total Number of Steps per Day without NAs", cex.main = 0.9, xlab = "Range of Steps per Day",  ylim = c(0, 25), breaks = 10)
```

Calculate the new mean:
```{r}
mean(steps_day_impute$steps)
```

Calculate the new median:
```{r}
median(steps_day_impute$steps)
```

So the new mean and median are the same, which is 10767. The new mean is the same as the old mean, while the new median is slightly higher than the old median. By comparing the histgrams, the only impact of imputing missing values is the higher frequency for the range of steps "10000 - 12500".

## 5. Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 5.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
#The output of weekdays is shown in my local language Swedish. lördag = Saturday; söndag = Sunday. Change the code according to your local language.
activity_impute$day  <- ifelse(weekdays(activity_impute$date) %in% c("lördag", "söndag"), "weekend", "weekday")
activity_impute$day  <- as.factor(activity_impute$day)
```

### 5.2. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
par(mfrow = c(2, 1))
with(activity_impute[activity_impute$day == "weekend",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekends"))
with(activity_impute[activity_impute$day == "weekday",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekdays"))
```
