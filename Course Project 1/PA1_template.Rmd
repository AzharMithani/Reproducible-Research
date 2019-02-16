---
title: "Reproducible Research - Course Project 1"
author: "Gabriel Verta"
date: "March 6, 2016"
output: html_document
---

To parse dates we're going to use lubridate, dplyr to parse data, and to plot some graphs ggplot2

```{r, echo=TRUE}

library(dplyr)
library(lubridate)
library(ggplot2)
```

Reading the data, and parsing dates. Grouping activities per day

```{r, echo=TRUE}
activities <- read.csv("activity.csv")
activities$date <- ymd(activities$date)

activities_per_day <- group_by(activities, date)
activities_per_interval <- group_by(activities, interval)
```


Histogram of the total number of steps taken each day:

```{r, echo=TRUE}
items <- tapply(activities$steps, activities$date, FUN=sum)
barplot(items, las=2, cex.names = 0.5)
```

Mean and median number of steps taken each day. Time series plot of the average number of steps taken.

```{r, echo=TRUE}
mean_and_median <- summarise(activities_per_day, mean=mean(steps, na.rm = TRUE), n=median(steps, na.rm = TRUE))
plot(mean_and_median$mean, type="l", col="red", ylab = "Steps", xlab = "Day", las=2, main="Average number of steps taken")
lines(mean_and_median$median, type="l", col="blue")
legend("topright", legend=c("mean", "median"), pch=1, col=c("red", "blue"))
```

The 5-minute interval that, on average, contains the maximum number of steps
```{r, echo=TRUE}
average_of_steps_by_interval <- summarise(activities_per_interval, average=mean(steps, na.rm = TRUE))
ranking_of_intervals <- arrange(average_of_steps_by_interval, desc(average))
print(ranking_of_intervals$interval[1])
```

Code to describe and show a strategy for imputing missing data

- Total number of missing values in the dataset
```{r, echo=TRUE}
nas <- is.na(activities$steps)
total_empty <- sum(nas)
percentage_empty <- mean(nas)
# Total empty results
print(total_empty)

# Percentage
print(paste(sprintf("%.2f", percentage_empty * 100), "%", sep=""))
```

- Using average of that 5-minute to populate NAs
```{r, echo=TRUE}
not_empty <- mutate(activities)
na_intervals <- not_empty$interval[nas]
average_na_intervals <- sapply(na_intervals, function(item) {average_of_steps_by_interval$average[average_of_steps_by_interval$interval==item]})
not_empty$steps[nas] = average_na_intervals
#average_of_steps_by_interval$average[average_of_steps_by_interval$interval==0]
```


Histogram of the total number of steps taken each day after missing values are imputed
```{r, echo=TRUE}
not_empty_items <- tapply(not_empty$steps, not_empty$date, FUN=sum)

par(mfrow=c(2,1))
barplot(items, las=2, cex.names = 0.5, main="Histogram with missing values (NAs)")
barplot(not_empty_items, las=2, cex.names = 0.5, main="Histogram without missing values (NAs)")
```

As we can see, missing values can change the final result of an analysis.

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo=TRUE}
activities_with_days <- mutate(activities, weekday=weekdays(date))
activities_by_day_interval <- group_by(activities_with_days, weekday, interval)

results_per_day <- summarise(activities_by_day_interval, steps=mean(steps, na.rm=TRUE))
qplot(interval, steps, data=results_per_day, facets= weekday ~ .) + geom_line()
```
