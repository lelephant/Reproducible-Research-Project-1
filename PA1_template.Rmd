---
title: "Reproducible Research Project 1"
author: "Lucas Fernandez"
date: "January 20, 2016"
output: html_document
---

This is an R markdown document containing the code and descriptions of the code used to create the necessary graphs for Project 1

First we load in necessary packages to use throughout the assignment.
```{r, results="hide"}
library(dplyr)
library(ggplot2)
library(lubridate)
```

**Code for reading in the dataset and/or processing the data**
Here we use the basic read.csv function to read in our data.

```{r}
dataset<- read.csv("activity.csv")
```

**Histogram of the total number of steps taken each day**
First we calculate the total number of steps taken each day.

```{r}
days<-dataset$date
steps<- dataset$steps
stepsperday<- aggregate(steps, list(days), FUN=sum)
head(stepsperday)
```
Then we create a histogram showing the frequency with which any number of step per day are taken.
```{r}
hist(stepsperday$x, xlab = "Steps Per Day", main = "Histogram of Frequency of Steps per Day")
```
**Mean and median number of steps taken each day**
```{r}
mean(stepsperday$x, na.rm = TRUE)
median(stepsperday$x, na.rm = TRUE)
```
**Time series plot of the average number of steps taken**
Here we find the average number of steps taken per interval, across all 60 days.
```{r}
avgstepsperint <- dataset %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(steps = mean(steps))
```
Then we plot this data using ggplot2.
```{r}
ggplot(avgstepsperint, aes(interval, steps)) + geom_line() + ggtitle("Average Steps per Inteveral")
```

**The 5-minute interval that, on average, contains the maximum number of steps**
Here we proceed to find the maximum value of steps in our variable avgstepsperint variable using the max() function...
```{r}
max(avgstepsperint$steps)
```
...we use which() and grepl() to discover the row it occurs at...
```{r}
which(grepl(206.1698, avgstepsperint$steps))
```
...and print the row.
```{r}
avgstepsperint[104,]
```

**Code to describe and show a strategy for imputing missing data**
First we check how many missing values are in the data set. It turns out this is 8 complete days of missing data.
```{r}
missing<-is.na(dataset$steps)
sum(missing)

(sum(missing))/(length(avgstepsperint$steps))
```
Then we create a copy of the dataset called complete where we fill in the missing values. This is done by checking for Na's and matching them by interval with our average steps per interval. This creates a dataset where all the missing values have been replaces with the average number of steps for that interval.
```{r}
complete<-dataset
complete$steps<- ifelse(is.na(complete$steps),avgstepsperint$steps[match(complete$interval,avgstepsperint$interval)],complete$steps)

```

I also stumbled upon the fact that this works even without matching missing values by interval with the exact same result. I have no idea why this works.

```{r}
test<-dataset
test$steps[which(is.na(test$steps))]<-avgstepsperint$steps
```

Proving that these two methods have the same result.

```{r}
sum(complete!=test)
```

***Histogram of the total number of steps taken each day after missing values are imputed***

Then we create a histogram showing the sum of steps per day.
```{r}
fullstepsperday<- aggregate(complete$steps, list(complete$date), FUN=sum)
hist(fullstepsperday$x, xlab = "Steps Per Day", main = "Histogram of Frequency of Steps per Day")
```

Then compute the mean and median values of our steps per day.

```{r}
mean(fullstepsperday$x)
median(fullstepsperday$x)
```

This does not change appreciably from our previous estimates, as we excluded our NA value's from those calculations, and simply replaced them with average values here.

***Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends***
Turn the dates into a factor of Weekend and Weekday and append it to our dataset.
```{r}
dates<- as.Date(complete$date)
days<-weekdays(dates)
days<-ifelse(grepl("Saturday", days), "Weekend", days)
days<-ifelse(grepl("Sunday", days), "Weekend", days)
days<-ifelse(grepl("Monday", days), "Weekday", days)
days<-ifelse(grepl("Tuesday", days), "Weekday", days)
days<-ifelse(grepl("Wednesday", days), "Weekday", days)
days<-ifelse(grepl("Thursday", days), "Weekday", days)
days<-ifelse(grepl("Friday", days), "Weekday", days)
days<- as.factor(days)
complete<-cbind(complete,days)
```
Then we use this information to create a panel plot showing average steps per interval and weekends and weekdays.
```{r}
intbydaytype <- complete %>% group_by(interval, days) %>% summarise(steps = mean(steps))
ggplot(intbydaytype, aes(x=interval, y=steps, color = days)) + geom_line() + facet_wrap(~days, ncol = 1, nrow=2)
```
We see here that on weekdays most of the steps are taken in the beginning of the day, while activity start later on weekends, and is more spread out.