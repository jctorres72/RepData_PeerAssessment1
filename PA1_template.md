---
title: "RRCP1"
author: "JuanCarlos Torres"
date: "30/7/2021"
output: 
  html_document:
  toc: yes
  toc_depth: 2
---

# INTRO
Reproducble Research Course Project 1

# DATA
For this work, the file: activity.csv provided as part of the task

## Loading and preprocessing the data
Loading and preprocessing the data
Show any code that is needed to
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

## input data
```{r}
library(ggplot2)
library(dplyr)

#Read the activity file
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r}
#Steps per day
StepsPerDay <- aggregate(data$steps, list(data$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay

#Histogram of the total numer of steps
ggplot(StepsPerDay, aes(Steps)) + geom_histogram(binwidth = 500) +
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")
  

#Mean and median of the total number of steps per day
  Xmean = mean(StepsPerDay$Steps, na.rm=TRUE) 
  Ymedian = median(StepsPerDay$Steps, na.rm=TRUE)
  Xmean
  Ymedian
```
  
## What is the average daily activity pattern?
```{r}
interval_steps <- data %>% 
group_by(interval) %>%
summarise(steps = mean(steps, na.rm =TRUE))

# Make a histogram of the total number of steps taken each day

ggplot(data=interval_steps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute intervals") +
    ylab("Average number of steps taken")
```    


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}

#Max average steps
  max(interval_steps$steps)

#Time for max average
  interval_steps[which.max(interval_steps$steps), ]
```

## Imputing missing values
```{r}
  sum(is.na(data))
  
  imputedData <- data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ interval_steps$steps[match(data$interval, interval_steps$interval)],      
      TRUE ~ as.numeric(steps)
    ))
```    

## Make a new histogram of the total number of steps taken each day
```{r}
imputedTotalSteps <- imputedData %>% group_by(date) %>% summarise(daily_steps = sum(steps))

ggplot(imputedTotalSteps, aes(daily_steps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")

imputedTotalSteps
```

## Are there differences in activity patterns between weekdays and weekends?
```{r}
library(lubridate)
dWeek <- imputedData %>%
  mutate(
    date = ymd(date),
    wDayEnd = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                   wday(date) %in% c(1,7) ~ "Weekend")
  ) %>% select(-date) %>%
  group_by(interval, wDayEnd) %>%
  summarise(
    steps = mean(steps)
  )
ggplot(dWeek, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~wDayEnd, nrow = 2) +
  xlab("5-Minute intervals") + 
  ylab("Average number of steps")
```
  
