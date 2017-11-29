---
title: "Peer assignment 1 week 2 RR"
author: "Jeanne Mason"
date: "11/28/2017"
output: 
  html_document: 
    keep_md: yes
---
## Peer Assignment for Week 2 of Reproducible Research
The first objective is to read in a dataset from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


##Loading and preprocessing the data
Once the data is read in, needed libraries are loaded and average, median and sum of steps by day are calculated

```r
setwd("/Users/Jeanne/Rwork/Data Science/Reproducible research/wk2")
#read in the dataset
active<-read.csv("activity.csv")

# What is mean total number of steps taken per day?

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(stats)

# make the date a Date Object, calculate summary of the total steps in each day
g_active<- active %>% mutate(realdate=as.Date(date, format='%Y-%m-%d' )) %>% 
        group_by(realdate) %>% 
        summarise(avg=mean(steps), summy=sum(steps), med=median(steps, na.rm=TRUE))
```
Next a histogram is plotted of the total steps per day

```r
#    Make a histogram of the total number of steps taken each day
a <- ggplot(g_active, aes(summy)) +
        geom_histogram(aes(x=summy ), binwidth=500) +
                        xlab("total daily steps") +
                        scale_y_continuous("number of days with total steps",
                                limits = c(0,8)) + theme_classic() +
        ggtitle("frequency the subject walked a given number of steps per day")
a
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template..Rmd_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


##What is mean total number of steps taken per day?

```r
 av<-(mean(g_active$summy, na.rm=TRUE))

 
 md<-median(g_active$summy, na.rm = TRUE)
```

The mean total steps taken per day is 1.0766189\times 10^{4}
The median total number of steps taken per day is 10765


##What is the average daily activity pattern?
#Investigating the average daily activity pattern


```r
g2_active<- active %>% group_by(interval) %>% 
        summarise(avg=mean(steps, na.rm=TRUE))

gg1<-ggplot(g2_active, aes(x=interval, y=avg)) + geom_point() +
       scale_x_continuous("interval of the day recorded steps") +
        scale_y_continuous("average number of steps during interval") +
        ggtitle("Trends in Subject's Daily Activity in 5 Minute Intervals")
gg1
```

![](PA1_template..Rmd_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
max_int<-max(g2_active$avg)


#   Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)  


missing<-nrow(active)-sum(complete.cases(active))
```
The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps is 206.1698113


The total number of missing values in the dataset is 2304

##Imputing missing values
There are a lot of NA's in the dataset. The strategy I devised was to use the mean steps for the 5 minute interval across all days, for any missing steps. I did not assume that zeros represented missing values, because it was possible the subject did not take a step in a given interval.  The new dataset is called trans_active.


```r
g3_active<-active %>% mutate(realdate=as.Date(date, format='%Y-%m-%d' )) %>%
        group_by(interval) %>% mutate(intavg=mean(steps, na.rm=TRUE))

trans_active = transform(g3_active, steps = ifelse(is.na(steps), intavg, steps))
```
Next a histogram of the total number of steps taken each day, is calculated

```r
transg_active<- trans_active %>% group_by(realdate) %>% 
        summarise(transsum=sum(steps)) 

gg2 <- ggplot(transg_active, aes(transsum)) +
        geom_histogram(aes(x=transsum ), binwidth=500) +
                        xlab("total daily steps") +
                        scale_y_continuous("number of days with total steps",
                                limits = c(0,8)) + theme_classic() +
       ggtitle( "frequency the subject walked a given number of steps per day",
                subtitle = "missing data imputed from avg for 5 min interval")
        
gg2
```

```
## Warning: Removed 1 rows containing missing values (geom_bar).
```

![](PA1_template..Rmd_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
#quantile with missing values removed
quantile(g_active$summy, na.rm=TRUE)
```

```
##    0%   25%   50%   75%  100% 
##    41  8841 10765 13294 21194
```

```r
#quantile with missing values imputed
quantile(transg_active$transsum)
```

```
##       0%      25%      50%      75%     100% 
##    41.00  9819.00 10766.19 12811.00 21194.00
```

```r
t_av<-(mean(transg_active$transsum, na.rm=TRUE))
 av
```

```
## [1] 10766.19
```

```r
t_md<-median(transg_active$transsum, na.rm = TRUE)
 md
```

```
## [1] 10765
```

#The impact of imputing the data
From the dataset with imputed values:
The mean total steps taken per day is 1.0766189\times 10^{4}
The median total number of steps taken per day is 1.0766189\times 10^{4}, so no difference for the mean and median, the differences appear in the 25% and 75% quantiles, above


##Are there differences in activity patterns between weekdays and weekends?


#Comparing weekday and weekend


```r
library(timeDate)

namedays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
trans_active$wd <- factor((weekdays(trans_active$realdate) %in% namedays), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') )

trans_weekday<-trans_active %>% filter( wd=="weekday") %>%
        group_by(interval) %>% summarise(avg=mean(steps, na.rm=TRUE))
trans_weekend<-trans_active %>% filter(wd=="weekend") %>%
        group_by(interval) %>% summarise(avg=mean(steps, na.rm=TRUE))

par(mfrow=c(2,1))
## 
## top graph 
plot(trans_weekday$interval, trans_weekday$avg ,xlab="", 
     ylab="average steps on weekdays", type="l")
## top right graph - 
plot(trans_weekend$interval, trans_weekend$avg, 
     xlab="5 Minute Interval during a Day", 
     ylab="average steps on weekends", type ="l")
```

![](PA1_template..Rmd_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

a lot more activity throughout the whole day on the weekend compared to the weekday.
