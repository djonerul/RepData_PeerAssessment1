---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Here's the code for reading in the dataset and/or processing the data


```r
library(tidyverse)
```



```r
d <- read.csv("activity.csv",na.strings = NA) %>% mutate(date = as.Date(date)) %>% na.omit()
```

## Here's how the number of steps are distributed per day:

Code

```r
d$Day <- as.factor(weekdays(d$date))
d <- subset(d,steps > 0)
ggplot(d, aes(x=steps)) +
        geom_histogram() +
        facet_grid(rows = vars(Day))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Here's the mean and median number of steps taken each day


```r
aggregate(steps ~ Day,d,mean)
```

```
##         Day    steps
## 1    Friday 144.9213
## 2    Monday 131.4953
## 3  Saturday 141.9871
## 4    Sunday 128.4664
## 5  Thursday 126.8378
## 6   Tuesday 120.7586
## 7 Wednesday 145.1169
```

```r
aggregate(steps ~ Day,d,median)
```

```
##         Day steps
## 1    Friday  55.0
## 2    Monday  51.0
## 3  Saturday  60.0
## 4    Sunday  60.0
## 5  Thursday  47.0
## 6   Tuesday  56.0
## 7 Wednesday  61.5
```


## Here's the average number of steps taken over time
Code

```r
c <- aggregate(steps ~ date,d,mean)
ggplot(c,aes(x=date,y=steps)) +
        geom_line() + geom_point()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## Here's the interval in which the highest number of steps were taken


```r
e <- aggregate(steps ~ interval,d,mean)
e <- e[order(e$steps, decreasing = T),]
e[1,]
```

```
##    interval    steps
## 86      835 352.4839
```



## Here's how the data was imputed in order for there to be values replacing the NAs


```r
library(plyr)
library(tidyverse)
```



```r
d <- read.csv("activity.csv") %>% mutate(date = as.Date(date))

grpmean <- function(x) replace(x,is.na(x),mean(x,na.rm = T))

d2 <- ddply(d,~interval,transform,steps=grpmean(steps))

d2 <- d2[order(d2$date),]
```

## Using the imputed dat, here's the distribution of the number of steps done per day.

Code

```r
d2$Day <- as.factor(weekdays(d2$date))
d2 <- subset(d2,steps > 0)
ggplot(d2, aes(x=steps)) +
        geom_histogram() +
        facet_grid(rows = vars(Day))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


## Here's the comparison on the activities done during the weekend versus the weekdays

Code

```r
d2$WkEnd <- grepl("S.+",weekdays(d2$date))
c2 <- aggregate(steps~interval+WkEnd,d2,mean)
c2$WkEnd <- factor(c2$WkEnd,levels = c(FALSE,TRUE),labels = c("Weekends","Weekdays"))
ggplot(c2,aes(x=interval,y=steps,group=WkEnd)) +
        geom_line() +
        facet_grid(cols = vars(WkEnd))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

