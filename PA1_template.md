# Reproducible Research Peer Ass. 1
Bentley  
November 15, 2014  

The following analysis is conducted on observations that occurred over a period of 61 days on a single subject.
Steps were recorded continuously and summed into 5 minute time intervals.
Each time interval was recorded in the hhmm format with no padding e.g. 00:00 as 0 and 23:30 as 2330

## Loading and preprocessing the data


```r
library(lattice)
```



```r
activity<- read.csv("~/Documents/R Working Directory/Coursera/Scripts/RepResProject1/activity.csv", colClasses = "character")
activity$steps<-as.numeric(activity$steps)  ## converting the steps variable to class numeric
activity$date<-as.Date(activity$date)     ## converting the date variable to class date
activity$interval<-as.numeric(activity$interval)  ## converting the interval variable to class numeric
activitycomp<-activity[complete.cases(activity),]  ## removing the NAs
```

## What is the mean total number of steps taken per day?


```r
hist(tapply(as.numeric(activitycomp$steps),activitycomp$date,FUN=sum))
```

![](./PA1_template_files/figure-html/plottinghistogram-1.png) 

### The mean steps per day are as follows:


```r
meansteps<-aggregate(activitycomp$steps ~ activitycomp$date,FUN=mean)
print(meansteps)
```

```
##    activitycomp$date activitycomp$steps
## 1         2012-10-02          0.4375000
## 2         2012-10-03         39.4166667
## 3         2012-10-04         42.0694444
## 4         2012-10-05         46.1597222
## 5         2012-10-06         53.5416667
## 6         2012-10-07         38.2465278
## 7         2012-10-09         44.4826389
## 8         2012-10-10         34.3750000
## 9         2012-10-11         35.7777778
## 10        2012-10-12         60.3541667
## 11        2012-10-13         43.1458333
## 12        2012-10-14         52.4236111
## 13        2012-10-15         35.2048611
## 14        2012-10-16         52.3750000
## 15        2012-10-17         46.7083333
## 16        2012-10-18         34.9166667
## 17        2012-10-19         41.0729167
## 18        2012-10-20         36.0937500
## 19        2012-10-21         30.6284722
## 20        2012-10-22         46.7361111
## 21        2012-10-23         30.9652778
## 22        2012-10-24         29.0104167
## 23        2012-10-25          8.6527778
## 24        2012-10-26         23.5347222
## 25        2012-10-27         35.1354167
## 26        2012-10-28         39.7847222
## 27        2012-10-29         17.4236111
## 28        2012-10-30         34.0937500
## 29        2012-10-31         53.5208333
## 30        2012-11-02         36.8055556
## 31        2012-11-03         36.7048611
## 32        2012-11-05         36.2465278
## 33        2012-11-06         28.9375000
## 34        2012-11-07         44.7326389
## 35        2012-11-08         11.1770833
## 36        2012-11-11         43.7777778
## 37        2012-11-12         37.3784722
## 38        2012-11-13         25.4722222
## 39        2012-11-15          0.1423611
## 40        2012-11-16         18.8923611
## 41        2012-11-17         49.7881944
## 42        2012-11-18         52.4652778
## 43        2012-11-19         30.6979167
## 44        2012-11-20         15.5277778
## 45        2012-11-21         44.3993056
## 46        2012-11-22         70.9270833
## 47        2012-11-23         73.5902778
## 48        2012-11-24         50.2708333
## 49        2012-11-25         41.0902778
## 50        2012-11-26         38.7569444
## 51        2012-11-27         47.3819444
## 52        2012-11-28         35.3576389
## 53        2012-11-29         24.4687500
```

### The median steps per day are as follows:


```r
mediansteps<-aggregate(activitycomp$steps ~ activitycomp$date,FUN=median)
print(mediansteps)
```

```
##    activitycomp$date activitycomp$steps
## 1         2012-10-02                  0
## 2         2012-10-03                  0
## 3         2012-10-04                  0
## 4         2012-10-05                  0
## 5         2012-10-06                  0
## 6         2012-10-07                  0
## 7         2012-10-09                  0
## 8         2012-10-10                  0
## 9         2012-10-11                  0
## 10        2012-10-12                  0
## 11        2012-10-13                  0
## 12        2012-10-14                  0
## 13        2012-10-15                  0
## 14        2012-10-16                  0
## 15        2012-10-17                  0
## 16        2012-10-18                  0
## 17        2012-10-19                  0
## 18        2012-10-20                  0
## 19        2012-10-21                  0
## 20        2012-10-22                  0
## 21        2012-10-23                  0
## 22        2012-10-24                  0
## 23        2012-10-25                  0
## 24        2012-10-26                  0
## 25        2012-10-27                  0
## 26        2012-10-28                  0
## 27        2012-10-29                  0
## 28        2012-10-30                  0
## 29        2012-10-31                  0
## 30        2012-11-02                  0
## 31        2012-11-03                  0
## 32        2012-11-05                  0
## 33        2012-11-06                  0
## 34        2012-11-07                  0
## 35        2012-11-08                  0
## 36        2012-11-11                  0
## 37        2012-11-12                  0
## 38        2012-11-13                  0
## 39        2012-11-15                  0
## 40        2012-11-16                  0
## 41        2012-11-17                  0
## 42        2012-11-18                  0
## 43        2012-11-19                  0
## 44        2012-11-20                  0
## 45        2012-11-21                  0
## 46        2012-11-22                  0
## 47        2012-11-23                  0
## 48        2012-11-24                  0
## 49        2012-11-25                  0
## 50        2012-11-26                  0
## 51        2012-11-27                  0
## 52        2012-11-28                  0
## 53        2012-11-29                  0
```

## What is the average daily activity pattern?


```r
timeseries<-aggregate(activitycomp$steps ~ activitycomp$interval,FUN=mean)
colnames(timeseries)<-c('interval','meansteps') # naming the columns
plot(timeseries$interval,timeseries$meansteps,type="l")
```

![](./PA1_template_files/figure-html/timeseriesplot-1.png) 


```r
maxinterval <-timeseries[timeseries$meansteps==max(timeseries$meansteps),1]
```
The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835 .

## Imputing missing values


```r
rowswithmissingvals<-nrow(subset(activity, is.na(activity$steps)))  ##calculating the total number of missing values
```

There are 'r rowswithmissingvals` rows with missing valus in the data set.


```r
## Replacing NAs with mean by 5 min interval across all days
activityimputed<-merge(timeseries,activity, by.x="interval",by.y="interval",all.y)
activityimputed[is.na(activityimputed$steps),]$steps<-activityimputed[is.na(activityimputed$steps),]$meansteps
```


```r
##histogram with imputed values
activityimputed_day<-aggregate(activityimputed$steps ~ activityimputed$date,FUN=sum)
colnames(activityimputed_day)<-c("Date","TotalSteps")
hist(activityimputed_day$TotalSteps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

### The mean steps per day after imputing values are as follows:


```r
meansteps<-aggregate(activityimputed$steps ~ activityimputed$date,FUN=mean)
print(meansteps)
```

```
##    activityimputed$date activityimputed$steps
## 1            2012-10-01            37.3825996
## 2            2012-10-02             0.4375000
## 3            2012-10-03            39.4166667
## 4            2012-10-04            42.0694444
## 5            2012-10-05            46.1597222
## 6            2012-10-06            53.5416667
## 7            2012-10-07            38.2465278
## 8            2012-10-08            37.3825996
## 9            2012-10-09            44.4826389
## 10           2012-10-10            34.3750000
## 11           2012-10-11            35.7777778
## 12           2012-10-12            60.3541667
## 13           2012-10-13            43.1458333
## 14           2012-10-14            52.4236111
## 15           2012-10-15            35.2048611
## 16           2012-10-16            52.3750000
## 17           2012-10-17            46.7083333
## 18           2012-10-18            34.9166667
## 19           2012-10-19            41.0729167
## 20           2012-10-20            36.0937500
## 21           2012-10-21            30.6284722
## 22           2012-10-22            46.7361111
## 23           2012-10-23            30.9652778
## 24           2012-10-24            29.0104167
## 25           2012-10-25             8.6527778
## 26           2012-10-26            23.5347222
## 27           2012-10-27            35.1354167
## 28           2012-10-28            39.7847222
## 29           2012-10-29            17.4236111
## 30           2012-10-30            34.0937500
## 31           2012-10-31            53.5208333
## 32           2012-11-01            37.3825996
## 33           2012-11-02            36.8055556
## 34           2012-11-03            36.7048611
## 35           2012-11-04            37.3825996
## 36           2012-11-05            36.2465278
## 37           2012-11-06            28.9375000
## 38           2012-11-07            44.7326389
## 39           2012-11-08            11.1770833
## 40           2012-11-09            37.3825996
## 41           2012-11-10            37.3825996
## 42           2012-11-11            43.7777778
## 43           2012-11-12            37.3784722
## 44           2012-11-13            25.4722222
## 45           2012-11-14            37.3825996
## 46           2012-11-15             0.1423611
## 47           2012-11-16            18.8923611
## 48           2012-11-17            49.7881944
## 49           2012-11-18            52.4652778
## 50           2012-11-19            30.6979167
## 51           2012-11-20            15.5277778
## 52           2012-11-21            44.3993056
## 53           2012-11-22            70.9270833
## 54           2012-11-23            73.5902778
## 55           2012-11-24            50.2708333
## 56           2012-11-25            41.0902778
## 57           2012-11-26            38.7569444
## 58           2012-11-27            47.3819444
## 59           2012-11-28            35.3576389
## 60           2012-11-29            24.4687500
## 61           2012-11-30            37.3825996
```

### The median steps per day after imputing values are as follows:


```r
mediansteps<-aggregate(activityimputed$steps ~ activityimputed$date,FUN=median)
print(mediansteps)
```

```
##    activityimputed$date activityimputed$steps
## 1            2012-10-01              34.11321
## 2            2012-10-02               0.00000
## 3            2012-10-03               0.00000
## 4            2012-10-04               0.00000
## 5            2012-10-05               0.00000
## 6            2012-10-06               0.00000
## 7            2012-10-07               0.00000
## 8            2012-10-08              34.11321
## 9            2012-10-09               0.00000
## 10           2012-10-10               0.00000
## 11           2012-10-11               0.00000
## 12           2012-10-12               0.00000
## 13           2012-10-13               0.00000
## 14           2012-10-14               0.00000
## 15           2012-10-15               0.00000
## 16           2012-10-16               0.00000
## 17           2012-10-17               0.00000
## 18           2012-10-18               0.00000
## 19           2012-10-19               0.00000
## 20           2012-10-20               0.00000
## 21           2012-10-21               0.00000
## 22           2012-10-22               0.00000
## 23           2012-10-23               0.00000
## 24           2012-10-24               0.00000
## 25           2012-10-25               0.00000
## 26           2012-10-26               0.00000
## 27           2012-10-27               0.00000
## 28           2012-10-28               0.00000
## 29           2012-10-29               0.00000
## 30           2012-10-30               0.00000
## 31           2012-10-31               0.00000
## 32           2012-11-01              34.11321
## 33           2012-11-02               0.00000
## 34           2012-11-03               0.00000
## 35           2012-11-04              34.11321
## 36           2012-11-05               0.00000
## 37           2012-11-06               0.00000
## 38           2012-11-07               0.00000
## 39           2012-11-08               0.00000
## 40           2012-11-09              34.11321
## 41           2012-11-10              34.11321
## 42           2012-11-11               0.00000
## 43           2012-11-12               0.00000
## 44           2012-11-13               0.00000
## 45           2012-11-14              34.11321
## 46           2012-11-15               0.00000
## 47           2012-11-16               0.00000
## 48           2012-11-17               0.00000
## 49           2012-11-18               0.00000
## 50           2012-11-19               0.00000
## 51           2012-11-20               0.00000
## 52           2012-11-21               0.00000
## 53           2012-11-22               0.00000
## 54           2012-11-23               0.00000
## 55           2012-11-24               0.00000
## 56           2012-11-25               0.00000
## 57           2012-11-26               0.00000
## 58           2012-11-27               0.00000
## 59           2012-11-28               0.00000
## 60           2012-11-29               0.00000
## 61           2012-11-30              34.11321
```

The impact of imputing the is that it raises the mean and median values for those days that there were significant NA observations

## Are there differences in activity patterns between weekdays and weekends?


```r
activityimputed$type<-paste("weekday")
activityimputed[which(weekdays(activityimputed$date)==c("Saturday","Sunday")),]$type<-"weekend"

##Calc mean by 5-min interval by day type
activitydaytype<-aggregate(activityimputed$steps ~ activityimputed$type + activityimputed$interval,FUN=mean)
colnames(activitydaytype)<-c("type","interval","meansteps")

##time series plot of mean by 5-min interval by type
xyplot(activitydaytype$meansteps~activitydaytype$interval | type ,type="l",data=activitydaytype,layout=c(1,2))
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

By the two graphs it is clearly evident that the subject is more active on weekends. 
