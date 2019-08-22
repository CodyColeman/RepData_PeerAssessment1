------------------------------------------------------------------------

Loading and Preprocessing the Data
----------------------------------

#### 1. Load the data

    if(!file.exists('activity.csv')){
      if(!file.exists('activity.zip')){
        download.file('https://github.com/CodyColeman/RepData_PeerAssessment1', destfile = 'activity.zip', mode = 'wb')
      }
      unzip('activity.zip')
    }
    activity_data <- read.csv('activity.csv')

#### 2. Quick data exploration

    names(activity_data)

    ## [1] "steps"    "date"     "interval"

    head(activity_data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    summary(activity_data)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    str(activity_data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

#### 3. Create a few variables

    steps <- activity_data$steps
    date <- activity_data$date
    interval <- activity_data$interval

------------------------------------------------------------------------

What is the mean total number of steps taken per day?
-----------------------------------------------------

#### 1. Make a histogram of the number of steps per day.

    steps_per_day <- tapply(steps, date, sum, na.rm = TRUE)
    hist(steps_per_day, xlab = 'Total Steps (per day)', ylab = 'Frequency', breaks = 20, col = 'steelblue')

![](PA1_template_files/figure-markdown_strict/Hist&Mean-1.png)

#### 2. Give the mean and median of the total number of steps per day.

    mean_steps <- mean(steps_per_day)
    median_steps <- median(steps_per_day)

    print(paste0('The mean number of steps per day is ',  mean_steps))

    ## [1] "The mean number of steps per day is 9354.22950819672"

    print(paste0('The median number of steps per day is ',  median_steps))

    ## [1] "The median number of steps per day is 10395"

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

#### 1. Creat a time series plot for average steps per 5 minute increment.

    avgsteps_per_interval <- aggregate(x = steps, by = list(interval), FUN = mean, na.rm = TRUE)
    colnames(avgsteps_per_interval) <- c('Intervals', 'Steps')
    ggplot(avgsteps_per_interval, aes(x = Intervals, y = Steps)) + geom_line() + xlab('Intervals (5 minute increments)') + ylab('Total Steps per Interval')

![](PA1_template_files/figure-markdown_strict/TimeSeriesPlot-1.png)

#### 2. Give the 5-minute interval that, on average, contains the maximum number of steps.

    maxSteps <- avgsteps_per_interval[which(avgsteps_per_interval$Steps == max(avgsteps_per_interval$Steps)), ]
    maxSteps

    ##     Intervals    Steps
    ## 104       835 206.1698

    print(paste0('Thus, on average, in the ', maxSteps$Intervals, 'th interval, there are ', round(maxSteps$Steps, digits = 3), ' steps taken.' ))

    ## [1] "Thus, on average, in the 835th interval, there are 206.17 steps taken."

------------------------------------------------------------------------

Imputing missing values
-----------------------

#### 1. Describe a strategy for imputing missing data.

1.  We will view the total number of missing data to be sure that there
    is, in fact, missing data.
2.  We will replace all missing data with the avereage number of steps
    taken without the missing data.

#### 2. Show the strategy for imputing missing data.

###### 1. Total number of missing data

    #This number can also be seen in the summary of the original data.
    NAcount <- sum(is.na(activity_data))
    NAcount

    ## [1] 2304

###### 2. Create new dataset.

    activity_data_impute <- activity_data
    activity_data_impute$steps <- impute(activity_data$steps, fun = mean)

#### 3. Create Histogram of total number of seps taken each day after missing values are imputed.

    steps_per_day_imp <- tapply(activity_data_impute$steps, activity_data_impute$date, FUN = sum)
    hist(steps_per_day_imp, xlab = "Steps per day (Imputed)", ylab = 'Frequency', col = 'violetred', breaks = 20)

![](PA1_template_files/figure-markdown_strict/HistImputed-1.png)

#### 4. Give new mean and median for imputed data.

    newMeanSteps <- mean(steps_per_day_imp)
    newMedianSteps <- median(steps_per_day_imp)

    print(paste0('The new mean number of steps after imputation per day is ',  newMeanSteps))

    ## [1] "The new mean number of steps after imputation per day is 10766.1886792453"

    print(paste0('The new median number of steps after imputation per day is ',  newMedianSteps))

    ## [1] "The new median number of steps after imputation per day is 10766.1886792453"

------------------------------------------------------------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

#### 1. Seperate Weekends from Weekdays.

    activity_data_impute$date <- as.Date(activity_data_impute$date)
    activity_data_impute$weekday <- weekdays(activity_data_impute$date)
    activity_data_impute$weekend <- ifelse(activity_data_impute$weekday == 'Saturday' | activity_data_impute$weekday == 'Sunday', 'Weekend', 'Weekday')

#### 2. Create a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

    aggregatedActivityImp <- aggregate(steps ~ interval + weekend, data = activity_data_impute, mean)

    ggplot(aggregatedActivityImp, aes(interval, steps, color = weekend)) + geom_line() +
    facet_grid(weekend ~ . ) +
    xlab('Interval (5 minutes)') +
    ylab('Average Steps')  

![](PA1_template_files/figure-markdown_strict/PanelPlotWeekends/days-1.png)

    ggtitle("Weekday vs. Weekend Activities")

    ## $title
    ## [1] "Weekday vs. Weekend Activities"
    ## 
    ## attr(,"class")
    ## [1] "labels"
