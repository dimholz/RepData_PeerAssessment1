#Peer-graded Assignment: Course Project 1
##Daniel Imholz

Before we begin, let's load the packages we will be using in this
analysis.

    library(ggplot2)
    library(plyr)
    library(dplyr)
    library(tidyr)

Loading and preprocessing the data
----------------------------------

Let's now load the data, assuming we are already in the correct working
directory. Part of the pre-processing for this data will include
transforming the items in the **date** column into Date objects.

    activity <- read.csv('activity.csv')
    activity <- transform(activity, date = as.Date(date, format = '%Y-%m-%d'))
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

    total_steps <- activity %>%
      group_by(date) %>%
      summarise(n_steps = sum(steps, na.rm = TRUE))

    ggplot(total_steps, aes(n_steps)) +
      geom_histogram(bins = 20, fill = 'slateblue', col = 'goldenrod') +
      xlab('Total Number of Steps') + 
      ylab('Count') +
      ggtitle('Total Number of Steps Taken per Day')

![](PA1_template_files/figure-markdown_strict/total-1.png)

The mean of the total number of steps taken per day is 9354.23 steps.  
The median of the total number of steps taken per day is 10395 steps.

What is the average daily activity pattern?
-------------------------------------------

    by_int <- activity %>%
      group_by(interval) %>%
      summarise(m_steps = mean(steps, na.rm = TRUE))

    ggplot(by_int, aes(interval, m_steps)) +
      geom_line() + 
      xlab('Time Interval') + 
      ylab('Average Number of Steps') +
      ggtitle('Average Number of Steps Taken per Time Interval, Across All Days') 

![](PA1_template_files/figure-markdown_strict/mean_daily-1.png) The
5-minute interval where **interval** = 835 contains the maximum number
of steps.

Imputing missing values
-----------------------

We noticed there are some missing values in our data set, 2304 to be
exact. Let's impute these missing values using the floor of the daily
*mean* for the corresponding 5-minute time interval. We'll use the newly
imputed data to create a histogram as we did earlier, and make note of
any differences.

    by_int_floors <- activity %>%
      group_by(interval) %>%
      mutate(f_mean = mean(steps, na.rm = TRUE) %>%
                  floor())

    by_int_floors$imputed <- apply(by_int_floors,1, function(x) {
        x[1] <- ifelse(is.na(x[1]), x[4], x[1])
      }) %>%
      as.numeric()

    activity_imp <- by_int_floors %>%
      dplyr::select(imputed, date, interval) %>%
      group_by(date) 
    total_imp <- activity_imp %>%
      summarise(imp_n_steps = sum(imputed))

    ggplot(total_imp, aes(imp_n_steps)) +
      geom_histogram(bins = 20, fill = 'slateblue', col = 'goldenrod') +
      xlab('Total Number of Steps') + 
      ylab('Count') +
      ggtitle('Total Number of Steps Taken per Day, After Imputation')

![](PA1_template_files/figure-markdown_strict/imputing-1.png)

After imputation, the mean of the total number of steps taken per day is
10749.77 steps, compared to a mean of 9354.23 before imputation.  
After imputation, the median of the total number of steps taken per day
is 10641 steps, compared to a median of 10395 before imputation.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    library(lattice)
    daynames <- weekdays(activity_imp$date)
    activity_imp$daytype <- ifelse(daynames %in% c('Saturday','Sunday'), 'weekend','weekday') %>%
      as.factor()
    by_int_imp <- activity_imp %>%
      group_by(daytype,interval) %>%
      summarise(mean_steps = mean(imputed))
    xyplot(mean_steps ~ interval | daytype, data = by_int_imp,
           type = 'l',
           xlab = 'Time Interval',
           ylab = 'Average Number of Steps',
           main = 'Average Number of Steps Taken per Time Interval, by DayType')

![](PA1_template_files/figure-markdown_strict/weekday-1.png)
