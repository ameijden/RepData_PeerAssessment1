## Commit containing full submission

## Code for reading in the dataset and/or processing the data
## Histogram of the total number of steps taken each day
## Mean and median number of steps taken each day
## Time series plot of the average number of steps taken
## The 5-minute interval that, on average, contains the maximum number of steps
## Code to describe and show a strategy for imputing missing data
## Histogram of the total number of steps taken each day after missing values are imputed
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report



## Loading and processing the data

## unzip the data and create the data directory

unzip("activity.zip", exdir = "data")

## Read the data

directory <- getwd()
data <- read.csv(paste0(directory,"/data/activity.csv"), sep=",")


## Histogram of the total number of steps taken each day
ggplot(data = data, aes(steps)) + geom_histogram(col="green") + scale_fill_gradient("count", low="green", high="red")


## What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
## Make a histogram of the total number of steps taken each day
## Calculate and report the mean and median total number of steps taken per day

## New dataset with mean and median per day
library(dplyr)

Sumperday <-
  data %>%
    group_by(date) %>%
      summarize(Sumeveryday = sum(steps, na.rm = TRUE))

## Histogram of the total number of steps taken each day
ggplot(data = Sumperday, aes(Sumeveryday)) + geom_histogram(col="green") + theme_light(base_size = 12)

## Mean and median number of steps taken each day
## Add extra column to the table
Sumperday2 <-
  data %>%
    group_by(date) %>%
      summarize(Sumeveryday = sum(steps, na.rm = TRUE), (Meaneveryday = mean(steps, na.rm = TRUE)))


Sumperday3   <-
      data %>%
          group_by(date) %>%
             summarize(sum(steps), mean(steps))

Sumperday3

## What is the average daily activity pattern?
## Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

## Converting integer to hours and minutes
temp <- c(data$interval)
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(temp))
temp3 <- paste0(temp2, temp)
temp4 <- format(strptime(temp3, format="%H%M"), format = "%H:%M")
temp5 <- 

##  or shorter 
tempa <- c(data$interval)
tempb <- sprintf("%04d", tempa)
data2 <- mutate(data, time = (format(strptime(tempb, format="%H%M"), format = "%H:%M")) )

## Give me the total steps for every 5 minutes -> wrong
## dataonlyinterval <-
##    data2 %>%
##      group_by(time) %>%
##        summarize(totsteps = sum(steps, na.rm = TRUE))

## Give me the average number of steps for every 5 minutes
dataonlyintervalavg <-
    data2 %>%
      group_by(time) %>%
        summarize(avgsteps = mean(steps, na.rm = TRUE))

## Make time of time for the plot
dataonlyintervalavg$time = as.POSIXct(hms::parse_hm(dataonlyintervalavg$time))

## The plot
plot(dataonlyintervalavg$time, dataonlyintervalavg$avgsteps, type = "l", col="blue", xlab = "time", ylab = "Average steps")

## or
library(ggplot2)
ggplot(data = dataonlyintervalavg, aes(x=time, y=avgsteps)) + geom_line(color = "blue", size =2)  + scale_x_datetime(date_labels = "%H:%M")


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxavgstepstime <- dataonlyintervalavg[which.max(dataonlyintervalavg$avgsteps), ] ## Better to do this before transforming time


## ## Imputing missing values
## Note that there are a number of days/intervals where there are missing values 
## (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into 
## some calculations or summaries of the data.

## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

NAinthisset <-is.na(data)
table(NAinthisset)

## Only for the steps column
NAinsteps <- is.na(data$steps)
table(NAinsteps)

## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
## or the mean for that 5-minute interval, etc.

## Strategy 

## Create a new dataset that is equal to the original dataset but with the missing data filled in.



## Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?


## ## Are there differences in activity patterns between weekdays and weekends?
##  For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating
## whether a given date is a weekday or weekend day.
## Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, averaged 
## across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this plot should look like
## using simulated data.

## Convert date to date format -> not yet necessary
str(Sumperday)
library(lubridate)
(Sumperday$date <- ymd(Sumperday$date))

library(xtable)
xt <- xtable(summary(Sumperday))


xt2 <- xtable(summary(Sumperday$Sumeveryday))

## Histogrom of the total number of steps taken each day
hist(Sumperday$date, Sumperday$SUmeveryday, col = "blue", breaks=61 , main="Total number of steps taken each day", xlab="Day", ylab = "Total number of steps")
hist(SUmeveryday ~ date, data = Sumperday)


library(RColorBrewer) 
cols <- brewer.pal(11, "Spectral") 
pal <- colorRampPalette(cols)

barplot(SUmeveryday ~ date, data = Sumperday, xlab = "Date", col=pal(61), ylab = "Total number of steps", main = "Total number of steps taken each day") 


library(ggplot2)
cols <- brewer.pal(11, "Spectral") 
pal <- colorRampPalette(cols)
ggplot(data = Sumperday, aes(SUmeveryday)) + geom_histogram(col="green") + scale_fill_gradient("count", low="green", high="red")




## Calculate and report the mean and median total number of steps taken per day

Meanperday <-
  data %>%
  group_by(date) %>%
  summarize(Meaneveryday = mean(steps, na.rm = TRUE))
