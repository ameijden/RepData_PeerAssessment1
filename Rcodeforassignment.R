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

## Transform the data for the histogram and removing all the NA's in the steps
library(dplyr)

Sumperday <-
  data %>%
    group_by(date) %>%
      summarize(Sumeveryday = sum(steps, na.rm = TRUE))

## Histogram of the total number of steps taken each day
ggplot(data = Sumperday, aes(Sumeveryday)) + geom_histogram(binwidth = 245, col="blue") + theme_light(base_size = 12) + scale_x_continuous(name = "Total number of steps taken each day")

## Mean and median number of steps taken each day
MeanMedianperday <-
      data %>%
          group_by(date) %>%
             summarize(mean = mean(steps), median = median(steps))

print(MeanMedianperday)

library(xtable)
xt <- xtable(MeanMedianperday)
print(xt)


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

library(dplyr)
## Give me the average number of steps for every 5 minutes
dataonlyintervalavg <-
    data2 %>%
      group_by(time) %>%
        summarize(avgsteps = mean(steps, na.rm = TRUE))

## Make time of time for the plot
dataonlyintervalavg$time = as.POSIXct(hms::parse_hm(dataonlyintervalavg$time))

## The plot
plot(dataonlyintervalavg$time, dataonlyintervalavg$avgsteps, type = "l", col="blue", xlab = "time", ylab = "Average steps")

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxavgstepstime <- dataonlyintervalavg[which.max(dataonlyintervalavg$avgsteps), ] ## Better to do this before transforming time
print(maxavgstepstime[1])

str()

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
## See article, I should use bCPA or FKM (fuzzy K-means) -> get to know how it works.
## Let's start with the most simple -> mean for that 5-minute interval 
## These values are already available dataonlyintervalavg

library(imputeMissings)
newdata <- impute(data)

NAinnewdataset <-is.na(newdata)
table(NAinnewdataset)

## Comparing the two datasets
write.csv(data, "data.csv")
write.csv(newdata, "newdata.csv")
## So all NA's are replaced with 0, I would like to use the 5-minute interval mean

## To replace the NA's with the mean of the 5-minute interval
## Add interval to the dataonlyyintervalavg dataset (this dataset has the average)

interval1day <- temp[1:288]

  doiavg2 <-
      dataonlyintervalavg %>%
        mutate(interval = interval1day)
  

## Merging the intitial dataset with this new dataset to get for every interval also the 5 minute mean
newdata3 <- inner_join(data, doiavg2, by=c("interval"))

## Imputing the missing data with the average of that time
newdata4 <-
  newdata3  %>%
    mutate(steps=ifelse(is.na(steps),(avgsteps), steps))

## Removing all unnecessary columns
newdata5 <- subset(newdata4, select = c(steps, date, interval))

## No more NA's
NAinnewdataset5 <-is.na(newdata5)
table(NAinnewdataset5)



write.csv(newdata5, "newdata5.csv")


## Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Histogram of the total number of steps taken each day
library(ggplot2)
ggplot(data = newdata5, aes(steps)) + geom_histogram(col="green") + scale_fill_gradient("count", low="green", high="red")


## Calculate and report the mean and median total number of steps taken per day. 
MeanMedianperdaynewdata   <-
    newdata5 %>%
      group_by(date) %>%
          summarize(mean = mean(steps), median = median(steps))

print(MeanMedianperdaynewdata)



## Do these values differ from the estimates from the first part of the assignment? 
write.csv(MeanMedianperdaynewdata, "MeanMedianperdaynewdata.csv")
write.csv(MeanMedianperday, "MeanMedianperday.csv")
## Only the days with NA's have no data

## What is the impact of imputing missing data on the estimates of the total daily number of steps?
## Nothing because that didn't change


## ## Are there differences in activity patterns between weekdays and weekends?
##  For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating
## whether a given date is a weekday or weekend day.

## Converting to a date format

library(lubridate)
newdata5$date <- ymd(newdata5$date)

## Getting the days of the week using the weekdays function
newdata6 <-
    newdata5  %>%

      mutate(weekday = weekdays(newdata5$date))

## Converting the days to weekend or weekday
newdata7 <-
    newdata6 %>% 
      mutate(weekendornot = ifelse(newdata6$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))


## Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, averaged 
## across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this plot should look like
## using simulated data.


##  Add the time to the dataset 
tempa <- c(data$interval)
tempb <- sprintf("%04d", tempa)
newdata71 <- mutate(newdata7, time = (format(strptime(tempb, format="%H%M"), format = "%H:%M")))


## Then split the data in weekday and weekend
newdata8 <- split(newdata71, newdata71$weekendornot)

## newdata8[[1]] -> weekday
## newdata8[[2]] -> weekend



## The average number of steps for every 5 minutes for weekday
dataonlyintervalavgweekday <-
  newdata8[[1]] %>%
    group_by(time) %>%
        summarize(avgsteps = mean(steps, na.rm = TRUE))


## The average number of steps for every 5 minutes for a weekend
dataonlyintervalavgweekend <-
  newdata8[[2]] %>%
      group_by(time) %>%
          summarize(avgsteps = mean(steps, na.rm = TRUE))


## Make time of time for the plot
dataonlyintervalavgweekday$time = as.POSIXct(hms::parse_hm(dataonlyintervalavgweekday$time))
dataonlyintervalavgweekend$time = as.POSIXct(hms::parse_hm(dataonlyintervalavgweekend$time))

str(newdata8)
## The plot

par(mfrow=c(2,1))
plot(dataonlyintervalavgweekend$time, dataonlyintervalavgweekend$avgsteps, type = "l", col="blue", xlab = "time", ylab = "Average steps in the weekend", main ="weekend")
plot(dataonlyintervalavgweekday$time, dataonlyintervalavgweekday$avgsteps, type = "l", col="blue", xlab = "time", ylab = "Average steps on a weekday", main = "weekday")


## or
library(ggplot2)
ggplot(data = dataonlyintervalavgweekend, aes(x=time, y=avgsteps)) + geom_line(color = "blue", size =2)  + scale_x_datetime(date_labels = "%H:%M") 

MeanMedianperday <-
  data %>%
  group_by(date) %>%
  summarize(mean = mean(steps), median = median(steps)) 

MeanMedianperdayX <-
  MeanMedianperday %>%
  mutate(mean2 = round(mean, digits = 2))



Medianperday <-
  data %>%
  group_by(date) %>%
  summarize( median = median(steps)) 


Datafortablemeanmedianperday <- select(MeanMedianperdayX, -mean)

Testround <- round(MeanMedianperday$mean, digits = 2)
Testroundnew <- round(MeanMedianperdaynewdata, digits = 2)

str(MeanMedianperday)


MeanMedianperdaynewdatatest   <-
  newdata5 %>%
  group_by(date) %>%
  round(summarize(mean = mean(steps), median = median(steps)), digits = 2)

MeanMedianperdaynewdatatest <- round(MeanMedianperday$mean, digits = 2)

Medianperdaynewdatatest   <-
  newdata5 %>%
  group_by(date) %>%
  summarize(median = median(steps))

MeanMedianRound <- round((MeanMedianperday[2:3]), digits = 2)


RoundedMeanMedianperdaynewdata <-
  MeanMedianperdaynewdata %>%
  mutate(meanround = round(mean, digits = 2)) %>%
  mutate(medianround = round(median, digits = 2))

DatafortableRoundedmeanmedianperdaynewdata <- select(RoundedMeanMedianperdaynewdata, -c(mean, median))