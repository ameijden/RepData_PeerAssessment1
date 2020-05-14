##Assignment
##The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. 
##You must use the database to answer the questions below and show the code for your entire analysis. 
##Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

## Questions
##Your data analysis must address the following questions:
  
##  Across the United States, which types of events (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable) are most harmful with respect to population health?
##  Across the United States, which types of events have the greatest economic consequences?
##
## Consider writing your report as if it were to be read by a government or municipal manager who might be responsible 
##  for preparing for severe weather events and will need to prioritize resources for different types of events. 
## However, there is no need to make any specific recommendations in your report.

## Loading and processing the data

##  They add more events gradually and only from Jan. 1996 they start recording all events type. 
## Since our objective is comparing the effects of different weather events, do we need to include all years, 
## even it has only single event type? -> load from 1996


##  The official events type are 48. However, if you use 'unique' function on 'EVTYPE' column you will get 
## near one thousand events! All that is just typo. 
## The regular expression ('regex', grepl, regexpr, gregexpr) and 'tolower', and 'toupper' functions can be a great help here. 
## But cleaning up all that mess can take several days. However, reducing the size of data first will make a great difference here. 
## Subset your data first, think of years, total value, frequency ... anything else that can be used to reduce the number of rows. 
## Also, there are 37 variables, you do not need all of that for your analysis, remove as much as you can to make the analysis faster.

## Back to 'EVTYPE' column, one way to fix the typo is loading the official list of storm event types 
## and manually mapping all (or most) list of unique recorded events with a typo. This way is the most painful one. 
## Another way is using 'match' (from 'base' package) or even better you may use 'amatch' function from 'stringdist' package 
## for Approximate String Matching. In that case, you will need to experiments with 'maxDist' option to get good accuracy.


## Read the data

directory <- getwd()
data <- read.csv(paste0(directory,"/repdata_data_StormData.csv.bz2"), sep=",")

library(sqldf)
Onlypartofthedata <- read.csv.sql(paste0(directory,"/repdata_data_StormData.csv.bz2"), sql = "select * from file where BGN_DATE  > '1/1/1996'", header = TRUE, sep = ",")


str(data)

## cacher package no longer available on CRAN
## downloaded the package from http://www2.uaem.mx/r-mirror/web/packages/cacher/index.html
## Replaced by R.cache


library(cacher)
clonecache(id=)


str(data)
summary(data)