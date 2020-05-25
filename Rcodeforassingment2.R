## Assignment
## The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. 
## You must use the database to answer the questions below and show the code for your entire analysis. 
## Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

## Questions
##Your data analysis must address the following questions:
  
##  Across the United States, which types of events (in EVTYPE variable) are most harmful with respect to population health?
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
library(data.table)
file <- "repdata_data_StormData.csv.bz2" ## assumes you got this file in your working directory
Alldata <- fread(file) 

## Converting to date
Alldata$BGN_DATE <- as.Date(Alldata$BGN_DATE, "%m/%d/%Y %H:%M:%S")

## Subsetting only dates after 1996 because only after this date there are all the eventtypes we can compare
Dataafter1996 <- Alldata[BGN_DATE > '1996-01-01']

str(Dataafter1996)

## Next step, fixing the EVTYPE column -> 515 events but should be only 48 according to specs

UniqueEvents <- unique(Dataafter1996$EVTYPE) ## -> 515
UniqueEvents


library(stringdist)
?amatch
## Getting the official orginal list of EVTYPES
## Knowing which Event Types we want and get those from the dataset.
## List of Event Types is on page 6 in the PDF to use with amatch (https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

##  Across the United States, which types of events (in EVTYPE variable) are most harmful with respect to population health?
## Which data do I need?
## 2.6 (PDF https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) Fatalities / Injuries

## Direct fatalities -> fatality / injury (columns fatalites / injuries) -> also location is available
## Indirect fatalities / injuries -> 

## Data is needed Event Type / number of fatalities / number of injuries
## Clean the data and only leave data / event types / number of fatalities / number of injuries

## Copied the list in Notepad, saved as text, put it in the workdirectory and then loaded in R
stringEvents <- read.table("EVTypes.txt", sep="\t")


library(dplyr)
Stormdataonhealth <-
    Dataafter1996 %>%
      select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, REMARKS)

WhichEVdoesmostdamage <-
  Stormdataonhealth %>%
    group_by(EVTYPE) %>%
      summarize(NrFatalities = sum(FATALITIES), NrInjuries = sum(INJURIES))

## all data where Nrinjuries = 0 and NrInjuries = 0

WhichDataCanBeRemovedFromEVmostdamage <-
  WhichEVdoesmostdamage %>%
    filter(NrFatalities == 0, NrInjuries == 0)

## Only data with EV which has injuries (should be 515 - 377 = 138)
OnlyEVwithdamage <-
  WhichEVdoesmostdamage %>%
  filter(NrFatalities > 0, NrInjuries > 0) ## missing some data

## Only with 1 or more fatalities
OnlyEVwithFatdamage <-
  WhichEVdoesmostdamage %>%
  filter(NrFatalities > 0)

## For this subset we bind them to one of the 48 Event Types

## Make a vector of EventTypes

vectorEvents <- as.vector(unlist(stringEvents['V1']))

vectorEVFAT <- as.vector(unlist(OnlyEVwithFatdamage['EVTYPE']))
##

library(RecordLinkage)

ClosestMatch3 = function(string, stringVector){
  
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

## ClosestMatch3("AVALANCHE", vectorEvents)

## Perform this match for all 

matches <- lapply((vectorEVFAT), ClosestMatch3, stringVector=vectorEvents)
str(matches)

## matches4 <- unlist(matches)
## matches5 <- matches[[1]][1]


## Also do this not for the first but also for the last 

matches7 <- lapply(matches, '[[',1)

str(matches7)

## Also do this not for the first but also for the last 
lastValue <- function(x)   tail(x[!is.na(x)], 1)

matches7a <- lapply(matches, lastValue)


## matches8 <- unlist(lapply(matches, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)


## Add this data (matches7) to the OnlyEVwithFatdamage table so we can see how good the matching has been done

str(OnlyEVwithFatdamage)
str(matches7)

summary(OnlyEVwithFatdamage)

## Add ID's to the dataframe and list so I can merge this by ID

OnlyEVwithFatdamagewithID <-
  OnlyEVwithFatdamage %>%
    mutate(id = row_number())

## Convert list to dataframe

matches10 <- data.frame(matrix(unlist(matches7), nrow=109, byrow=T),stringsAsFactors=FALSE)

## Add ID's to the list
matches10withID <-
    matches10 %>%
        mutate(id = row_number())

## Join this two dataframes by id

OnlyEVwithFATdamagecompare2 <- full_join(OnlyEVwithFatdamagewithID, matches10withID)
str(OnlyEVwithFATdamagecompare2)

colnames(OnlyEVwithFATdamagecompare2)[5] <- "OrginalEventTypes"

WhichEVdoesmostFATdamage <-
  OnlyEVwithFATdamagecompare2 %>%
      group_by(OrginalEventTypes) %>%
          summarize(NrFat = sum(NrFatalities), NrInjur = sum(NrInjuries)) %>%
            arrange(desc(NrFat))


NrFatgreaterthan250 <- 
  WhichEVdoesmostFATdamage %>%
    filter(NrFat > 250)  
      arrange(desc(NrFat))


CombinedFATANDINJdamage <- 
    WhichEVdoesmostFATdamage %>%
        mutate(combineddamage = (2*NrFat + NrInjur))
    
    
CombinedFATANDINJdamagegr2500 <- 
 CombinedFATANDINJdamage %>%
   filter(combineddamage > 2500)
      

  
## Create Plot
library(ggplot2)
library(gridExtra)

p1 <- ggplot(data=NrFatgreaterthan250, aes(OrginalEventTypes, NrFat, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
p2 <- ggplot(data=NrINjgreaterthan1500, aes(OrginalEventTypes, NrInjur, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
p3 <- ggplot(data=CombinedFATANDINJdamagegr2500, aes(OrginalEventTypes, combineddamage, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
grid.arrange(p1, p2, p3, nrow = 2)


library(ggplot2)
## Only number of fatalities
ggplot(data=WhichEVdoesmostFATdamage, aes(OrginalEventTypes, NrFat, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
ggplot(data=NrFatgreaterthan250, aes(OrginalEventTypes, NrFat, fill = OrginalEventTypes)) + geom_bar(stat = "identity")


## Only number of injuries

ggplot(data=WhichEVdoesmostInjdamage, aes(OrginalEventTypes, NrInjur, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
ggplot(data=NrINjgreaterthan250, aes(OrginalEventTypes, NrInjur, fill = OrginalEventTypes)) + geom_bar(stat = "identity")



## Total number of accidents (Fatalities is counted 2x and injuries only once)

ggplot(data=CombinedFATANDINJdamagegr600, aes(OrginalEventTypes, combineddamage, fill = OrginalEventTypes)) + geom_bar(stat = "identity")


################# Repeating this for number of injuries

## Only with 1 or more injury
OnlyEVwithInjdamage <-
  WhichEVdoesmostdamage %>%
  filter(NrInjuries > 0)

## For this subset we bind them to one of the 48 Event Types,Perform this match for all 

## Make a vector of EventTypes

vectorEVInj <- as.vector(unlist(OnlyEVwithInjdamage['EVTYPE']))


matchesinj <- lapply((vectorEVInj), ClosestMatch3, stringVector=vectorEvents)
str(matches)

matchesinj2 <- unlist(matches)
matchesinj3 <- matches[[1]][1]

matchesinj4 <- lapply(matchesinj, '[[',1)

## Add this data (matches7) to the OnlyEVwithFatdamage table so we can see how good the matching has been done

## Add ID's to the dataframe and list so I can merge this by ID

OnlyEVwithInjdamagewithID <-
  OnlyEVwithInjdamage %>%
  mutate(id = row_number())

## Convert list to dataframe

matchesinj5 <- data.frame(matrix(unlist(matchesinj4), nrow=109, byrow=T),stringsAsFactors=FALSE)

## Add ID's to the list
matchesinj6withID <-
  matchesinj5  %>%
  mutate(id = row_number())

## Join this two dataframes by id

OnlyEVwithInjdamagecompare2 <- full_join(OnlyEVwithInjdamagewithID , matchesinj6withID)
str(OnlyEVwithInjdamagecompare2)

colnames(OnlyEVwithInjdamagecompare2)[5] <- "OrginalEventTypes"

WhichEVdoesmostInjdamage <-
  OnlyEVwithInjdamagecompare2 %>%
    group_by(OrginalEventTypes) %>%
      summarize(NrFat = sum(NrFatalities), NrInjur = sum(NrInjuries)) %>%
        arrange(desc(NrInjur))


NrINjgreaterthan1500 <- 
  WhichEVdoesmostInjdamage %>%
    filter(NrInjur > 1500)  %>%
      arrange(desc(NrFat))




#########################################################################################################################
## Second question:Across the United States, which types of events have the greatest economic consequences?
##################################################################################################################


## Which data do we need?

## The 'CROPDMGEXP' is the exponent values for 'CROPDMG' (crop damage). 
## In the same way, 'PROPDMGEXP' is the exponent values for 'PROPDMG' (property damage). 
## You should use both to get the total values for crops and property damage. 
## (B or b = Billion, M or m = Million, K or k = Thousand, H or h = Hundred). 
## The number from one to ten represent the power of ten (10^The number). 
## The symbols "-", "+" and "?" refers to less than, greater than and low certainty. 
## You have the option to ignore these three symbols altogether. 
## Also, there is a nice work entitled: How To Handle Exponent Value of PROPDMGEXP and CROPDMGEXP. 
## It discusses that issue in more depth.
## https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html



## Only the data with damage and events

Stormdataoneconomics <-
  Dataafter1996 %>%
    select(BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP ,CROPDMG, CROPDMGEXP)


#################


## Transform the letters to numbers 
## multiply by this number

str(Stormdataoneconomics)

Valuesstormdataoneconomics <- 
  Stormdataoneconomics %>%
    mutate(propexvalue = case_when(
      PROPDMGEXP == "B" ~ 1000000000,
      PROPDMGEXP == "b" ~ 1000000000,
      PROPDMGEXP == "M" ~ 1000000,
      PROPDMGEXP == "m" ~ 1000000,
      PROPDMGEXP == "K" ~ 1000,
      PROPDMGEXP == "k" ~ 1000))


Valuesstormdataoneconomics2 <- 
  Valuesstormdataoneconomics %>%
      mutate(cropexvalue = case_when(
    CROPDMGEXP == "B" ~ 1000000000,
    CROPDMGEXP == "b" ~ 1000000000,
    CROPDMGEXP == "M" ~ 1000000,
    CROPDMGEXP == "m" ~ 1000000,
    CROPDMGEXP == "K" ~ 1000,
    CROPDMGEXP == "k" ~ 1000))


Valuesstormdataoneconomics3 <- 
    Valuesstormdataoneconomics2 %>%
          mutate(realpropvalue = (PROPDMG * propexvalue))

Valuesstormdataoneconomics4 <- 
  Valuesstormdataoneconomics3 %>%
      mutate(realcropvalue = (CROPDMG * cropexvalue))

str(Valuesstormdataoneconomics4)

### Most property damage
    MostPropDamage <-
      Valuesstormdataoneconomics4 %>%
          group_by(EVTYPE) %>%
            summarize(PropDamage = sum(realpropvalue), CropDamage = sum(realcropvalue)) %>% 
                  filter(PropDamage > 0) %>%
                       arrange(desc(PropDamage))

### Match the EventTypes with the orginal set of EventTypes
## Make a vector of EventTypes
    
vectorEvents <- as.vector(unlist(stringEvents['V1']))
    
vectorPropdam <- as.vector(unlist(MostPropDamage['EVTYPE']))
    
matchesprop <- lapply((vectorPropdam), ClosestMatch3, stringVector=vectorEvents)

## matchesprop2 <- unlist(matchesprop)
## matchesprop3 <- matchesprop[[1]][1]
    
matchesprop4 <- lapply(matchesprop, '[[',1)


lastValue <- function(x)   tail(x[!is.na(x)], 1)

matchesprop4a <- lapply(matchesprop, lastValue)
    
    ## Add this data to MostPropDamage table so we can see if the matching has been properly done
    
    ## We need to add ID's to the dataframe and list so I can merge this by ID
    
MostPropDamagewithID <-
      MostPropDamage  %>%
      mutate(id = row_number())

    ## Convert list to dataframe
    
matchesprop5 <- data.frame(matrix(unlist(matchesprop4), nrow=53, byrow=T),stringsAsFactors=FALSE)

matchesprop5a <- data.frame(matrix(unlist(matchesprop4a), nrow=53, byrow=T),stringsAsFactors=FALSE)
    
    ## Add ID's to the list
matchesprop6withID <-
  matchesprop5  %>%
      mutate(id = row_number())


matchesprop6withIDa <-
  matchesprop5a  %>%
  mutate(id = row_number())

    ## Join this two dataframes by id
    
MostPropDamagecompare <- full_join(matchesprop6withID, MostPropDamagewithID)
MostPropDamagecomparea <- full_join(matchesprop6withIDa, MostPropDamagewithID)

## Taking last seems to be doing a much better job so we stick to the last

str(MostPropDamagecompare) ## See which I need 

## Use https://forecast.weather.gov/glossary.php?word=TSTM to see what a word means
## THUNDERSTORM WIND (nothing) only Thunderstorm so we link it to Thunderstorm
## River Flooding, The rise of a river to an elevation such that the river overflows its natural banks causing or threatening damage.
## Link to Flash Flood (A rapid and extreme flow of high water into a normally dry area, or a rapid water level rise in a stream or creek 
## above a predetermined flood level, beginning within six hours of the causative event (e.g., intense rainfall, dam failure, ice jam).
## However, the actual time threshold may vary in different parts of the country. Ongoing flooding can intensify to flash flooding in cases 
## where intense rainfall results in a rapid surge of rising flood waters.


## ad 1) Thunderstorm wind -> matching not so good for this.
## ad 6) Other change -> 	DAMAGING FREEZE -> will be Frost/Freeze NOT Debris Flow
## ad 17) MARINE THUNDERSTORM WIND -> should be Marien Thunderstorm Wind

colnames(MostPropDamagecomparea)[1] <- "OrginalEventTypes"
    
WhichEVdoesmostpropertydamage <-
      MostPropDamagecomparea %>%
        group_by(OrginalEventTypes) %>%
          summarize(TotalPropDamage = sum(PropDamage), TotalCropDamage = sum(CropDamage)) %>%
              arrange(desc(TotalPropDamage))

    
## Only the most damage
   Top6EVMostPropDamage <-
     WhichEVdoesmostpropertydamage %>%
        filter(TotalPropDamage > 	8110001)
    
    

## Create Plot
library(ggplot2)
library(scales)
library(gridExtra)
    
p1 <- ggplot(data=Top6EVMostPropDamage, aes(OrginalEventTypes, TotalPropDamage, fill = OrginalEventTypes)) + geom_bar(stat = "identity") + coord_trans(x = "exp")
p2 <- ggplot(data=NrINjgreaterthan1500, aes(OrginalEventTypes, NrInjur, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
p3 <- ggplot(data=CombinedFATANDINJdamagegr2500, aes(OrginalEventTypes, combineddamage, fill = OrginalEventTypes)) + geom_bar(stat = "identity")
grid.arrange(p1, p2, p3, nrow = 2)

grid.arrange(p1)

    

library(treemap)
treemap(Top6EVMostPropDamage,
        index=c("OrginalEventTypes"),
        vSize = "TotalPropDamage",
        type = "index",
        palette = "Reds",
        title="Total Propery Damage by Event Type",
        fontsize.title = 14)

########################################################################################
## How does it look like if we do this for Crop Damage?

### Most crop damage
MostCropDamage <-
  Valuesstormdataoneconomics4 %>%
    group_by(EVTYPE) %>%
      summarize(PropDamage = sum(realpropvalue), CropDamage = sum(realcropvalue)) %>% 
         filter(CropDamage > 0) %>%
            arrange(desc(CropDamage))

## Make a vector of EventTypes
vectorEvents <- as.vector(unlist(stringEvents['V1']))

## Make a vector of the Crop Events
vectorCropdam <- as.vector(unlist(MostCropDamage['EVTYPE']))


### Match the EventTypes with the orginal set of EventTypes

matchescrop <- lapply((vectorCropdam), ClosestMatch3, stringVector=vectorEvents)

lastValue <- function(x)   tail(x[!is.na(x)], 1)

matchescrop2 <- lapply(matchescrop, lastValue)

## Add this data to MostPropDamage table so we can see if the matching has been properly done

## We need to add ID's to the dataframe and list so I can merge this by ID

MostCropDamagewithID <-
  MostCropDamage  %>%
    mutate(id = row_number())

## Convert list to dataframe

matchescrop3 <- data.frame(matrix(unlist(matchescrop2), nrow=10, byrow=T),stringsAsFactors=FALSE)


## Add ID's to the list
matchescrop4withID <-
  matchescrop3  %>%
    mutate(id = row_number())

## Join this two dataframes by id

MostCropDamagecompare <- full_join(matchescrop4withID, MostCropDamagewithID)
colnames(MostCropDamagecompare)[1] <- "OrginalEventTypes"


## Looking at the data some events where not well matched
## (2)	Early Frost -> should be Frost/Freeze not Coastal Flood
## (4) 	AGRICULTURAL FREEZE -> should be Frost/Freeze not Coastal Flood
## (6) UNSEASONAL RAIN -> HEAVY RAIN? not Volcanic Ash
## (7) 	Unseasonable Cold -> Frost/Freeze not 	Coastal Flood

## Fix this -> Change Coastal Flood for Frost/Freeze
## To DO

MostCropDamagecompareimproved <- 
  MostCropDamagecompare  %>%
  mutate(OrginalEventTypes = case_when(
    OrginalEventTypes == "Coastal Flood" ~ "Frost/Freeze",
    OrginalEventTypes == "Volcanic Ash" ~ "Heavy Rain",
    TRUE ~ as.character(OrginalEventTypes)))


## Or do this for all eventtypes? Do the manual compare

WhichEVdoesmostcropdamage <-
  MostCropDamagecompareimproved %>%
    group_by(OrginalEventTypes) %>%
        summarize(TotalPropDamage = sum(PropDamage), TotalCropDamage = sum(CropDamage)) %>%
            arrange(desc(TotalCropDamage))


## Create plot

treemap(WhichEVdoesmostcropdamage,
        index=c("OrginalEventTypes"),
        vSize = "TotalCropDamage",
        type = "index",
        palette = "Reds",
        title="Total Crop Damage by Event Type",
        fontsize.title = 14)





## ggplot(data = req, aes(year, Emissions, fill = year, label = round(Emissions,2))) +geom_bar(stat = "identity") + ggtitle("Emissions From Motor Vehicle Sources in Baltimore City") + geom_label(aes(fill = year),colour = "white", fontface = "bold")



#######################################################################################################
### Can I beat this performance by writing a function for amatch 


####
word <- 'test'
words <- c('Teest','teeeest','New York City','yeast','text','Test')

## Create a function to see the match?


library(stringdist)

ClosestMatch2 = function(string, stringVector){
  
  stringVector[amatch(string, stringVector, maxDist=2)]
  
}



ClosestMatch2(OnlyEVwithFatdamage$EVTYPE, stringEvents)

ClosestMatch2(word,words)

ClosestMatch2("AVALANCHE", vectorEvents)

str(OnlyEVwithFatdamage$EVTYPE)

## 
library(RecordLinkage)

ClosestMatch3 = function(string, stringVector){
  
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

ClosestMatch3("AVALANCHE", vectorEvents)



ClosestMatch3(word,words)

dplyr::pull(vectorEvents, V1)


str(word)
str(stringEvents)

vectorEvents <- as.vector(unlist(stringEvents['V1']))
class(vectorEvents)
str(vectorEvents)

vectorEvents <- 
str(vectorEvents)
  
str(words)

## Only with 1 or more injuries
OnlyEVwithInjdamage <-
  WhichEVdoesmostdamage %>%
  filter(NrInjuries > 0)

## To Do : Automate this getting this list
EventTypes <- read.table("EVTypes", sep = "\t")



match1 <- amatch(OnlyEVwithInjdamage$EVTYPE, EventTypes)

match1 <- amatch("Coastal Flooding", EventTypes, maxDist = 10)
match1

str(EventTypes)

EventTypes <- table(EventTypes)

## Merge these two datasets

Alldatawithharm <- merge(OnlyEVwithFatdamage, OnlyEVwithInjdamage, by='EVTYPE') ## 77 

## Which data is meaningfull?

## number of injuries > 10? 



## Removing this data from the dataset
str(WhichEVdoesmostdamage)

## using join



## cacher package no longer available on CRAN
## downloaded the package from http://www2.uaem.mx/r-mirror/web/packages/cacher/index.html

library(cacher)
clonecache(id=)


## Read and subset the data. Why is this not working?

directory <- getwd()
data <- read.csv(paste0(directory,"/repdata_data_StormData.csv.bz2"), sep=",")

library(sqldf)
Onlypartofthedata <- read.csv2.sql(paste0(directory,"/repdata_data_StormData.csv.bz2"), sql = "select * from file where BGN_DATE like '1/1/1996'", header=TRUE, sep = ",")





###
library(stringdist)
?amatch/
  
vb1 <- amatch("leia",c("uhura","leela"),maxDist=5)
vb2 <- amatch("AVALANCHE", vectorEvents,maxdist=5)

vectorEvents

## Make a vector of EventTypes

vectorEvents <- as.vector(unlist(stringEvents['V1']))

vectorEVFAT <- as.vector(unlist(OnlyEVwithFatdamage['EVTYPE']))


##########
## (https://stackoverflow.com/questions/51253360/r-how-to-find-the-most-optimal-string-matches-while-combining-different-distanc)
names_evsplit <- strsplit(OnlyEVwithFatdamage$EVTYPE, ",")[[1]]
names_orgevtypes <- strsplit(EventTypes, ",")[[1]]

# It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
dist.name<-adist(OnlyEVwithFatdamage$EVTYPE, EventTypes, partial = TRUE, ignore.case = TRUE)

# We now take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)
min.name

match.s1.s2<-NULL  
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=EventTypes[s2.i], s1name=OnlyEVwithFatdamage$EVTYPE[s1.i], adist=min.name[i]),match.s1.s2)
}
# and we then can have a look at the results
View(match.s1.s2)

rm(match.s1.s2)


## Function for lower casing
OnlyEVwithFatdamage2 <-   lapply(OnlyEVwithFatdamage, function(v) {
  if(is.character(v)) return(tolower(v))
  else return(v)
})
but easier with
vectorEVFAT3 <- lapply(vectorEVFAT, tolower)


#####

## How To Handle Exponent Value of PROPDMGEXP and CROPDMGEXP
## https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

## Guy spending months on this assignment
## https://github.com/jzstats/Reproducible-Research--2nd-Assignment
## RPubs: https://rpubs.com/JZstats/Reproducible-Research--2nd-Assignment
## Review https://www.coursera.org/learn/reproducible-research/peer/OMZ37/course-project-2/review/PWN9LJVKEeqTHRLkj-HkLw


