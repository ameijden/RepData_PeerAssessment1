
## Only code

## Read the data
library(data.table)
file <- "repdata_data_StormData.csv.bz2" ## assumes you got this file in your working directory
Alldata <- fread(file) 

## Converting to date
Alldata$BGN_DATE <- as.Date(Alldata$BGN_DATE, "%m/%d/%Y %H:%M:%S")

## Subsetting only dates after 1996 because only after this date there are all the eventtypes we can compare
Dataafter1996 <- Alldata[BGN_DATE > '1996-01-01']


## Only the data we are interested in 
library(dplyr)
Stormdataonhealth <-
  Dataafter1996 %>%
    select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, REMARKS)

WhichEVdoesmostdamage <-
  Stormdataonhealth %>%
    group_by(EVTYPE) %>%
     summarize(NrFatalities = sum(FATALITIES), NrInjuries = sum(INJURIES))

## Only with 1 or more fatalities
OnlyEVwithFatdamage <-
  WhichEVdoesmostdamage %>%
    filter(NrFatalities > 0)

## Only FAT event types
UniqueFATEvents <- unique(OnlyEVwithFatdamage$EVTYPE) 

stringEvents <- read.table("EVTypes.txt", sep="\t")



#######################################################################################
## Create an automated way to find the matches
#####################################################################################
library(stringdist)

FindMatch = function(string, stringVector) {
  amatch(string, stringVector, method='jw', maxDist = 15, p=0.1)
}


matchesfat <- lapply((vectorEVFAT2), FindMatch, stringVector=vectorEvents)

matchesfat2 <- as.vector(unlist(matchesfat))

matchesfat3 <- vectorEvents[(matchesfat2)]

matchesfat4 <- data.frame(matrix(unlist(matchesfat3), nrow=109, byrow=T),stringsAsFactors=FALSE)

matchefat4withID <-
  matchesfat4  %>%
  mutate(id = row_number())

## Add ID's to the dataframe and list so I can merge this by ID
OnlyEVwithFatdamagewithID <-
  OnlyEVwithFatdamage %>%
  mutate(id = row_number())

OnlyEVwithFATdamagecompare <- full_join(OnlyEVwithFatdamagewithID, matchefat4withID)

colnames(OnlyEVwithFATdamagecompare)[5] <- "Event"

OnlyEVwithFATdamagecompare2 <-
  OnlyEVwithFATdamagecompare %>%
    filter(NrFatalities > 60) %>%
      arrange(desc(NrFatalities))

library(xtable)
library(htmltools)
library(htmlTable)
xtfat <- xtable(OnlyEVwithFATdamagecompare2)
htmlTable(xtfat, caption= "Were the Events well matched?")

OnlyEVwithFATdamagecompareimproved <- 
  OnlyEVwithFATdamagecompare %>%
    mutate(Event = case_when(
      id == 95 ~ (Event = "Thunderstorm Wind"),
        TRUE ~ as.character(Event)))

xtfat1a <- xtable(OnlyEVwithFATdamagecompareimproved)
htmlTable(xtfat1a, caption= "Is it well replaced?")

OnlyEVwithFATdamagecompareimproved2 <-
  OnlyEVwithFATdamagecompareimproved %>%
    group_by(Event) %>%
      summarize(Fatalities= sum(NrFatalities), NrInjur = sum(NrInjuries)) %>%
        filter(Fatalities > 450) %>%
          arrange(desc(Fatalities))

OnlyEVwithFATdamagecompareimproved3 <-
  OnlyEVwithFATdamagecompareimproved %>%
    group_by(Event) %>%
      summarize(Fatalities= sum(NrFatalities), NrInjur = sum(NrInjuries)) %>%
        arrange(desc(NrInjur))


xtfat2 <- xtable(OnlyEVwithFATdamagecompareimproved2)
htmlTable(xtfat2, caption= "Which Events had more than 450 fatalities?")


## Create Plot
library(ggplot2)
library(gridExtra)
ggplot(data=OnlyEVwithFATdamagecompareimproved2, aes(Event, Fatalities, fill = Event)) + geom_bar(stat = "identity")



####### Now the same for injuries

## Only with 1 or more fatalities
OnlyEVwithInjdamage <-
  WhichEVdoesmostdamage %>%
  filter(NrInjuries > 0)

## Only FAT event types
UniqueInjEvents <- unique(OnlyEVwithInjdamage$EVTYPE) 
vectorEVInj <- as.vector(unlist(OnlyEVwithInjdamage['EVTYPE']))

## First lower casing so we can better match this.
vectorEVInj2 <- lapply(vectorEVInj, tolower)

matchesinj <- lapply((vectorEVInj2), FindMatch, stringVector=vectorEvents)

matchesinj2 <- as.vector(unlist(matchesinj))

matchesinj3 <- vectorEvents[(matchesinj2)]

matchesinj4 <- data.frame(matrix(unlist(matchesinj3 ), nrow=106, byrow=T),stringsAsFactors=FALSE)

matchesinj4withID <-
  matchesinj4  %>%
    mutate(id = row_number())

## Add ID's to the dataframe and list so I can merge this by ID
OnlyEVwithINjdamagewithID <-
  OnlyEVwithInjdamage %>%
    mutate(id = row_number())

OnlyEVwithInjdamagecompare <- full_join(OnlyEVwithINjdamagewithID , matchesinj4withID)

colnames(OnlyEVwithInjdamagecompare)[5] <- "Event"

OnlyEVwithInjdamagecompare2 <-
  OnlyEVwithInjdamagecompare %>%
  filter(NrInjuries > 60) %>%
  arrange(desc(NrFatalities))

library(xtable)
library(htmltools)
library(htmlTable)
xtinj <- xtable(OnlyEVwithInjdamagecompare2)
htmlTable(xtinj, caption= "Are the EV Types for the Injuries well matched?")


OnlyEVwithInjdamagecompareimproved <- 
  OnlyEVwithInjdamagecompare %>%
  mutate(OriginalEventTypes = case_when(
    id == 89 ~ (Event = "Thunderstorm Wind"),
    id == 92 ~ (Event = "Thunderstorm Wind"),
    id == 26 ~ (Event = "Dense Fog"),
    id == 30 ~ (Event = "Frost/Freeze"),
    TRUE ~ as.character(Event)))



OnlyEVwithInjdamagecompareimproved2 <-
  OnlyEVwithInjdamagecompareimproved  %>%
   group_by(Event) %>%
     summarize(NrFat= sum(NrFatalities), Injuries = sum(NrInjuries)) %>%
       filter(Injuries > 2500) %>%
        arrange(desc(Injuries))

OnlyEVwithInjdamagecompareimproved3 <-
  OnlyEVwithInjdamagecompareimproved  %>%
    group_by(Event) %>%
        summarize(NrFat= sum(NrFatalities), NrInjur = sum(NrInjuries)) %>%
            arrange(desc(NrInjur))



xtinj2 <- xtable(OnlyEVwithInjdamagecompareimproved2)
htmlTable(xtinj2, caption= "Which events had the highest number of injuries?")


library(ggplot2)
library(gridExtra)
p1 <- ggplot(data=OnlyEVwithFATdamagecompareimproved2, aes(Event, Fatalities, fill = Event)) + geom_bar(stat = "identity")
p2 <- ggplot(data=OnlyEVwithInjdamagecompareimproved2, aes(Event, Injuries, fill = Event)) + geom_bar(stat = "identity")
grid.arrange(p1, p2, nrow = 2, top='Which Events (based on NOAA) are the most harmful with respect to population health')


## Need an extra with combined?
## Add data for all the others? or just below?
## Adjust labels on the Axises.

## What is total of Fatalities?
## What is the total of Injuries?
  
Piechart?
  
library(plotmatrix)
slices <- c(OnlyEVwithInjdamagecompareimproved3$NrInjur)
lbls <- c(OnlyEVwithInjdamagecompareimproved3$OriginalEventTypes)
pie(slices, labels=lbls, main="Which events have the most injuries")

str(OnlyEVwithInjdamagecompareimproved3$NrInjur)


ggplot(data=OnlyEVwithInjdamagecompareimproved3, aes(OriginalEventTypes, NrFat, fill = OriginalEventTypes)) + geom_treemap(width = 1) + coord_polar("y")

p3 <-


library(treemap)

par(mfrow=c(1,2))
t1 <- treemap(OnlyEVwithInjdamagecompareimproved3,
        index=c("OriginalEventTypes"),
        vSize = "NrInjur",
        type = "index",
        palette = "YlOrRd",
        title="Injuries by Event Type",
        fontsize.title = 14)
t2 <- treemap(OnlyEVwithFATdamagecompareimproved3,
        index=c("OriginalEventTypes"),
        vSize = "NrFat",
        type = "index",
        palette = "Reds",
        title="Fatalities by Event Type",
        fontsize.title = 14)


library(ggplot2)

ggplot(data=OnlyEVwithInjdamagecompareimproved3, aes(OriginalEventTypes, NrFat, fill = OriginalEventTypes)) + geom_treemap(width = 1) + coord_polar("y")


library(treemapify)
ggplot2::ggplot(OnlyEVwithInjdamagecompareimproved3, ggplot2::aes(area=NrFat, fill = OriginalEventTypes))+ geom_treemap()
## Works but looks crap

## https://stackoverflow.com/questions/18400771/multiple-r-treemaps-on-a-single-page-with-scaling


##################################
####### Second question: Across the United States, which types of events have the greatest economic consequences?
###################################

## Only the data with damage and events

Stormdataoneconomics <-
  Dataafter1996 %>%
    select(BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP ,CROPDMG, CROPDMGEXP)


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


### Let's first focus on the most Property damage

MostPropDamage <-
  Valuesstormdataoneconomics4 %>%
  group_by(EVTYPE) %>%
  summarize(PropDamage = sum(realpropvalue)) %>% 
  filter(PropDamage > 0) %>%
  arrange(desc(PropDamage))

vectorPropdam <- as.vector(unlist(MostPropDamage['EVTYPE']))

## First lower casing so we can better match this.
vectorPropdam2 <- lapply(vectorPropdam, tolower)

matchesprop <- lapply((vectorPropdam2), FindMatch, stringVector=vectorEvents)

matchesprop2 <- as.vector(unlist(matchesprop))

matchesprop3 <- vectorEvents[(matchesprop2)]

matchesprop4 <- data.frame(matrix(unlist(matchesprop3), nrow=53, byrow=T),stringsAsFactors=FALSE)

matchesprop4withID <-
  matchesprop4  %>%
    mutate(id = row_number())

## Add ID's to the dataframe and list so I can merge this by ID
MostPropDamagewithID <-
  MostPropDamage %>%
    mutate(id = row_number())

OnlyEVwithPropdamagecompare <- full_join(MostPropDamagewithID, matchesprop4withID)

colnames(OnlyEVwithPropdamagecompare)[5] <- "Event"

OnlyEVwithPropdamagecompare2 <-
  OnlyEVwithPropdamagecompare %>%
    filter(PropDamage > 250000) %>%
      arrange(desc(PropDamage))


xtprop <- xtable(OnlyEVwithPropdamagecompare2)
htmlTable(xtprop, caption= "How did the matching go?")

OnlyEVwithPropdamagecompareimproved <- 
  OnlyEVwithPropdamagecompare %>%
  mutate(Event = case_when(
    id == 6 ~ (Event = "Frost/Freeze"),
    id == 14 ~ (Event = "Heavy Snow"),
    id == 15 ~ (Event = "Landslump"),
    TRUE ~ as.character(Event)))

WhichEventwithmostPropDamage <- 
  OnlyEVwithPropdamagecompare %>%
    group_by(Event) %>%
      summarize(PropertyDamage = sum(PropDamage)) %>% 
         filter(PropertyDamage > 250000) %>%
            arrange(desc(PropertyDamage))


xtprop2 <- xtable(WhichEventwithmostPropDamage)
htmlTable(xtprop2, caption= "Which Event did the most damage to properties?")


library(treemap)
treemap(WhichEventwithmostPropDamage,
        index=c("Event"),
        vSize = "PropertyDamage",
        type = "index",
        palette = "Reds",
        title="Total Property Damage by Event Type",
        fontsize.title = 14)



#####################################
########## Crop Damage
##########################

MostCropDamage <-
  Valuesstormdataoneconomics4 %>%
  group_by(EVTYPE) %>%
  summarize( CropDamage = sum(realcropvalue)) %>% 
  filter(CropDamage > 0) %>%
  arrange(desc(CropDamage))


MostCropDamagewithEvent <- 
  MostCropDamage %>%
    mutate(Event=EVTYPE)

MostCropDamagewImproved <- 
  MostCropDamagewithEvent %>%
  mutate(Event = case_when(
    EVTYPE == "THUNDERSTORM WIND" ~ (Event = "Thunderstorm Wind"),
    EVTYPE == "Early Frost"  ~ (Event = "Frost/Freeze"),
    EVTYPE == "Damaging Freeze"  ~ (Event = "Frost/Freeze"),
    EVTYPE == "AGRICULTURAL FREEZE" ~ (Event = "Frost/Freeze"),
    EVTYPE == "Freeze"  ~ (Event = "Frost/Freeze"),
    EVTYPE == "UNSEASONAL RAIN" ~ (Event = "UNSEASONAL RAIN"),
    EVTYPE == "Unseasonable Cold" ~ (Event = "Frost/Freeze"),
    EVTYPE == "Heavy Rain/High Surf"  ~ (Event = "High Surf"),
    EVTYPE == "Frost/Freeze" ~ (Event = "Frost/Freeze"),
    EVTYPE == "MARINE THUNDERSTORM WIND"  ~ (Event = "Marine Thunderstorm Wind"),
    TRUE ~ as.character(Event)))

WhichEventwithmostCropDamage <- 
  MostCropDamagewImproved %>%
  group_by(Event) %>%
  summarize(CropDamage = sum(CropDamage)) %>% 
      arrange(desc(CropDamage))


xtCrop <- xtable(WhichEventwithmostCropDamage)
htmlTable(xtCrop2, caption= "Which Event did the most damage to Crop?")


#### The plots

library(treemap)
treemap(WhichEventwithmostCropDamage,
        index=c("Event"),
        vSize = "CropDamage",
        type = "index",
        palette = "Blues",
        title="Total Property Damage by Event Type",
        fontsize.title = 14)



## No use to use these plots.
p3 <- ggplot(data=WhichEventwithmostPropDamage, aes(Event, PropertyDamage, fill = Event)) + geom_bar(stat = "identity")
p4 <- ggplot(data=WhichEventwithmostCropDamage, aes(Event, CropDamage, fill = Event)) + geom_bar(stat = "identity")
grid.arrange(p3, p4, nrow = 2, top='Which Events (based on NOAA) have the greatest economic consequences?')









#######################################################################################
## Can I create a function to match these 
#####################################################################################
library(stringdist)
library(dplyr)
?amatch

ClosestMatch3 = function(string, stringVector) {
  amatch(string, stringVector, method='jw', maxDist = 15, p=0.1)
}

matchesx2 <- lapply((vectorEVFAT2), ClosestMatch3, stringVector=vectorEvents)
str(matchesx2)

matchesx3 <- as.vector(unlist(matchesx2))
str(matchesx3)

matchesx2real <- vectorEvents[(matchesx3)]

matches10real <- data.frame(matrix(unlist(matchesx2real ), nrow=109, byrow=T),stringsAsFactors=FALSE)

matches10realwithID <-
  matches10real %>%
  mutate(id = row_number())

AOnlyEVwithFATdamagecompare <- full_join(OnlyEVwithFatdamagewithID, matches10realwithID)


write.csv(AOnlyEVwithFATdamagecompare, file="comparejw15.csv")

?write.csv

####################

library(stringr)
x <- c("Cat", "CAT", "cAt") 
b <- str_view(x, "cat")
c <- str_view(x, regex("cat", ignore_case = TRUE))

b
c
grep("stringofinterest",names(dataframeofinterest),ignore.case=TRUE,value=TRUE)
d <- grep("cat",names(x),ignore.case=TRUE,value=TRUE)
d
e <- regexpr("LIGHTNING",names(vectorEvents),ignore.case=TRUE)

vectorEvents 
vectorEVFAT

ClosestMatch2 = function(string, stringVector){
  
  distance = amatch(string, stringVector);
  stringVector[distance == max(distance)]
}

matchesx1 <- lapply((vectorEVFAT), ClosestMatch2, stringVector=vectorEvents)





## Created this function to get the best match


library(stringdist)
?amatch

ClosestMatch3 = function(string, stringVector) {
  amatch(string, stringVector, method='lv', maxDist = 100)
}

matchesx2 <- lapply((vectorEVFAT2), ClosestMatch3, stringVector=vectorEvents)
str(matchesx2)

matchesx3 <- as.vector(unlist(matchesx2))
str(matchesx3)

matchesx2real <- vectorEvents[(matchesx3)]
  
matches10real <- data.frame(matrix(unlist(matchesx2real ), nrow=109, byrow=T),stringsAsFactors=FALSE)

matches10withID <-
  matches10 %>%
  mutate(id = row_number())





  
replace(matchesx2, matchesx3, stringEvents)

str(stringEvents)
?replace


ClosestMatch4 = function(string, stringVector) {
  match(string, stringVector)
}

matchesx3 <- lapply((vectorEVFAT), ClosestMatch4, stringVector=vectorEvents)

Results = function(a, b) {
    
}


##############################################################################
## Use this function to do a match
library(RecordLinkage)

ClosestMatch = function(string, stringVector){
  
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}

vectorEvents <- as.vector(unlist(stringEvents['V1']))
vectorEVFAT <- as.vector(unlist(OnlyEVwithFatdamage['EVTYPE']))

## First lower casing so we can better match this.
vectorEVFAT2 <- lapply(vectorEVFAT, tolower)

matches <- lapply((vectorEVFAT2), ClosestMatch, stringVector=vectorEvents)
matches2 <- lapply(matches, '[[',1)

## Which is better? -> no difference
lastValue <- function(x)   tail(x[!is.na(x)], 1)
matches2a <- lapply(matches, lastValue)


## Add ID's to the dataframe and list so I can merge this by ID

OnlyEVwithFatdamagewithID <-
  OnlyEVwithFatdamage %>%
  mutate(id = row_number())


matches10 <- data.frame(matrix(unlist(matches2), nrow=109, byrow=T),stringsAsFactors=FALSE)
matches10a <- data.frame(matrix(unlist(matches2a), nrow=109, byrow=T),stringsAsFactors=FALSE)

## Add ID's to the list
matches10withID <-
  matches10 %>%
  mutate(id = row_number())

matches10withIDa <-
  matches10a %>%
  mutate(id = row_number())


OnlyEVwithFATdamagecompare <- full_join(OnlyEVwithFatdamagewithID, matches10withID)
OnlyEVwithFATdamagecompare2 <- full_join(OnlyEVwithFatdamagewithID, matches10withIDa)
##############################################################################################


