
## @knitr installLibraries

#install.packages("knitr")
#install.packages("printr")
#install.packages("formatR")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("lattice")
#install.packages("lmtest")
## install.packages("maps")
## install.packages("mapdata")

## @knitr loadLibraries

library(readxl)
library(ggplot2)
library(stats)
library(pastecs)
library(dplyr)
library(maps)
library(mapdata)

## @knitr helperFunctions

# Obtains the full File Path
fullFilePath <- function(fileName)
{
  fileFolder <- "./"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  fileNamePath
}

# Creates the transpose of a section of a Data Frame
sectionTranspose <- function(DFrame, lRow, hRow, lCol, hCol) {
  dFrame <- DFrame[lRow:hRow,]
  tdFrame <- t(dFrame[,lCol:hCol])
  tdFrame
}

# Adjusts the Transposed Data for Plotting
tGraphData <- function(tDFrame, cols1,cols2) {
  colnames(tDFrame) <- cols1
  data.m2 <- melt(tDFrame, id.vars=var1)
  colnames(data.m2) <- cols2
  
  data.m2
}

# Creates the transpose of a chunk of a Data Frame
chunkTranspose <- function(DFrame, lRow, hRow, lCol, hCol) {
  tdFrame <- t(DFrame[lRow:hRow,lCol:hCol])
  tdFrame
}

# Converts column of Timestamps to Date
ttColToDate <- function(dFrame, colName) {
  dFrame[colName] <- as.POSIXct(dFrame[colName], origin="1970-01-01")
  dFrame
}

# Converts column to utf-8
toUtf8 <- function(column) {
  columnUtf8 <- iconv(enc2utf8(column), sub = "byte")
  columnUtf8
}

## @knitr loadSheets

#Set Data File Name:
attackDataFile <- "AWS_Honeypot_marx-geo.csv"

# Honeypot Attack
HoneypotAttack <- attackDataFile %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=FALSE, stringsAsFactors=FALSE)

colnames(HoneypotAttack) <- c("datetime", "host", "src", "proto", "type", "spt",
                              "dpt", "srcstr", "cc", "country", "locale", "localeabbr",
                              "postalcode", "latitude", "longitude", "NA")
HoneypotAttack$longitude <- HoneypotAttack$longitude %>% toUtf8 %>% as.double
HoneypotAttack$latitude <- HoneypotAttack$latitude %>% toUtf8 %>% as.double
HoneypotAttack$spt <- HoneypotAttack$spt %>% toUtf8 %>% as.double

HoneypotAttack <- HoneypotAttack %>%
  select(c("datetime", "host", "src", "proto", "type", "spt",
           "dpt", "srcstr", "cc", "country", "locale", "localeabbr",
           "postalcode", "latitude", "longitude"))

sampleHoneypotAttack <- sample_n(HoneypotAttack, 1000)

## @knitr attacksData

#attacksData <- data.frame(COUNTRY = HoneypotAttack$country,
#                         SPT = HoneypotAttack$spt,
#                         PROTO = HoneypotAttack$proto,
#                         LONGITUDE = HoneypotAttack$longitude,
#                         LATITUDE = HoneypotAttack$latitude)
#attacksData <- tail(attacksData,-1)

attacksData <- HoneypotAttack %>%
  group_by(country) %>%
  mutate(ATTACKS_COUNT = n(), region=country) %>%
  filter(row_number()==1)

## @knitr protocolAttacks

ggplot(sampleHoneypotAttack, aes(x = country, y = spt, fill=proto)) +
  geom_bar(stat="identity") +
  ggtitle("Attacks by Protocol")

#ggplot(HoneypotAttack, aes(x = country, y = spt, fill=proto)) +
#  geom_bar(stat="identity") +
#  ggtitle("Attacks by Protocol")

dd <- attacksData %>%
  filter(country %in% c("China", "Russia", "France", "south korea", "Germany"))

ggplot(dd, aes(x = country, y = spt, fill=proto)) +
  geom_bar(stat="identity") +
  ggtitle("Attacks by Protocol China, Russia, France, south korea, Germany")

## @knitr mappingData

world <- map_data("world")
colnames(world)

#attacksMapData <- attacksData
#attacksMapData$region <- attacksData$COUNTRY

attacksMapData <- world %>% inner_join(., attacksData)
colnames(attacksMapData)

## @knitr mapPlots

#ggplot() +
#  geom_polygon(aes(x=long, y=lat, group=group, fill=region), attacksMapData, colour = "black") +
#  geom_point(aes(x = longitude, y = latitude, size = ATTACKS_COUNT), data = attacksMapData, alpha = 0.8) +
#  scale_size_area() +
#  coord_quickmap() +
#  ggtitle("Attacks by Location")

ggplot() +
  geom_map(aes(x=long, y=lat, group=group, map_id=region),
           data=world, map=world, fill="white", colour="#7f7f7f", size=0.5) +
  geom_point(aes(x = longitude, y = latitude, size = ATTACKS_COUNT), data = attacksMapData, alpha = 0.8) +
  ggtitle("Attacks from 9:53pm on Mar 3rd to 5:55am on Sept 8th of year 2013")

