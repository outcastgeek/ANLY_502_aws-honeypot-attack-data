
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

## @knitr attacksData

attacksCountData <- HoneypotAttack %>%
  group_by(country) %>%
  mutate(ATTACKS_COUNT = n(), region=country) %>%
  filter(row_number()==1)
colnames(attacksCountData)

totalSptProtoData <- HoneypotAttack %>%
  group_by(proto) %>%
  mutate(SPT_TOTAL = sum(spt)) %>%
  filter(row_number()==1)

#attacksFirst20ProtoData <- attacksCountData %>%
#  slice(1:20)
#colnames(attacksFirst20ProtoData)

## @knitr protocolAttacks

ggplot(totalSptProtoData, aes(x = proto, y = SPT_TOTAL, fill=proto)) +
  geom_bar(stat="identity") +
  ggtitle("Total SPT by Protocol")

#ggplot(attacksFirst20ProtoData, aes(x = country, y = ATTACKS_COUNT, fill=proto)) +
#  geom_bar(stat="identity") +
#  ggtitle("Attacks by Protocol for First 20 Countries")

## @knitr mappingData

world <- map_data("world")
colnames(world)

attacksCountMapData <- world %>% inner_join(., attacksCountData)
colnames(attacksCountData)

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
  geom_point(aes(x = longitude, y = latitude, size = ATTACKS_COUNT), data = attacksCountData, alpha = 0.8) +
  ggtitle("Attacks from 9:53pm on Mar 3rd to 5:55am on Sept 8th of year 2013")

