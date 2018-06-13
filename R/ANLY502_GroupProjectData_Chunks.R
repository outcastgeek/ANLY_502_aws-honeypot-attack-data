
## @knitr installLibraries

#install.packages("knitr")
#install.packages("printr")
#install.packages("formatR")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("lattice")
#install.packages("lmtest")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("lubridate")
#install.packages("forecast")
#install.packages("nnet")

## @knitr loadLibraries

library(readxl)
library(ggplot2)
library(stats)
library(pastecs)
library(dplyr)
library(maps)
#library(mapdata)
library(lubridate)
library(forecast)
library(nnet)

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
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)
HoneypotAttack <- HoneypotAttack[,1:15]

## @knitr attacksData

HoneypotAttack_us <- HoneypotAttack %>%
  filter(host %in% c("groucho-oregon","groucho-us-east", "groucho-norcal"))

attacksCountData <- HoneypotAttack %>%
  group_by(country) %>%
  mutate(ATTACKS_COUNT = n(), region=country) %>%
  filter(row_number()==1)
colnames(attacksCountData)

top15CountriesAttacksCountData <- attacksCountData %>%
  select(country, ATTACKS_COUNT) %>%
  arrange(desc(ATTACKS_COUNT)) %>%
  head(n=15)

protoCountData <- HoneypotAttack %>%
  group_by(country) %>%
  mutate(PROTO_COUNT = n(), region=proto) %>%
  filter(row_number()==1)
colnames(protoCountData)

## @knitr locationOfAttacks

ggplot(HoneypotAttack_us, aes(x=host)) +
  geom_bar() +
  labs(x = "Host", y = "Count", title = "Histogram of host locations in US") #Histogram of attacks according to US locations

## @knitr forecastPrediction

time <- mdy_hm(HoneypotAttack$datetime) #convert datetime to Date
time_num <- hour(time) + minute(time)/60  #extract hour and minute and convert it to numeric
time_num <- data.frame(time_num)
timepred <- auto.arima(time_num) #time series prediction

plot(forecast(timepred, h=20), main = "Forecast of next attack time") #plot of forecast result
forecast(timepred, h=5) #forecast result, seeing the Forecast, you need to convert the numeric to Date. eg 5.982467 is 5:59. Therefore, the next attack will be at 5:59

## @knitr locationPrediction

split <- round(nrow(HoneypotAttack)*0.8)
training <- HoneypotAttack[1:split,]
testing <- HoneypotAttack[(split + 1):nrow(HoneypotAttack),] # split data into train data and test data

locationpred <- multinom(host ~ src + spt + dpt, data = training) #regression model

z <- summary(locationpred)$coefficients/summary(locationpred)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2 #p-value of each variable
p

prediction <- predict(locationpred, newdata = testing, "probs") #the prediction result shows the probability of the attack occuring at each locations
prediction

attacklocationprob <- data.frame(colMeans(prediction,na.rm = TRUE))
attacklocationprob <- cbind(rownames(attacklocationprob), data.frame(attacklocationprob, row.names=NULL))
colnames(attacklocationprob) <- c("location","probability") #the average probability of the attack occuring at each locations
attacklocationprob

attacklocationprob$location

ggplot(attacklocationprob, aes(x = attacklocationprob$location, y = attacklocationprob$probability)) +
  geom_boxplot()

## @knitr locationPredictionPlot

#locationpred <- readRDS(file = 'locationpred.RDS')
#locationpred <- 'locationpred.RDS' %>%
#  fullFilePath %>%
#  readRDS
#summary(locationpred)

attacklocationprob <- readRDS(file = 'attacklocationprob.RDS')
#attacklocationprob <- 'attacklocationprob.RDS' %>%
#  fullFilePath %>%
#  readRDS

ggplot(attacklocationprob, aes(x = attacklocationprob$location, y = attacklocationprob$probability)) +
  geom_boxplot()

## @knitr attacksByFacet

ggplot(protoCountData, aes(x = proto, y = PROTO_COUNT, fill=proto)) +
  geom_bar(stat="identity") +
  ggtitle("Total Attacks Count by Protocol")

ggplot(top15CountriesAttacksCountData, aes(x = country, y = ATTACKS_COUNT, fill=country)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Top 15 Countries Total Attacks Count")

## @knitr mappingData

world <- map_data("world")
colnames(world)

attacksCountMapData <- world %>% inner_join(., attacksCountData)
colnames(attacksCountData)

## @knitr mapPlots

ggplot() +
  geom_map(aes(x=long, y=lat, group=group, map_id=region),
           data=world, map=world, fill="white", colour="#7f7f7f", size=0.5) +
  geom_point(aes(x = longitude, y = latitude, size = ATTACKS_COUNT), data = attacksCountData, alpha = 0.8) +
  ggtitle("Attacks from 9:53pm on Mar 3rd to 5:55am on Sept 8th of year 2013")

