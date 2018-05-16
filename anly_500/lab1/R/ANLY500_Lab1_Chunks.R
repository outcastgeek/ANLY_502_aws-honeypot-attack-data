
## @knitr installLibraries

## install.packages("knitr")
## install.packages("printr")
## install.packages("formatR")
## install.packages("readxl")
## install.packages("ggplot2")
## install.packages("pastecs")
## install.packages("psych")
## install.packages("Hmisc")
## install.packages("lattice")
## install.packages("lmtest")
## install.packages("BSDA")

## @knitr loadLibraries

library(readxl)
library(ggplot2)
library(psych)
library(Hmisc)
library(stats)
#library(pastecs)
library(magrittr)
library(reshape2)
library(BSDA)
require(scales)

### @knitr samplePlot

# Define 2 vectors
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

# Graph cars using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0,12))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)


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

## @knitr loadSheets

perfFileName <- "Performance Lawn Equipment Database.xlsx"

DealerSat <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = " Dealer Satisfaction", skip = 1) #Skip 1 skips the first row

EndUserSat <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "End-User Satisfaction", skip = 1)

CustomerSurvey2014 <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "2014 Customer Survey", skip = 1)

Complaints <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Complaints", skip = 1)

MowerUnitSales <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Mower Unit Sales", skip = 1)

TractorUnitSales <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Tractor Unit Sales", skip = 1)

IndustryMowerTotalSales <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Industry Mower Total Sales", skip = 1)

IndustryTractorTotalSales <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Industry Tractor Total Sales", skip = 1)

UnitProductionCosts <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Unit Production Costs", skip = 1)

OperatingAndInterestExpenses <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Operating & Interest Expenses", skip = 1)

OnTimeDelivery <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "On-Time Delivery", skip = 1)

DefectsAfterDelivery <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Defects After Delivery", skip = 3)

TimeToPaySuppliers <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Time to Pay Suppliers", skip = 1)

ResponseTimesCSC <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Response Time", skip = 1)

EmployeeSatisfaction <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Employee Satisfaction", skip = 1)

EngineProductionTime <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Engines", skip = 1)

TransmissionCosts <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Transmission Costs", skip = 1)

BladeWeight <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Blade Weight")

MowerTest <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Mower Test", skip = 1)

EmployeeRetention <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Employee Retention", skip = 1)

UnitShippingCost <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Shipping Cost", skip = 1)

FixedCost <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Fixed Cost", skip = 1)

PurchasingSurvey <- perfFileName %>%
    fullFilePath %>%
    read_excel(sheet = "Purchasing Survey", skip = 1)

## @knitr satisfactionByRegionAxis

yearsCols <- c("2010", "2011", "2012", "2013", "2014")
dataTags <- c("Level", "Year", "Counts")


## @knitr dealerSatNorthAmerica

naDealerSat <- DealerSat %>%
        sectionTranspose(1,5, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(naDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(naDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")


## @knitr dealerSatSouthAmerica

saDealerSat <- DealerSat %>%
        sectionTranspose(6,10, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(saDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(saDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")


## @knitr dealerSatEurope

euDealerSat <- DealerSat %>%
        sectionTranspose(11,15, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(euDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(euDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")


## @knitr dealerSatPacificRim

prDealerSat <- DealerSat %>%
        sectionTranspose(16,20, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(prDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(prDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")


## @knitr dealerSatChina

yearsInChinaCols <- c("2012", "2013", "2014")

chDealerSat <- DealerSat %>%
        sectionTranspose(21,23, 3,8) %>%
        tGraphData(yearsInChinaCols,dataTags)
ggplot(chDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(chDealerSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")


## @knitr endUserSatNorthAmerica

naEndUserSat <- EndUserSat %>%
        sectionTranspose(1,5, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(naEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(naEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")

## @knitr endUserSatSouthAmerica

saEndUserSat <- EndUserSat %>%
        sectionTranspose(6,10, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(saEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(saEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")

## @knitr endUserSatEurope

euEndUserSat <- EndUserSat %>%
        sectionTranspose(11,15, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(euEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(euEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")

## @knitr endUserSatPacificRim

prEndUserSat <- EndUserSat %>%
        sectionTranspose(16,20, 3,8) %>%
        tGraphData(yearsCols,dataTags)
ggplot(prEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(prEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")

## @knitr endUserSatChina

yearsInChinaCols <- c("2012", "2013", "2014")

chEndUserSat <- EndUserSat %>%
        sectionTranspose(21,23, 3,8) %>%
        tGraphData(yearsInChinaCols,dataTags)
ggplot(chEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), position="dodge", stat="identity")
ggplot(chEndUserSat, aes(x=Year, y=Counts)) + geom_bar(aes(fill=Level), stat="identity")

## knitr mowerUnitSalesDataTags

musDataTags <- c("Region", "Month", "Sales")

## @knitr mowerUnitSales2010

mowerUnitSales2010 <- MowerUnitSales %>%
    chunkTranspose(1,12, 2,7) %>%
    tGraphData(MowerUnitSales$Month[1:12],musDataTags)
mowerUnitSales2010$Month <- as.POSIXct(mowerUnitSales2010$Month, origin="1970-01-01")
ggplot(mowerUnitSales2010, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr mowerUnitSales2011

mowerUnitSales2011 <- MowerUnitSales %>%
    chunkTranspose(13,24, 2,7) %>%
    tGraphData(MowerUnitSales$Month[13:24],musDataTags)
mowerUnitSales2011$Month <- as.POSIXct(mowerUnitSales2011$Month, origin="1970-01-01")
ggplot(mowerUnitSales2011, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr mowerUnitSales2012

mowerUnitSales2012 <- MowerUnitSales %>%
    chunkTranspose(25,36, 2,7) %>%
    tGraphData(MowerUnitSales$Month[25:36],musDataTags)
mowerUnitSales2012$Month <- as.POSIXct(mowerUnitSales2012$Month, origin="1970-01-01")
ggplot(mowerUnitSales2012, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr mowerUnitSales2013

mowerUnitSales2013 <- MowerUnitSales %>%
    chunkTranspose(37,48, 2,7) %>%
    tGraphData(MowerUnitSales$Month[37:48],musDataTags)
mowerUnitSales2013$Month <- as.POSIXct(mowerUnitSales2013$Month, origin="1970-01-01")
ggplot(mowerUnitSales2013, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr mowerUnitSales2014

mowerUnitSales2014 <- MowerUnitSales %>%
    chunkTranspose(49,60, 2,7) %>%
    tGraphData(MowerUnitSales$Month[49:60],musDataTags)
mowerUnitSales2014$Month <- as.POSIXct(mowerUnitSales2014$Month, origin="1970-01-01")
ggplot(mowerUnitSales2014, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

# ###########

## knitr tractorUnitSalesDataTags

tusDataTags <- c("Region", "Month", "Sales")

## @knitr tractorUnitSales2010

tractorUnitSales2010 <- MowerUnitSales %>%
    chunkTranspose(1,12, 2,7) %>%
    tGraphData(MowerUnitSales$Month[1:12],tusDataTags)
tractorUnitSales2010$Month <- as.POSIXct(tractorUnitSales2010$Month, origin="1970-01-01")
ggplot(tractorUnitSales2010, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr tractorUnitSales2011

tractorUnitSales2011 <- MowerUnitSales %>%
    chunkTranspose(13,24, 2,7) %>%
    tGraphData(MowerUnitSales$Month[13:24],tusDataTags)
tractorUnitSales2011$Month <- as.POSIXct(tractorUnitSales2011$Month, origin="1970-01-01")
ggplot(tractorUnitSales2011, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr tractorUnitSales2012

tractorUnitSales2012 <- MowerUnitSales %>%
    chunkTranspose(25,36, 2,7) %>%
    tGraphData(MowerUnitSales$Month[25:36],tusDataTags)
tractorUnitSales2012$Month <- as.POSIXct(tractorUnitSales2012$Month, origin="1970-01-01")
ggplot(tractorUnitSales2012, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr tractorUnitSales2013

tractorUnitSales2013 <- MowerUnitSales %>%
    chunkTranspose(37,48, 2,7) %>%
    tGraphData(MowerUnitSales$Month[37:48],tusDataTags)
tractorUnitSales2013$Month <- as.POSIXct(tractorUnitSales2013$Month, origin="1970-01-01")
ggplot(tractorUnitSales2013, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr tractorUnitSales2014

tractorUnitSales2014 <- MowerUnitSales %>%
    chunkTranspose(49,60, 2,7) %>%
    tGraphData(MowerUnitSales$Month[49:60],tusDataTags)
tractorUnitSales2014$Month <- as.POSIXct(tractorUnitSales2014$Month, origin="1970-01-01")
ggplot(tractorUnitSales2014, aes(x=Month, y=Sales)) + geom_bar(aes(fill=Region), stat="identity")

## @knitr responseTime

q1.13 <- data.frame(Quarters = "Q1 2013", Time = ResponseTimesCSC$`Q1 2013`)
q2.13 <- data.frame(Quarters = "Q2 2013", Time = ResponseTimesCSC$`Q2 2013`)
q3.13 <- data.frame(Quarters = "Q3 2013", Time = ResponseTimesCSC$`Q3 2013`)
q4.13 <- data.frame(Quarters = "Q4 2013", Time = ResponseTimesCSC$`Q4 2013`)

q1.14 <- data.frame(Quarters = "Q1 2014", Time = ResponseTimesCSC$`Q1 2014`)
q2.14 <- data.frame(Quarters = "Q2 2014", Time = ResponseTimesCSC$`Q2 2014`)
q3.14 <- data.frame(Quarters = "Q3 2014", Time = ResponseTimesCSC$`Q3 2014`)
q4.14 <- data.frame(Quarters = "Q4 2014", Time = ResponseTimesCSC$`Q4 2014`)

responseTimePlotData = rbind(q1.13,q2.13,q3.13,q4.13,q1.14,q2.14,q3.14,q4.14)

ggplot(responseTimePlotData, aes(x=Quarters, y=Time, fill=Quarters)) +  geom_boxplot()
