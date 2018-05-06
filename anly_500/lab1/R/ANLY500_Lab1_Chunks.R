
## @knitr installLibraries

## install.packages("knitr")
## install.packages("printr")
## install.packages("formatR")
## install.packages("readxl")
## install.packages("ggplot2")
## install.packages("pastecs")
## install.packages("lattice")
## install.packages("lmtest")

## @knitr loadLibraries

library(readxl)
library(ggplot2)
library(stats)
library(pastecs)
library(reshape2)

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

# Load Sheet from Current Directory
loadSheetFromFile <- function(
                              fileName,
                              sheet=NULL,
                              skip=NULL
                              )
{
  fileFolder <- "./"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  dataFrame <- read_excel(
    fileNamePath,
    sheet = sheet,
    skip = skip
  )
  dataFrame
}

# Creates the transpose of a section of a Data Frame
sectionTranspose <- function(DFrame, lRow, hRow, lCol, hCol) {
    dFrame <- DFrame[lRow:hRow,]
    tdFrame <- t(dFrame[,lCol:hCol])
    tdFrame
}

# Adjusts the Region Data for Plotting
regionData <- function(DFrame, lRow, hRow, lCol, hCol, cols1,cols2) {
    tdFrame <- sectionTranspose(DFrame, lRow, hRow, lCol, hCol)
    colnames(tdFrame) <- cols1
    tdFrame

    data.m2 <- melt(tdFrame, id.vars=var1)
    colnames(data.m2) <- cols2

    data.m2
}

## @knitr loadSheets

perfFileName <- "Performance Lawn Equipment Database.xlsx"

DealerSat <- loadSheetFromFile(perfFileName, sheet = " Dealer Satisfaction", skip = 1)
EndUserSat <- loadSheetFromFile(perfFileName, sheet = "End-User Satisfaction", skip = 1)
CustomerSurvey2014 <- loadSheetFromFile(perfFileName, sheet = "2014 Customer Survey", skip = 1)
Complaints <- loadSheetFromFile(perfFileName, sheet = "Complaints", skip = 1)
MowerUnitSales <- loadSheetFromFile(perfFileName, sheet = "Mower Unit Sales", skip = 1)
TractorUnitSales <- loadSheetFromFile(perfFileName, sheet = "Tractor Unit Sales", skip = 1)
IndustryMowerTotalSales <- loadSheetFromFile(perfFileName, sheet = "Industry Mower Total Sales", skip = 1)
IndustryTractorTotalSales <- loadSheetFromFile(perfFileName, sheet = "Industry Tractor Total Sales", skip = 1)
UnitProductionCosts <- loadSheetFromFile(perfFileName, sheet = "Unit Production Costs", skip = 1)
OperatingAndInterestExpenses <- loadSheetFromFile(perfFileName, sheet = "Operating & Interest Expenses", skip = 1)
OnTimeDelivery <- loadSheetFromFile(perfFileName, sheet = "On-Time Delivery", skip = 1)
DefectsAfterDelivery <- loadSheetFromFile(perfFileName, sheet = "Defects After Delivery", skip = 1)
TimeToPaySuppliers <- loadSheetFromFile(perfFileName, sheet = "Time to Pay Suppliers", skip = 1)
ResponseTimesCSC <- loadSheetFromFile(perfFileName, sheet = "Response Time", skip = 1)
EmployeeSatisfaction <- loadSheetFromFile(perfFileName, sheet = "Employee Satisfaction", skip = 1)
EngineProductionTime <- loadSheetFromFile(perfFileName, sheet = "Engines", skip = 1)
TransmissionCosts <- loadSheetFromFile(perfFileName, sheet = "Transmission Costs", skip = 1)
BladeWeight <- loadSheetFromFile(perfFileName, sheet = "Blade Weight", skip = 1) #Skip 1 skips the first row
MowerTest <- loadSheetFromFile(perfFileName, sheet = "Mower Test", skip = 1)
EmployeeRetention <- loadSheetFromFile(perfFileName, sheet = "Employee Retention", skip = 1)
UnitShippingCost <- loadSheetFromFile(perfFileName, sheet = "Shipping Cost", skip = 1)
FixedCost <- loadSheetFromFile(perfFileName, sheet = "Fixed Cost", skip = 1)
PurchasingSurvey <- loadSheetFromFile(perfFileName, sheet = "Purchasing Survey", skip = 1)
