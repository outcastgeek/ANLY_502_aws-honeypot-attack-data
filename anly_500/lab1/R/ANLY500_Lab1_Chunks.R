
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

