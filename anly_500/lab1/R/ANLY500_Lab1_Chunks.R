
## @knitr installLibraries

## install.packages("knitr")
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

# Adjusts the Region Data for Plotting
regionData <- function(DFrame, lRow, hRow, lCol, hCol, cols1,cols2) {
  dFrame_NA <- DFrame[lRow:hRow,]
  tDFrame_NA <- t(dFrame_NA[,lCol:hCol])
  tDFrame_NA
  colnames(tDFrame_NA) <- cols1
  tDFrame_NA

  data.m2 <- melt(tDFrame_NA, id.vars=var1)
  colnames(data.m2) <- cols2

  data.m2
}

