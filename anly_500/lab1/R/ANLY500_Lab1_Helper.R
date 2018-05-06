# This is the helper Script for Lab 1


#Setup X11 Display

#x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
#setwd("~/datascience_workspace/MS_Analytics/anly_500/lab1")
getwd()

#Set Library Packages Folder
.libPaths()
.libPaths( c( .libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.4") )
.libPaths()

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
