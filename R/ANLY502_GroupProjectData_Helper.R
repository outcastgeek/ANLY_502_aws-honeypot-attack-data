# This is the helper Script for Lab 1


#Setup X11 Display

#x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
#setwd("~/datascience_workspace/MS_Analytics/anly_502/group_work")
#setwd("/Users/outca/workspace/ms_analytics")
getwd()

#Set Library Packages Folder
.libPaths()
.libPaths( c( .libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.4") )
.libPaths()

# Load Sheet from Current Directory
loadSheetFromFile <- function(
                             fileName,
                             skip=NULL
                             )
{
  fileFolder <- "./"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  dataFrame <- read.csv(
    fileNamePath,
    skip = skip
  )
  dataFrame
}


