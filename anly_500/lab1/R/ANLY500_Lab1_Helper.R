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


