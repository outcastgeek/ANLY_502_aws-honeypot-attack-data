# This is the helper Script for Lab 1


#Setup X11 Display

x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
setwd("~/datascience_workspace/MS_Analytics/anly_500/lab1")
getwd()

#Set Library Packages Folder
.libPaths()
.libPaths( c( .libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.4") )
.libPaths()

#Install package readxl
#install.packages("readxl")

#Install package ggplot2
#install.packages("ggplot2")

#Install package pastecs
install.packages("pastecs")

#Install package lattice
install.packages("lattice")


loadExcelFileFromCurrentFolder <- function(fileName)
{
  fileFolder <- "./"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  dataFrame <- read_excel(fileNamePath)
  dataFrame
}


