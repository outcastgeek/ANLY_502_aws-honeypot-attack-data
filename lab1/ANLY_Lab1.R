
#Setup X11 Display
x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
setwd("~/datascience_workspace/anly500/lab1")
getwd()

#Install package readxl
install.packages("readxl")

#Load package readxl
library(readxl)

## fileFolder <- "~/Downloads/"
fileFolder <- "./"
perfFileName <- paste(fileFolder, "Performance Lawn Equipment Database.xlsx", sep = "")

DealerSat <- read_excel(perfFileName, sheet = " Dealer Satisfaction", skip = 1)
View(DealerSat)
str(DealerSat) #Good way to quickly show the structure of something
summary(DealerSat)

EndUserSat <- read_excel(perfFileName, sheet = "End-User Satisfaction", skip = 1)
View(EndUserSat)
str(EndUserSat)

CustomerSurvey2014 <- read_excel(perfFileName, sheet = "2014 Customer Survey", skip = 1)

BladeWeight <- read_excel(perfFileName, sheet = "Blade Weight", skip = 1) #Skip 1 skips the first row

#View, Structure
View(CustomerSurvey2014)
str(CustomerSurvey2014)

View(BladeWeight)
str(BladeWeight)

#Next identify the N/A rows
is.na(BladeWeight$Sample)
is.na(BladeWeight$Weight)

#Next reverse tge vector of truefalse using a boolean-not operation
BladeWeight[!is.na(BladeWeight$Sample),]

#Delete the rows
BladeWeight = BladeWeight[!is.na(BladeWeight$Sample),]

#Verify it worked
summary(BladeWeight$Sample)

#Finally let's subset the data by region. ##
View(Dealer_Sat)
dealerSat_NA = Dealer_Sat[1:5,]
dealerSat_NA
View(dealerSat_NA)
head(dealerSat_NA)

tdealerSat_NA = t(dealerSat_NA[,3:8])
tdealerSat_NA
View(tdealerSat_NA)

colnames(tdealerSat_NA) <- c("2010", "2011", "2012", "2013", "2014")
head(tdealerSat_NA)
colnames(tdealerSat_NA)

