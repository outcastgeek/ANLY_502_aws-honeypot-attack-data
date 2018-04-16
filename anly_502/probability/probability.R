
#Setup X11 Display
x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()

#Set Working Directory
setwd("~/datascience_workspace/MS_Analytics/anly_502/probability")
getwd()

# Instruct R to download the data of performance of player Kobe Bryant of LA Lakers from OpenIntro
download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")

# Load and Explore the Data
load("kobe.RData")

head(kobe)

# In Game 1 Kobe had the following sequence of hits and misses
kobe$basket[1:9]

# All shooting streaks from custom function calc_streak
kobe_streak <- calc_streak(kobe$basket)

?barplot

barplot(table(kobe_streak))

