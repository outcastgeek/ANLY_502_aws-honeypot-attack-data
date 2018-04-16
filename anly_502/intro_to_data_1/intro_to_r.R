
#Setup X11 Display
x11(width = 3, height = 3)
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY"=":0")
capabilities()


# Instruct R to fetch Arbuthnot baptism counts for boys and girls from OpenIntro Website
source("http://www.openintro.org/stat/data/arbuthnot.R")


# View the Data: Dr. Arbuthnot's Baptism Records'
arbuthnot
View(arbuthnot)
dim(arbuthnot) # Should be 82 rows and 3 columns

names(arbuthnot) # Columns names

# Some Exploration

arbuthnot$boys # Boys column (Number of boys baptized each year)

# Plot of girls baptized per year
plot(x <-  arbuthnot$year, y <-  arbuthnot$girls) # Default = Scattered Plot

?plot # Check Plotting Options

plot(x <-  arbuthnot$year, y <-  arbuthnot$girls, type = "l") # Line Plot

arbuthnot$boys + arbuthnot$girls # All baptisms count for each year

plot(arbuthnot$year, arbuthnot$boys + arbuthnot$girls, type = "l") # Plot of all baptisms per year

plot(sin, -pi, 2*pi)
