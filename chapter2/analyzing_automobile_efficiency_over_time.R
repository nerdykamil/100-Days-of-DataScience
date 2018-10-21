install.packages("plyr")
install.packages("ggplot2")
install.packages("reshape2")

library(plyr)
library(ggplot2)
library(reshape2)

# set the working directory
setwd("/home/lfd/courses/Practical_datascience_cookbook/chapter2")

# reading data from zip file, as we the know the filename inside
vehicles <- read.csv(unz("vehicles.csv.zip","vehicles.csv"),stringsAsFactors = F)
# display first few rows
head(vehicles)

# reading labels
# labels <- read.table("varlabels.txt",sep = "-", header = FALSE) #doesnt work bcz multiple - found in one line

labels <- do.call(rbind,strsplit(readLines("varlabels.txt")," - "))

# DATA EXPLORATION

# number of rows and cols in data
nrow(vehicles)
ncol(vehicles)

# names of labels
names(vehicles)

# finding how many years of data in this
length(unique(vehicles[,"year"]))
# length(unique(vehicles$year)) #use this line or above line to find number of unique years

# finding min and max year in data
min(vehicles[,"year"])
max(vehicles[,"year"])

# finding primary fueltype in all automobiles
table(vehicles$fuelType1)

# explore transmission used by automobiles
head(vehicles$trany)
table(vehicles$trany)
#setting all blacks to NA
vehicles$trany[vehicles$trany == ""] <- NA
# setting auto and manual to trany2 as that is needed only
vehicles$trany2 <- ifelse(substr(vehicles$trany,1,4) == "Auto", "Auto", "Manual")
# convert trany2 variables to factor and see count
vehicles$trany <- as.factor(vehicles$trany)
table(vehicles$trany2)

# finding vehicles that have sChargers over the year range given
with(vehicles,table(sCharger,year))

# finding datatypes of super charger and turbo charger columns
class(vehicles$sCharger)
class(vehicles$tCharger)

# finding unique values in above 2 columns
unique(vehicles$sCharger)
unique(vehicles$tCharger)  # this shows that even though this col has "" and "T" values but R reads T as True

## exploration part-2 finding fuel efficiency over time with other data points

# on avg how MPG changes over time
mpgByYr <- ddply(vehicles,~year,summarize,avgMPG = mean(comb08),avgHghy = mean(highway08), avgCity = mean(city08))
head(mpgByYr)

# plotting using ggplot, mpgbyyr over years and getting a shaded region of smoothed conditional mean
ggplot(mpgByYr, aes(year,avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("All Cars")

# the graph can be misleading bcz in recent years many hybrid cars have been launched
table(vehicles$fuelType1)

# to only consider gasoline cars lets make a subset
gasCars <- subset(vehicles, fuelType1 %in% c("Regular Gasoline", "Premium Gasoline","Midgrade Gasoline")
                  & fuelType2 == "" & atvType != "Hybrid")
mpgByYr_Gas <- ddply(gasCars,~year,summarize,avgMPG = mean(comb08),avgHghy = mean(highway08), avgCity = mean(city08))
ggplot(mpgByYr_Gas, aes(year,avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Gasoline Cars")