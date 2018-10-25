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

# finding why the curve is increasing towards end
# do we have fewer large engine cars now
typeof(gasCars$displ) #assuming gasCars$displ value is a char
gasCars$displ <- as.numeric(gasCars$displ) #converting it to numeric

ggplot(gasCars, aes(displ,comb08)) + geom_point() + geom_smooth() + xlab("displ") + ylab("comb08") + ggtitle("Gasoline Cars engine displacement")
## thus plot from above line tells us that small cars have greter fuel efficiency

avgCarSize <- ddply (gasCars, ~year, summarize , avgDispl = mean(displ))
ggplot(avgCarSize, aes(year,avgDispl)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Avg displ of engine") + ggtitle("car displ over years")

## plotting avg engine displacement and fuel efficiency on one plot
byYear <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08), avgDispl = mean(displ))
head(byYear)

# melting data frame to convert from wide to long format (columns year, avgdispl, 
# avgmpg converted to columns year, variable, value )
# where variable column shows if its avgdispl value or avgmpg value
byYear2 <- melt(byYear, id = "year")
head(byYear2)
levels(byYear$varianble) <- c("Average MPG", "Avg displ")

#use nrow to compare rows of byYear and byYear2
nrow(byYear)
nrow(byYear2)

# plot using ggplot (the plot below helps in identifying trends between carsize and avgMPG over years)
ggplot(byYear2, aes(year,value)) + geom_point() + geom_smooth() + facet_wrap(~variable, ncol =1, scales ="free_y") + xlab("Year") + ylab("")

# finding efficiencies of manual and automatic cars for four cylinder engines
gasCars4 <- subset(gasCars, cylinders == "4")

# the below plot is used to generate a boxplot of values that 
#show the distribution of values
ggplot(gasCars4, aes(factor(year),comb08)) + geom_boxplot() + facet_wrap(~trany2,ncol=1) + theme(axis.text.x = element_text(angle = 45)) + labs(x="Year",y = "MPG")

# now lets see the change in proportion of manual cars available each year
ggplot(gasCars4, aes(factor(year),fill = factor(trany2))) + geom_bar(position = "fi-ll")+ labs(x="Year",y="Propotion of cars",fill= "Transmission") + theme(axis.text.x = element_text(angle = 45)) + geom_hline(yintercept = 0.5,linetype=2)

## investigating the makes and models of cars with four cylinders
carsMake <- ddply(gasCars4,~year,summarise,numberOfMakes = length(unique(make)))

ggplot(carsMake, aes(year,numberOfMakes)) + geom_point() +labs(x = "Year",y = "number of available makes") + ggtitle( "Four cylinder cars")

# finding makes available every years
uniqMakes <- dlply(gasCars4,~year,function(x) unique(x$make))
# finding constants in every year
commonMakes <- Reduce(intersect,uniqMakes) #reduce is a higher order func and accepts another func as input
commonMakes

# finding fuel efficiency of these makers over time
carsCommonMakes4 <- subset(gasCars4, make %in% commonMakes)
avgMPG_commonMakes <- ddply(carsCommonMakes4, ~year+make,summarise,avgMPG = mean(comb08))

ggplot(avgMPG_commonMakes, aes(year,avgMPG)) + geom_line()+facet_wrap(~make,nrow =2)
