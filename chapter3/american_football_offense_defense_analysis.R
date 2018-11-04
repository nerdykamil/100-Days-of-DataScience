## below code will help us analyze US football team data and 
## simulate games and virtual winnings
## american football basics at: http://www.nfl.com/rulebook/beginnersguidetofootball

## Rough Data Science Pipeline

install.packages("XML")
install.packages("RSQLite")
install.packages("stringr")
install.packages("ggplot2")

library(XML)
library(RSQLite)
library(stringr)
library(ggplot2)


## Day 3 -  Acquiring and deleting football data
# acquire data from http://sports.yahoo.com/

year <-2013
url <- paste("http://sports.yahoo.com/nfl/stats/byteam?group=Offense& 
cat=Total&conference=NFL&year=season_",year,"&sort=530&old_cat 
egory=Total&old_group=Offense")

# reading data from url
Offense <- readHTMLTable(url, encoding = "UTF-8",colClasses = "character")[[7]]

url2 <- paste("http://sports.yahoo.com/nfl/stats/byteam?group=Defense& 
cat=Total&conference=NFL&year=season_",year,"&sort=530&old_cat 
egory=Total&old_group=Defense")

# reading data from url
Defense <- readHTMLTable(url2, encoding = "UTF-8",colClasses = "character")[[7]]
