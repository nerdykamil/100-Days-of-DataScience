## installing required packages

install.packages("XML")
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("zoo")

## loading packages
library(XML)
library(ggplot2)
library(plyr)
library(reshape2)
library(zoo)

## set working directory
setwd("/home/lfd/courses/100-Days-of-DataScience/chapter4")

## getting the data
# access the given link: http://finviz.com/screener.ashx?v=152&c=0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68
#url_to_open <- "http://finviz.com/export.ashx?v=152&c=0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68"
#url_to_open <- sprintf("http://finviz.com/export.ashx?v=152&c=%s",paste(0:68, collapse = ","))
#url_to_open <- "https://finviz.com/screener.ashx?v=152&r=41"
#finviz <- read.csv(url(url_to_open))
finviz <- read.csv("finviz.csv")
head(finviz)
summary(finviz)

# Ticker is unique for all companies
# Price is ongoing one share value in $
# volume tells recent no of shares transacted in a day
# Price/Earning ratio
# P/E Growth rtio is above divided by annual grwoth
# Shares Outstanding, total shares
# EPS expected rate shares will grow next year
# total debt/equity , less value more good
# beta volatality of share
# relative strength index, days in which price high to days in which price low

# cleaning stock data
# removing %$// so that fields are not considered as character
clean_numeric <- function(s) {
  s <- gsub("%|\\$|,|\\)\\(","",s)
  s <- as.numeric(s)
}

finviz <- cbind(finviz[,1:6],apply(finviz[,7:68],2,clean_numeric))

hist(finviz$Price,breaks=100,main = "Price Distribution", xlab= "Price")

# due to higher range of x because of some outliers the graph by above code is not so explanatory, hence we put a cap of x <150
hist(finviz$Price[finviz$Price<150],breaks=100,main = "Price Distribution", xlab= "Price")

# this shows that most of the stocks lie under $50, but they might differ according to sectors,
# so we group data w.r.t sectors

sector_avg_prices <- aggregate(Price~Sector,data = finviz,FUN="mean")

# changing column name of second column
colnames(sector_avg_prices)[2] <- "Sector_Avg_Price"

ggplot(sector_avg_prices,aes(x = Sector,y= Sector_Avg_Price,fill=Sector)) + geom_bar(stat="identity") + ggtitle("Sector Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

# this shows that tech has highest value lets see if we can verify this by finding industries and companies

industry_avg_prices <- aggregate(Price~Sector+Industry,data=finviz,FUN="mean")
industry_avg_prices <- industry_avg_prices[order(industry_avg_prices$Sector,industry_avg_prices$Industry),]

colnames(industry_avg_prices)[3] <- "Industry_Avg_Price"

industry_chart <- subset(industry_avg_prices,Sector== "Technology")
ggplot(industry_chart,aes(x=Industry,y=Industry_Avg_Price,fill=Industry)) + geom_bar(stat="identity")+theme(legend.position= "none")+ ggtitle("Industry Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

industry_chart <- subset(industry_avg_prices,Sector== "Financial")
ggplot(industry_chart,aes(x=Industry,y=Industry_Avg_Price,fill=Industry)) + geom_bar(stat="identity")+theme(legend.position= "none")+ ggtitle("Industry Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

# in Tech Application Software and in Financial Property Management are outliers
# we drill down more

company_chart <- subset(finviz,Industry == "Application Software")
ggplot(company_chart,aes(x= Company,y=Price,fill="Company")) + geom_bar(stat="identity") + theme(legend.position = "none") + ggtitle("Company Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

company_chart <- subset(finviz,Industry == "Property Management")
ggplot(company_chart,aes(x= Company,y=Price,fill="Company")) + geom_bar(stat="identity") + theme(legend.position = "none") + ggtitle("Company Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

# in actual data the culprit/outlier is Berkshire Hathaway in Financial Sector which moves 
# financial sector avg to 250 and industry to 2000 and has 172,000$ per share
# this data is less so it doesn't have Berkshire Hathaway but if there was we'd do following steps

finviz <- subset(finviz,Ticker!="BRK-A") # removing the row from data

sector_avg_prices <- aggregate(Price~Sector,data=finviz,FUN="mean")
colnames(sector_avg_prices)[2]  <- "Sector_Avg_Price"

ggplot(sector_avg_prices,aes(x=Sector,y=Sector_Avg_Price,fill=Sector)) + geom_bar(stat="identity") + ggtitle("Sector Avg Prices") + theme(axis.text.x = element_text(angle =90,hjust=1))

## whereever above aggreagte has been used we can replace it with ddply from plyr package as follows 
sector_avg_price <- ddply(finviz,"Sector",summarise,Price=mean(Price,na.rm=TRUE))

## Day 10
## Generating relative valuations
# intrinsic valuation - involves digging into financial statements of company 
# relative valuation - quickly provide a sense of how stock is valued
# to find relative valuation we find avg of sector and industry and then compare it againt each value
# to check whther the share has been undervalued
sector_avg <- melt(finviz,id="Sector")
sector_avg <- subset(sector_avg,variable%in%c("Price","P.E","PEG","P.S","P.B"))

#removing null records and making values numeric
sector_avg <- (na.omit(sector_avg))
sector_avg$value <- as.numeric(sector_avg$value)

# making melted data wide again
sector_avg <- dcast(sector_avg,Sector~variable,mean)
colnames(sector_avg)[2:6] <- c("SAvgPE","SAvgPEG","SAvgPS","SAvgPB","SAvgPrice")

# doing for industry now
industry_avg <- melt(finviz,id="Industry")
industry_avg <- subset(industry_avg,variable%in%c("Price","P.E","PEG","P.S","P.B"))
industry_avg <- (na.omit(industry_avg))
industry_avg$value <- as.numeric(industry_avg$value)
industry_avg <- dcast(industry_avg,Industry~variable,mean)
colnames(industry_avg)[2:6] <- c("IAvgPE","IAvgPEG","IAvgPS","IAvgPB","IAvgPrice")

# adding sector and industry avg to original data
finviz <- merge(finviz,sector_avg,by.x="Sector",by.y="Sector")
finviz <- merge(finviz,industry_avg,by.x="Industry",by.y="Industry")

# adding new fields to track value of each stock
finviz$SPEUnder <- 0
finviz$SPEGUnder <- 0
finviz$SPSUnder <- 0
finviz$SPBUnder <- 0
finviz$SPriceUnder <-0
finviz$IPEUnder <- 0
finviz$IPEGUnder <- 0
finviz$IPSUnder <- 0
finviz$IPBUnder <- 0
finviz$IPriceUnder <-0

# replacing 0's with 1 whereever stock is undervalued
finviz$SPEUnder[finviz$P.E < finviz$SAvgPE] <- 1
finviz$SPEGUnder[finviz$PEG < finviz$SAvgPEG] <- 1
finviz$SPSUnder[finviz$PS < finviz$SAvgPS] <- 1
finviz$SPBUnder[finviz$PB < finviz$SAvgPB] <- 1
finviz$SPriceUnder[finviz$Price < finviz$SAvgPrice] <-1
finviz$IPEUnder[finviz$P.E < finviz$IAvgPE] <- 1
finviz$IPEGUnder[finviz$PEG < finviz$IAvgPEG] <- 1
finviz$IPSUnder[finviz$PS < finviz$IAvgPS] <- 1
finviz$IPBUnder[finviz$PB < finviz$IAvgPB] <- 1
finviz$IPriceUnder[finviz$Price < finviz$IPrice] <-1

# now we will sum the 10 columns added to get a gist of overall effect on stock
finviz$RelValIndex <- apply(finviz[79:88],1,sum)

# greater realvalindex means that the stock is pretty devalued and has the capacity of 
# investment if the company is finacially strong
potentially_undervalued <- subset(finviz,RelValIndex>=5)

# there could be other ways also to determine whether stock is undervalued, the method may differ but structure and steps will be same

## Screening stocks and analyzing stock historical data
# selecting stocks which could be good for future investments
# the criteria differ from person to person, for instance 
# 1.choose only US companies
# 2.price per share $20 to $100
# 3.positive earnings/share currently and for future
# 4.volume greater than 10000
# 5.total debt/quity ratio less than 1
# 6.beta less than 1
# 7.instituional ownership less than 30
# 8.relative valuation index value greater than 8

target_stocks <- subset(finviz,Price>20 & Price < 100 & Country == "USA" & EPS>0 & EPS.next.Y>0 & EPS.next.5Y>0 & Debt.Eq <1 & Beta < 1.5 & Inst.Own < 30 & RelValIndex > 8)

# analyzing overall prices of target stocks

counter <- 0 
for (symbol in target_stocks$Ticker){
  url <- paste0("http://ichart.fincance.yahoo.com/table.csv?s=",symbol,"&a=08&b=7&c=198&d=01&e=23&f=2014&g=d&ignore=.csv")
  stock <- read.csv(url)
  stock <- na.omit(stock)
  colnames(stock)[7] <- "AdjClose"
  stock[,1] <- as.Date(stock[,1])
  stock <- cbind(Symbol = symbol,stock)
  maxrow <- nrow(stock)-49
  ma50 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,50,allign="right"))
  maxrow <- nrow(stock)-199
  ma200 <- cbind(stock[1:maxrow,1:2],rollmean(stock$AdjClose,200,allign="right"))
  stock <- merge(stock,ma50,by.x=c("Symbol","Date"),by.y=c("Symbol","Date"),all.x=TRUE)
  colnames(stock)[9] <- "MovAvg50"
  stock <- merge(stock,ma200,by.x=c("Symbol","Date"),by.y=c("Symbol","Date"),all.x=TRUE)
  colnames(stock)[10] <- "MovAvg200"
  price_chart <- melt(stock[,c(1,2,8,9,10)],id=c("Symbol","Date"))
  qplot(Date,value,data=price_chart,geom="line",color=variable,main=paste(symbol,"Daily Sr=tock Prices"),ylab="Price")
  ggsave(filename=paste0("stock_price_",counter,".png"))
  price_summary <- ddply(stock,"Symbol",summarise,open=Open[nrow(stock)],high=max(High),low=min(Low),close=AdjClose[1])
  if(counter==0){
    stocks <- rbind(stock)
    price_summaries <-rbind(price_summary)
  }
  else{
    stocks <- rbind(stocks,stock)
    price_summaries <- rbind(price_summaries,price_summary)
  }
  counter <- counter +1
}
qplot(Date,AdjClose,data=stocks,geom ="line",color=Symbol,main="Daily Stock Prices")
ggsave(filename="stock_price_combined.png")
summary <- melt(price_summaries,id="Symbol")
ggplot(summary,aes(x=variable,y=value,fill=Symbol)) + geom_bar(stat="identity") + facet_wrap(~Symbol)
ggsave(filename=("stock_price_summaries.png"))
}  



url <- "https://finviz.com/quote.ashx?t=ABAC&ty=c&ta=0&p=d&b=1"
stock <- read.csv(URL