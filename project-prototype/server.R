
library(shiny)
library(ggplot2)
library(reshape)
library(plyr)


library(quantmod)
library(zoo)
options("getSymbols.warning4.0"=FALSE)
options(warn=-1)

#Read in all the tickers and their meta-data for stock and currency.
#In the future, I can add the etf, future, index, and mutualfund.
#However, at this point, quantmod doesn't have the built-in function to retrieve
#this information from yahoo finance.
#etf <- read.csv('etf.csv')
#future <- read.csv('future.csv')
#index <- read.csv('index.csv')
#mutualfund <- read.csv('mutualfund.csv')
stock <- read.csv('stock.csv')
currency <- read.csv('currency.csv')
currencyName <- read.csv('currencyname.csv')

#process the currency data to provide a vector (database) for unique currency
getAllCurrencyTicker <- function(){
  currencyNames <- as.character(currency$Name[grep("/",currency$Name)])
  currencyNames <- currencyNames[nchar(currencyNames)==7]
  currencyname1 <- substr(currencyNames,1,3)
  currencyname2 <- substr(currencyNames,5,7)
  uniq_currencynames <- sort(unique(c(currencyname1,currencyname2)))
  return(uniq_currencynames)
  #print(data.frame(matrix(uniq_currencynames,nrow=21,byrow=T)),row.names=F)
}

#function for search currency ticker
getTickerCurrency <- function(name,topNum){
  index <- order(adist(name,currencyName$Name,partial=TRUE,ignore.case=TRUE))[1:topNum]
  searchresult <- currencyName[index,c('Name','Ticker')]
  print(searchresult,row.names=F)
}


#function for search stock ticker
getTickerStock <- function(name,topNum){
  index <- order(adist(name,stock$Name,partial=TRUE,ignore.case=TRUE))[1:topNum]
  searchresult <- stock[index,c('Name','Ticker','Country','Exchange','Category.Name')]
  print(searchresult,row.names=F)
}

#function for creating numeric date
getNumericDate <- function(ts){
  posixtime <- as.POSIXlt(time(ts))
  numtime <- 1900 + posixtime$year + posixtime$yday/365
  return(numtime)
}

#function for loading stock data
getStock <- function(symbols,start,end,granularity='days'){
  getSymbols(symbols,from=start,to=end)
  df <- data.frame()
  mapgran <- data.frame(gran=c("days", "weeks", "months", "quarters", "years"),
                        inter=c('daily','weekly', 'monthly', 'quarterly', 'yearly'),
                        stringsAsFactors=F)
  interval <- mapgran[mapgran$gran==granularity,2]
  for(sym in symbols){
    newsym <- paste(sym,granularity,sep='')
    assign(newsym,to.period(get(sym),period=granularity))
    Return <- as.numeric(periodReturn(get(sym),period=interval))
    tempdf <- data.frame(date=index(get(newsym)),
                         time=getNumericDate(get(newsym)),
                         get(newsym),row.names=NULL)
    year <- format.Date(tempdf[,1],"%Y")
    month <- format.Date(tempdf[,1],"%b")
    week <- format.Date(tempdf[,1],"%W")
    name <- sym
    tempdf <- cbind(name,tempdf,year,month,week,Return)
    df <- rbind(df,tempdf)
  }
  colnames(df) <- c('name','date','time','open',
                    'high','low','close','volume',
                    'adjusted','year','month','week','Return')
  return(df)
}

symbols <- c('MSFT','TIF','MAR','BAC')
startdate <- '2000-01-01'
enddate <- '2014-04-28'
df <- getStock(symbols,startdate,enddate,granularity='months')

#function for loading dividend data
getDividend <- function(symbols,start,end){
  df <- data.frame()
  for(sym in symbols){
    newsym <- paste(sym,'.Div',sep='')
    assign(newsym,getDividends(sym,from=start,to=end))
    tempdf <- data.frame(date=index(get(newsym)),
                         time=getNumericDate(get(newsym)),
                         get(newsym),row.names=NULL)
    year <- format.Date(tempdf[,1],"%Y")
    month <- format.Date(tempdf[,1],"%b")
    week <- format.Date(tempdf[,1],"%W")
    name <- sym
    tempdf <- cbind(name,tempdf,year,month,week)
    df <- rbind(df,tempdf)
  }
  colnames(df) <- c('name','date','time','dividend','year','month','week')
  return(df)
}

df.div <- getDividend(symbols,startdate,enddate)

#function for getting financial report data
getFinancialReport <- function(symbol){
  getFinancials(symbol)
  return(get(paste(symbol,'.f',sep='')))
}
financialReport <- getFinancialReport('MSFT')
#viewFinancials(financialReport)

#get any item on the income statement
ISitemList <- rownames(financialReport$IS$A)
getReportItemIS <- function(report,interval,item){
  if(interval=='Quarter'){
    return(report$IS$Q[item,])
  }else{
    return(report$IS$A[item,])
  }
}
#getReportItemIS(financialReport,'Quarter',"Income Before Tax")
#get any item on the balance sheet
BSitemList <- rownames(financialReport$BS$A)
getReportItemBS <- function(report,interval,item){
  if(interval=='Quarter'){
    return(report$BS$Q[item,])
  }else{
    return(report$BS$A[item,])
  }
}
#get any item on the cash flow statement
CFitemList <- rownames(financialReport$CF$A)
getReportItemCF <- function(report,interval,item){
  if(interval=='Quarter'){
    return(report$CF$Q[item,])
  }else{
    return(report$CF$A[item,])
  }
}

#get currency exchange rate
getExchangeRate <- function(ticker1,ticker2,granularity='days',start='2000-01-01',end='2014-04-28'){
  symbol <- paste(ticker1,'/',ticker2,sep='')
  #####
  #The reason I have to break down the calls for 400 days 
  #at a time is because oanda only allows 500 days at maximum
  #for each call
  startdate <- as.Date(start)
  enddate <- as.Date(end)
  len <- as.numeric(enddate-startdate)/400
  datelist <- seq(startdate,enddate,length.out=ceiling(len)+1)
  getFX(symbol,from=as.character(datelist[1]),
        to=as.character(datelist[2]-1))
  ts <- get(paste(ticker1,ticker2,sep=''))
  if(len>1){
    for(i in 2:(length(datelist)-1)){
      getFX(symbol,from=as.character(datelist[i]),
            to=as.character(datelist[i+1]-1))
      ts <- rbind(ts,get(paste(ticker1,ticker2,sep='')))
    }
  }
  #####
  assign('ts_new',to.period(ts,period=granularity))
  tempdf <- data.frame(date=index(ts_new),
                       time=getNumericDate(ts_new),
                       ts_new,row.names=NULL)
  year <- format.Date(tempdf[,1],"%Y")
  month <- format.Date(tempdf[,1],"%b")
  week <- format.Date(tempdf[,1],"%W")
  tempdf <- cbind(tempdf,year,month,week)
  colnames(tempdf) <- c('date','time','open','high','low','close','year','month','week')
  return(tempdf)
}
er <- getExchangeRate("USD","JPY",granularity='months',
                      start='2013-06-01',end='2014-04-28')
  

#linear filtering for time series
#decomposition of time series
dfmsft <- df[df$name=='MSFT',]
msft <- ts(dfmsft$close,start=2000,freq=12)
plot(stl(msft,s.window="periodic"),main='Decomposition of MSFT Stock Price')
#regression analysis
start <- min(dfmsft$time)
end <- max(dfmsft$time)
len <- length(dfmsft$time)
t <- seq(start,end,length=len)
t2 <- t^2
sin.t <- sin(2*pi*t)
cos.t <- cos(2*pi*t)
plot(msft)
lmmsft<-lm(msft~t+t2+sin.t+cos.t)
summary(lmmsft)
lines(x=t,y=lmmsft$fit,col=2,lwd=2)
#exponential smoothing
expfit <- HoltWinters(msft)
summary(expfit)
#plot(msft)
#lines(expfit$fitted,col='red')



#source('global.r')

head(dfmsft)
melt.dfmsft <- melt(dfmsft,id.vars=c('name','date','time','year',
                                     'month','week'),
                    measure.vars=c('open','low','high','close'))
plot1 <- ggplot(melt.dfmsft,aes(x=time,y=value,
                       group=factor(variable),color=factor(variable))) +
  geom_line()

melt.df <- melt(df,id.vars=c('name','date','time','year',
                                 'month','week'),
                measure.vars=c('open','low','high','close'))
plot2 <- ggplot(melt.df,aes(x=time,y=value,
                group=factor(variable),color=factor(variable))) +
  geom_line() +
  facet_wrap(~name,nrow=2)

shinyServer(function(input, output) {
  
  cat("Press \"ESC\" to exit...\n")
  
  # Copy the data frame (don't want to change the data
  # frame for other viewers)
  
  output$report_text <- renderPrint(
    return(viewFinancials(financialReport))
  )
  
  output$overview <-  renderPlot({
    print(plot1)
  })
  
  output$multiple_stock <- renderPlot({
    print(plot2)
  })
  
  output$timeseriesanalysis <- renderPlot({
    print(plot(stl(msft,s.window="periodic"),main='Decomposition of MSFT Stock Price')
)
  })
  
  
})