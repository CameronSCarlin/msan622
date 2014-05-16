library(reshape)
library(plyr)
library(shiny)
library(ggplot2)
library(scales)
library(tseries)
library(forecast) #get the fitted values for the arima model


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
quantmod_indicators <- read.csv('quantmod_indicators.csv',stringsAsFactors = F)
quantmod_indicators$quantmodName_new <- paste(quantmod_indicators[,3],'()',sep="")

#process the currency data to provide a vector (database) for unique currency
getAllCurrencyTicker <- function(){
  currencyNames <- as.character(currency$Name[grep("/",currency$Name)])
  currencyNames <- currencyNames[nchar(currencyNames)==7]
  currencyname1 <- substr(currencyNames,1,3)
  currencyname2 <- substr(currencyNames,5,7)
  uniq_currencynames <- sort(unique(c(currencyname1,currencyname2)))
  return(uniq_currencynames)
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


#function for getting financial report data
getFinancialReport <- function(symbol){
  getFinancials(symbol)
  return(get(paste(symbol,'.f',sep='')))
}


#get any item on the income statement
getReportItemIS <- function(report,interval,item){
  if(interval=='Quarter'){
    d <- data.frame(report$IS$Q[item,])
  }else{
    d <- data.frame(report$IS$A[item,])
  }
  colnames(d) <- item
  return(d)
}

#get any item on the balance sheet
getReportItemBS <- function(report,interval,item){
  if(interval=='Quarter'){
    d <- data.frame(report$BS$Q[item,])
  }else{
    d <- data.frame(report$BS$A[item,])
  }
  colnames(d) <- item
  return(d)
}

#get any item on the cash flow statement
getReportItemCF <- function(report,interval,item){
  if(interval=='Quarter'){
    d <- data.frame(report$CF$Q[item,])
  }else{
    d <- data.frame(report$CF$A[item,])
  }
  colnames(d) <- item
  return(d)
}

#wrapper for the three get report item functions
getReportItem <- function(report,interval,item,type){
  if(type=='Income Statement'){
    return(getReportItemIS(report,interval,item))
  }else
    if(type=='Balance Sheet'){
    return(getReportItemBS(report,interval,item))
  }else{
    return(getReportItemCF(report,interval,item))
  }
}

#function for choosing report items in the shiny server
getReport <- function(report,type,interval){
  if(type=='Balance Sheet') {
    reports1 <- report$BS
  }else
    if(type=='Cash Flow Statement'){
      reports1 <- report$CF
    }else{
      reports1 <- report$IS
    }
  #'Quarter','Annual'
  if(interval=='Quarter'){
    reports2 <- reports1$Q
  }else{
    reports2 <- reports1$A
  }
  return(reports2)
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



iskeyitemtable <- data.frame(Multi_Step_Format=c('Net Sales','Cost of Sales','Gross Income','Selling, General and Administrative Expenses (SG&A)',
                                                 'Operating Income','Other Income & Expenses','Pretax Income*  Taxes','Taxes', 'Net Income (after tax)'),
                             Single_Step_Format=c('Net Sales', 'Materials and Production','Marketing and Administrative','Research and Development Expenses (R&D)',
                                                  'Other Income & Expenses','Pretax Income','Taxes','Net Income','--'))



financialReportmsft <- getFinancialReport('MSFT')
ISitemList <- rownames(financialReportmsft$IS$A)
BSitemList <- rownames(financialReportmsft$BS$A)
CFitemList <- rownames(financialReportmsft$CF$A)


#summarize the stock and currencyName data
getOverviewFRData <- function(var){
  varCount <- sort(table(stock[,var]),decreasing=T)
  vardata <- data.frame(var=names(varCount),
                        Counts=varCount,
                        rank=1:length(varCount),
                        row.names=NULL)
  vardata$var <- factor(vardata$var,
                        levels=vardata$var[order(vardata$Counts,decreasing=T)])
  return(vardata)
}


getOverviewBarplotFR4 <- function(vardata,brushrange,variable){
  barplot <- ggplot(subset(vardata,rank>=brushrange[1] & rank <= brushrange[2]),
                    aes(x=var,y=Counts)) +
    geom_bar(stat='identity',fill='#756bb1') +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    xlab(variable) + ylab('Stocks') +
    theme(panel.grid=element_blank(),axis.ticks=element_blank(),
          panel.border=element_blank())
  return(barplot)
}

#getOverviewBarplotFR4(brushrange,variable)



getSingleStockOverview <- function(df,series,start,window){
  xmin <- start
  xmax <- start + window/12
  ymin <- min(df[df$time>=xmin & df$time <= xmax & df$variable %in% series,'value']) * 0.9
  ymax <- max(df[df$time>=xmin & df$time <= xmax & df$variable %in% series, 'value']) * 1.1
  datarect <- data.frame(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax)
  overview <- ggplot(df[df$variable %in% series,],aes(x=time,y=value,group=variable,color=variable)) +
    geom_rect(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax,fill = 'grey',color='grey',alpha=0.5) +
    geom_line(aes(linetype=variable)) +
    scale_y_continuous(label=dollar) +
    scale_color_brewer(palette='Dark2',name='Price',guide='none') +
    scale_linetype(name='Price',guide='none') +
    theme_bw() +
    xlab('Time') + ylab('Price') +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          axis.title.x=element_blank())
  return(overview)
}
#getSingleStockOverview(df,series,start,window)

getSingleStockDetailview <- function(df,series,start,window){
  xmin <- start
  xmax <- start + window/12
  ymin <- min(df[df$time>=xmin & df$time <= xmax & df$variable %in% series,'value']) * 0.9
  ymax <- max(df[df$time>=xmin & df$time <= xmax & df$variable %in% series, 'value']) * 1.1
  detailview <- ggplot(df[df$variable %in% series,],aes(x=time,y=value,group=variable,color=variable)) +
    geom_line(aes(linetype=variable)) +
    scale_color_brewer(palette='Dark2',name='Price') +
    scale_linetype(name='Price') +
    theme_bw() +
    scale_x_continuous(limits = c(xmin, xmax),expand = c(0, 0),
                       breaks = seq(floor(xmin), ceiling(xmax), by = 1)) +
    scale_y_continuous(limits = c(ymin, ymax),expand = c(0, 0),
                       breaks = seq(ymin, ymax, length.out = 5),
                       label=dollar) +
    theme(legend.text = element_text(face = "bold"),
          legend.position=c(0,0),legend.justification=c(0,0),
          legend.title = element_blank(),legend.background = element_blank(),
          legend.direction='horizontal',
          legend.key = element_rect(fill = NA,colour = "white",size = 1)) +
    theme(axis.ticks=element_blank(),axis.title.x=element_blank(),
          panel.border=element_blank())
  return(detailview)
}
#getSingleStockDetailview(df,series,start,window) 
  

#quantmod charts
getXTSforQuantmodCharts <- function(symbol,start,end,granularity){
  getSymbols(symbol,from=start,to=end)
  assign('xts',to.period(get(symbol),period=granularity))
  return(xts)
}


getQuantCharts <- function(xts_obj,charttype,symbol,daterange,TANames){
  TAS <- paste(quantmod_indicators[which(quantmod_indicators$Indicator %in% TANames),4],collapse=';')
  chartSeries(xts_obj,name=symbol,
              type=charttype,
              subset=paste(daterange,collapse='::'),
              TA=TAS,
              theme='white',
              plot=T)
}




getMultistockSmallMultiple <- function(df,series,start){
  xmin <- start
  xmax <- start + 1
  ymin <- min(df$value[df$variable %in% series])
  ymax <- max(df$value[df$variable %in% series])
  datarect <- data.frame(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax)
  smallmultiple <- ggplot(df[df$variable %in% series,],aes(x=time,y=value,group=variable,color=variable)) +
    geom_rect(xmin = xmin, xmax = xmax,ymin = ymin, ymax = ymax,fill = 'grey',color='grey',alpha=0.2) +
    geom_line(aes(linetype=variable)) +
    facet_wrap(~name) +
    scale_y_continuous(label=dollar) +
    scale_color_brewer(palette='Dark2',name='Price') +
    scale_linetype(name='Price') +
    theme_bw() +
    xlab('Time') + ylab('Price') +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.text.x=element_text(angle=45))
  return(smallmultiple)
}
#getMultistockSmallMultiple(multistocks_melt,series,2005)



getBubblePlot <- function(df,xvar,yvar,svar,start){
  dfnew <- df[,c('name','date',xvar,yvar,svar)]
  colnames(dfnew) <- c('name','date','xvar','yvar','svar')
  xmin <- min(dfnew$xvar)
  xmax <- max(dfnew$xvar)
  ymin <- min(dfnew$yvar)
  ymax <- max(dfnew$yvar)
  moment <- sort(unique(dfnew$date))[start]
  bubbleplot <- ggplot(subset(dfnew,date==moment),aes(x=xvar,y=yvar,color=factor(name))) +
    geom_point(aes(size=svar)) +
    geom_text(aes(label=name),color='black',vjust=2,size=4) +
    scale_size_area(max_size = 20, guide='none') +
    scale_color_brewer(palette='Dark2',guide='none') +
    scale_y_continuous(limits=c(ymin-5,ymax+5)) +
    scale_x_continuous(limits=c(xmin-5,xmax+5)) +
    theme(axis.ticks=element_blank(),
          panel.grid.minor=element_blank()) +
    xlab(paste(xvar,'price')) + ylab(paste(yvar,'price')) +
    geom_abline(intercept=0,slope=1,color='blue',linetype=3) +
    annotate('text',x=xmin,y=ymax,label=as.character(moment),size = 4)
  return(bubbleplot)
}

#getBubblePlot(multistocks,xvar,yvar,svar,start=1)

getHeatMap <- function(df,fillvar,brushrange,midrange){
  dflocal <- subset(df,time>=brushrange[1] & time <= brushrange[2],select=c('name','date',fillvar))
  colnames(dflocal) <- c('name','date','fillvar')
  palette <- c("#008837", "#f7f7f7", "#f7f7f7", "#7b3294")
  heatmap <- ggplot(dflocal,aes(x=as.character(date),y=name)) +
    geom_tile(aes(fill = fillvar), colour = "white") +
    theme_minimal() +
    theme(axis.text = element_text(angle = 90, hjust = 0.5)) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(panel.grid = element_blank()) +
    labs(fill=fillvar) +
    #theme(legend.position = "none") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))
  if(midrange[1] == midrange[2]) {
    heatmap <- heatmap + scale_fill_gradient2(low = palette[1], mid = palette[2], high = palette[4], midpoint = midrange[1])
  }else {
    heatmap <- heatmap + scale_fill_gradientn(colours = palette, values = c(0, midrange[1], midrange[2], 1))
  }
  return(heatmap)
}

#getHeatMap(df,fillvar,brushrange,midrange)


getParCoord <- function(df,cols,grpcol,brushvar,range,moment){
  moment_date <- sort(unique(df$date))[moment]
  localframe <- subset(df, date==moment_date)
  localframe$brushvar <- localframe[,brushvar]
  localframe$hide_ind <- ifelse(localframe$brushvar>=range[1] & localframe$brushvar<=range[2],1,0)
  localframe$grpcol <- localframe[,grpcol]
  meltdt <- melt(localframe,id.vars=c('name','hide_ind','grpcol','time','date'),measure.vars=cols) 
  meltdt2 <- ddply(meltdt,.(variable),transform,value2=(value-min(value))/(max(value)-min(value)))
  localframe1 <- subset(meltdt2,hide_ind==1 )
  localframe2 <- subset(meltdt2,hide_ind==0)
  if(nrow(localframe1)!=0){
    pc <- ggplot(localframe1,aes(x=variable,y=value2,color=grpcol,group=name))
    if(nrow(localframe2)!=0){
      pc <- pc + geom_line(data=localframe2,aes(x=variable,y=value2,group=name),color='grey',alpha=0.2)
    }
    pc <- pc + geom_line() + scale_color_brewer(palette='Dark2') +
      annotate('text',x=localframe1$variable[localframe1$variable==cols[1]],
               y=localframe1$value2[localframe1$variable==cols[1]],
               label=localframe1$name[localframe1$variable==cols[1]],size=3)
  }else{
    pc <- ggplot(localframe2,aes(x=variable,y=value2,group=name)) + geom_line(colour='grey',apha=0.3)
  }
  pc <- pc + theme_minimal() +
    scale_y_continuous(expand = c(0.02, 0.02)) +
    scale_x_discrete(expand = c(0.02, 0.02)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.major.x = element_line(color = "#bbbbbb")) +
    theme(legend.position = "bottom") +
    labs(color=grpcol) +
    ggtitle(moment_date)
  return(pc) 
}

#getParCoord(df,cols,grpcol,brushvar,range,moment)


getBarGraph <- function(divdf,brushrange){
  localframe <- subset(divdf,time>=brushrange[1] & time <= brushrange[2])
  bargraph <- ggplot(localframe,aes(x=as.factor(date),y=dividend,fill=name)) +
    geom_bar(stat='identity') +
    facet_wrap(~name,ncol=1,scale='free') +
    scale_fill_brewer(palette='Dark2',guide='none') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          axis.ticks=element_blank(),
          axis.title.x=element_blank()) +
    theme(panel.grid=element_blank(),
          panel.border=element_blank(),
          strip.background = element_rect(colour="white", fill="white")) +
    xlab('Date') + ylab('Dividend Per Share')
  return(bargraph)
}
#getBarGraph(divdf,brushrange)


getERTimeSeries <- function(erdf,series,brushrange){
  localframe <- subset(erdf,time>=brushrange[1] & time <= brushrange[2])
  localframe_melt <- melt(localframe,id=c('date','time','year','month','week'))
  melt <- subset(localframe_melt,variable %in% series)
  timeseries <- ggplot(melt,aes(x=time,y=value,group=variable,color=variable)) +
    geom_line(aes(linetype=variable)) +
    scale_color_brewer(palette='Dark2',name='Rate') +
    scale_linetype(name='Rate') +
    theme_bw() +
    xlab('Time') + ylab('Rate') +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1))
  return(timeseries)
}
#getERTimeSeries(erdf,series,brushrange)


getSTLDecompData <- function(df,col,freq){
  startindex <- floor(min(df$time))
  stlts <- ts(df[,col],start=startindex,frequency=freq)
  stldecomp <- stl(stlts,s.window="periodic")
  dfstldecomp <- cbind(df,data.frame(stldecomp$time.series))
  dfstldecomp <- subset(dfstldecomp,
                        select=c('name','date','time','year','month',
                                 'week',col,'seasonal','trend','remainder'))
  return(dfstldecomp)
}
#dfstldecomp<-getSTLDecompData(df,col,freq)


getSTLDecompositionPlot <- function(df,col,brushrange,scale_ind=F){
  df_new <- subset(df,time >= brushrange[1] & time <= brushrange[2])
  localframe <- melt(df_new,id=c('name','date','time','year','month','week'))
  dp <- ggplot(localframe,aes(x=time,y=value,group=variable,color=variable)) +
    geom_line()
  if(!scale_ind){
    dp <- dp + facet_wrap(~variable,ncol=1,scale='free_y')
  }else{
    dp <- dp + facet_wrap(~variable,ncol=1)
  }
  dp <- dp + scale_color_brewer(palette='Dark2',name='Components',guide='none') +
    scale_linetype(name='Price',guide='none') +
    scale_y_continuous(label=dollar) +
    theme_bw() +
    xlab('Time') + ylab('Price') +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.text.x=element_text(angle=45),
          axis.title.x=element_blank())
  return(dp)
}

getSTLSeasonalAdjustedPlot <- function(df,col,brushrange){
  df_new <- subset(df,time >= brushrange[1] & time <= brushrange[2])
  sap <- ggplot(df_new,aes(x=time,y=(trend+remainder),group=1,color=name)) +
    geom_line() +
    scale_y_continuous(label=dollar) +
    scale_color_brewer(palette='Dark2',guide='none') +
    theme_bw() +
    xlab('Time') + ylab(paste(toupper(col),'Price')) +
    ggtitle('Seasonal Adjusted Price (Price = Trend + Remainder)') +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.text.x=element_text(angle=45))
  return(sap)
}

#getSTLSeasonalAdjustedPlot(df,col,brushrange)


getExponentialSmoothingData <- function(df,col,freq,model,modeltype,CI,npred,brushrange,granularity){
  df_new <- subset(df,time >= brushrange[1] & time <= brushrange[2])
  startindex <- 1#floor(min(df_new$time))
  ests <- ts(df_new[,col],start=startindex,frequency=freq)
  if(model=='Simple Exponential'){
    hw <- HoltWinters(ests,beta=FALSE,gamma=FALSE)
    hwdf <- data.frame(hw$fitted,time=df_new$time[-1])
    hwdf$residuals <- df_new[-1,col] - hwdf$xhat
    alpha <- paste('alpha',round(hw$alpha,5),sep=': ')
    SSE <- paste('SSE',round(hw$SSE,2),sep=': ')
    parameters <- paste(alpha,SSE,sep='; ')
  }else
  if(model=='Double Exponential'){
    hw <- HoltWinters(ests,gamma=FALSE)
    hwdf <- data.frame(hw$fitted,time=df_new$time[-c(1:2)])
    hwdf$residuals <- df_new[-c(1:2),col] - hwdf$xhat
    alpha <- paste('alpha',round(hw$alpha,5),sep=': ')
    beta <- paste('beta',round(hw$beta,5),sep=': ')
    SSE <- paste('SSE',round(hw$SSE,2),sep=': ')
    parameters <- paste(alpha,beta,SSE,sep='; ')
  }else
  if(model=='Seasonal Exponential'){
      hw <- HoltWinters(ests,beta=FALSE,seasonal=modeltype)
      hwdf <- data.frame(hw$fitted,time=df_new$time[-c(1:freq)])
      hwdf$residuals <- df_new[-c(1:freq),col] - hwdf$xhat
      alpha <- paste('alpha',round(hw$alpha,5),sep=': ')
      gamma <- paste('gamma',round(hw$gamma,5),sep=': ')
      SSE <- paste('SSE',round(hw$SSE,2),sep=': ')
      parameters <- paste(alpha,gamma,SSE,sep='; ')
  }else{
    hw <- HoltWinters(ests,seasonal=modeltype)
    hwdf <- data.frame(hw$fitted,time=df_new$time[-c(1:freq)])
    hwdf$residuals <- df_new[-c(1:freq),col] - hwdf$xhat
    alpha <- paste('alpha',round(hw$alpha,5),sep=': ')
    beta <- paste('beta',round(hw$beta,5),sep=': ')
    gamma <- paste('gamma',round(hw$gamma,5),sep=': ')
    SSE <- paste('SSE',round(hw$SSE,2),sep=': ')
    parameters <- paste(alpha,beta,gamma,SSE,sep='; ')
  }
  forecast <- predict(hw,n.ahead=npred,prediction.interval=T,level=CI)
  if(granularity=='days') {   
    forecasttime <- max(df_new$time) + 1/365 * 1:npred
  }else
    if(granularity=='weeks'){
     forecasttime <- max(df_new$time) + 1/52 * 1:npred
  }else
    if(granularity=='months'){
    forecasttime <- max(df_new$time) + 1/12 * 1:npred
  }else
    if(granularity=='quarters'){
    forecasttime <- max(df_new$time) + 1/4 * 1:npred  
  }else{
    forecasttime <- max(df_new$time) + 1:npred
  }
  forecastdf <- data.frame(forecast,time=forecasttime)
  return(list(hwdf,forecastdf,hw,parameters))
}

#esdata <- getExponentialSmoothingData(df,col,freq,model,modeltype,CI,npred,brushrange,granularity)

getESTimeSeriesPlot <- function(df,esdata,col,brushrange){
  df_new <- subset(df,time >= brushrange[1] & time <= brushrange[2])
  df_new$col <- df_new[,col]
  forecast <- melt(esdata[[2]],id='time')
  ests <- ggplot(esdata[[2]]) + geom_ribbon(aes(x=time,ymin=lwr,ymax=upr),fill='yellow',alpha=0.3) +
    geom_line(data=df_new,aes(x=time,y=col,group=1),alpha=0.2) +
    geom_line(data=esdata[[1]],aes(x=time,y=xhat,group=1),color='red') +
    geom_line(data=forecast,aes(x=time,y=value,group=variable,color=variable)) +
    scale_color_brewer(palette='Dark2',name='Components') +
    scale_y_continuous(label=dollar) +
    theme_bw() +
    xlab('Time') + ylab(paste(toupper(col),'Price')) +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.text.x=element_text(angle=45),
          axis.title.x=element_blank()) +
    annotate('text',x=min(df_new$time)+0.5,y=max(df_new$col)*1.1,color='dark green',size=4,label='grey = actual, red = fitted')
  return(ests) 
}

#getESTimeSeriesPlot(df,esdata,col,brushrange)

#get ES residual plot
getESResidualPlot <- function(esdata){
  localframe <- esdata[[1]]
  residualsPlot <- ggplot(localframe,aes(x=time,y=residuals)) +
    geom_bar(stat='identity',fill='dark green') +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    ylab('Model Residuals') + xlab('Time') +
    theme(panel.grid=element_blank(),axis.ticks=element_blank(),
          panel.border=element_blank(),
          axis.text.x=element_blank()) +
    ggtitle(esdata[[4]])
  return(residualsPlot)
}

#getESResidualPlot(esdata)

#create plots for acf and pacf
getACFPlot <- function(df,col,lags) {
  acfdata <- acf(df[,col],lag=lags,plot=F)
  pacfdata <- pacf(df[,col],lag=lags,plot=F)
  acfdf <- data.frame(lag=acfdata$lag[-1],acf=acfdata$acf[-1],pacf=pacfdata$acf)
  acfdf_melt <- melt(acfdf,id='lag')
  acfPlot <- ggplot(acfdf_melt,aes(x=factor(lag),y=value,group=variable,fill=factor(variable))) +
    geom_bar(stat='identity') +
    facet_wrap(~variable,ncol=2) +
    scale_fill_brewer(palette='Dark2',guide='none') +
    #scale_y_continuous(limits=c(-1,1)) +
    theme_bw() +
    xlab('Lag') + 
    theme(axis.ticks=element_blank(),panel.grid=element_blank(),
          panel.border=element_blank(),
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.title.y=element_blank())
  return(acfPlot)
}
#getACFPlot(df,col,lags)



#Box-Ljung Test for white noise
getBJTest <- function(df,col,bjtest_lag){
  bjtest_lag <- as.numeric(bjtest_lag)
  bjtest<-Box.test(df[,col],lag=bjtest_lag,type='Ljung-Box')
  return(bjtest)
}

#unit root test for stationarity
getUnitRootTest <- function(df,col){
  unitroottest <- adf.test(df[,col])
  return(unitroottest)
}

box.cox <- function(ts,lambda){
  if(lambda==0){
    tstransform <- log(ts)
  }else{
    tstransform <- (ts**lambda - 1)/lambda
  }
  return(tstransform)
}

box.cox.inv <- function(tstrans,lambda){
  if(lambda==0){
    ts <- exp(tstrans)
  }else{
    ts <- (lambda*tstrans+1)**(1/lambda)
  }
  return(ts)
}


olsdetrend <- function(df,col){
  t <- 1:length(df$time)
  t2 <- t^2
  sin.t <- sin(2*pi*t)
  cos.t <- cos(2*pi*t)
  lmfit<-lm(df[,col]~t+t2+sin.t+cos.t)
  return(lmfit)
}
#fit <- olsdetrend(df,col)

getolsdetrendPrediction <- function(olsfit,df,npred){
  t <- length(df$time) + 1:npred
  t2 <- t^2
  sin.t <- sin(2*pi*t)
  cos.t <- cos(2*pi*t)
  predictions <- predict(olsfit,data.frame(cbind(t,t2,sin.t,cos.t)))
  return(predictions)
}


getARIMAmodelDataframe <- function(df,col,p,d,q,seasonal,P,D,Q,period,
                                   detrend,boxcox,lambda,npred,granularity,CI,brushrange){
  localframe <- subset(df, time>=brushrange[1] & time <= brushrange[2])
  alpha <- 1-CI
  if(detrend==T & boxcox==F){
    olsfit <- olsdetrend(localframe,col)
    localframe$col <- olsfit$residuals
    localframe$fit <- olsfit$fitted
    olspredictions <- getolsdetrendPrediction(olsfit,localframe,npred)
  }else if(detrend==T & boxcox==T){
    olsfit <- olsdetrend(localframe,col)
    residuals <- olsfit$residuals
    bcresiduals <- box.cox(residuals,lambda)
    localframe$col <- bcresiduals
    localframe$fit <- olsfit$fitted
    localframe$residuals <- residuals
    olspredictions <- getolsdetrendPrediction(olsfit,localframe,npred)
  }else if(detrend==F & boxcox==T){
    localframe$col <- box.cox(localframe[,col],lambda)
  }else{
    localframe$col <- localframe[,col]
  }
  if(seasonal==F){
    arima.model <- arima(localframe$col,order=c(p,d,q))
  }else{
    arima.model <- arima(localframe$col,order=c(p,d,q),
                         seasonal=list(order=c(P,D,Q),period=period))
  }
  predictions <- predict(arima.model,n.ahead=npred)
  localframe$modelresiduals <- arima.model$residuals
  AICs <- paste('AIC',round(arima.model$aic,1),sep=': ')
  BICs <- paste('BIC',round(AIC(arima.model,k=log(length(localframe$col))),1),sep=': ')
  loglikelihood <- paste('LogLikelihood',round(arima.model$loglik,2),sep=': ')
  sigma <- paste('Sigma^2',round(arima.model$sigma2,3),sep=': ')
  coefs <- paste(names(arima.model$coef),round(arima.model$coef,3),sep=': ',collapse='; ')
  if(detrend==T & boxcox==F){
    localframe$modelfit <- localframe$fit + fitted(arima.model)
    modelpredictions <- olspredictions + predictions$pred
    modelpredictions_upper <- modelpredictions + qnorm(1-alpha/2)*predictions$se
    modelpredictions_lower <- modelpredictions - qnorm(1-alpha/2)*predictions$se
  }else if(detrend==T & boxcox==T){
    localframe$modelfit <- localframe$fit + box.cox.inv(fitted(arima.model),lambda)
    modelpredictions <- olspredictions + box.cox.inv(predictions$pred,lambda)
    modelpredictions_upper <- olspredictions + box.cox.inv(predictions$pred+qnorm(1-alpha/2)*predictions$se,lambda)
    modelpredictions_lower <- olspredictions + box.cox.inv(predictions$pred-qnorm(1-alpha/2)*predictions$se,lambda)
  }else if(detrend==F & boxcox==T){
    localframe$modelfit <- box.cox.inv(fitted(arima.model),lambda)
    modelpredictions <- box.cox.inv(predictions$pred,lambda)
    modelpredictions_upper <- box.cox.inv(predictions$pred+qnorm(1-alpha/2)*predictions$se,lambda)
    modelpredictions_lower <- box.cox.inv(predictions$pred-qnorm(1-alpha/2)*predictions$se,lambda)
  }else{
    localframe$modelfit <- fitted(arima.model)
    modelpredictions <- predictions$pred
    modelpredictions_upper <- predictions$pred+qnorm(1-alpha/2)*predictions$se
    modelpredictions_lower <- predictions$pred-qnorm(1-alpha/2)*predictions$se
  }
  modelpredictions_df <- data.frame(pred=modelpredictions,upper=modelpredictions_upper,
                                    lower=modelpredictions_lower)
  if(granularity=='days') {   
    modelpredictions_df$time <- max(localframe$time) + 1/365 * 1:npred
  }else if(granularity=='weeks'){
    modelpredictions_df$time <- max(localframe$time) + 1/52 * 1:npred
  }else if(granularity=='months'){
    modelpredictions_df$time <- max(localframe$time) + 1/12 * 1:npred
  }else if(granularity=='quarters'){
    modelpredictions_df$time <- max(localframe$time) + 1/4 * 1:npred  
  }else{
    modelpredictions_df$time <- max(localframe$time) + 1:npred
  }
  return(list(localframe,modelpredictions_df,c(AICs,BICs,loglikelihood,sigma,coefs),arima.model)) 
}

#arimamodeldata <- getARIMAmodelDataframe(df,col,p,d,q,seasonal,P,D,Q,period,
#                                   detrend,boxcox,lambda,npred,granularity,CI,brushrange)

#create time series plot for arima model
getTimeSeriesPlotARIMA <- function(dt,col){
  localframe <- dt[[1]]
  localframe$var <- localframe[,col]
  forecast <- data.frame(pred=as.numeric(dt[[2]]$pred),upper=as.numeric(dt[[2]]$upper),
                         lower=as.numeric(dt[[2]]$lower),time=dt[[2]]$time)
  forecast_melt <- melt(forecast,id='time')
  tsarima <- ggplot(forecast) +
    geom_ribbon(aes(x=time,ymin=lower,ymax=upper),alpha=0.3,fill='yellow') +
    geom_line(data=localframe,aes(x=time,y=var,group=1),alpha=0.5) +
    geom_line(data=localframe,aes(x=time,y=modelfit,group=1),color='red') +
    geom_line(data=forecast_melt,aes(x=time,y=value,group=variable,color=variable)) +
    scale_color_brewer(palette='Dark2') +
    scale_y_continuous(label=dollar) +
    labs(color='Type') +
    theme_bw() +
    xlab('Time') + ylab(paste(toupper(col),'Price')) +
    theme(axis.ticks=element_blank(),panel.grid.minor=element_blank(),
          panel.border=element_blank(),legend.position='bottom',
          legend.key = element_rect(fill = NA,colour = "white",size = 1),
          strip.background = element_rect(colour="white", fill="white"),
          axis.text.x=element_text(angle=45),
          axis.title.x=element_blank()) +
    annotate('text',x=min(localframe$time)+0.5,y=max(localframe$var)*1.1,color='dark green',size=4,label='grey = actual, red = fitted') 
  return(tsarima)
}


#getTimeSeriesPlotARIMA(dt,col)

#create residual plot for model fit
getResidualPlotARIMA <- function(dt){
  residualplot <- ggplot(dt[[1]],aes(x=as.character(date),y=modelresiduals)) +
    geom_bar(stat='identity',fill='dark green') +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
     ylab('Model Residuals') + xlab('Time') +
    theme(panel.grid=element_blank(),axis.ticks=element_blank(),
          panel.border=element_blank(),
          #axis.title.x=element_blank(),
          axis.text.x=element_blank()) +
    ggtitle(paste(dt[[3]],collapse='; '))
  return(residualplot)
}

#getResidualPlotARIMA(dt)




getPerChangData <- function(df,col,startpt,symbols){
  localframe <- df
  localframe$col <- localframe[,col]
  starttime <- localframe$time[startpt]
  localframe$sign <- ifelse(localframe$time<starttime,-1,1)
  transformlocalframe <- data.frame()
  for(i in 1:length(symbols)){
    dftemp <- subset(localframe,name==symbols[i])
    startpr <- dftemp$col[which(dftemp$time==starttime)]
    if(length(startpr)>0) {
      dftemp$startprice <- startpr
      dftemp_new <- ddply(dftemp,.(name),transform,perChange=(col-startprice)/startprice*sign)
      transformlocalframe <- rbind(transformlocalframe,dftemp_new)
    }
  }   
  return(transformlocalframe)
}

#perchangedata <- getPerChangData(df,col,startpt,symbols)


getPercentageChangePlot <- function(changedt,col,startpt,hlts,symbols,verticalrange){
  localframe <- changedt
  highlightdt <- subset(localframe,name==hlts)
  nonhighlightdt <- subset(localframe,name!=hlts)
  minx <- min(localframe$time)
  maxx <- max(localframe$time)
  maxchange <- verticalrange#min(max(abs(localframe$perChange)),verticalrange)
  yannotation <- seq(-maxchange,maxchange,length=7)
  labels <- percent(yannotation)
  labels[ceiling(length(labels)/2)] <- 'Starting\nTime'
  yperchange <- round(as.numeric(subset(highlightdt,time==maxx & name==hlts,select='perChange')),2)
  yperchange_nhl <- nonhighlightdt$perChange[nonhighlightdt$time==maxx & nonhighlightdt$name!=hlts]#round(as.vector(subset(nonhighlightdt,time==maxx & name!=hlts,select='perChange')),2)
  nonhlts <- symbols[symbols!=hlts]
  starttime <- localframe$time[startpt]
  pcplot <- ggplot(highlightdt,aes(x=time,y=perChange,group=name)) +
    geom_line(data=nonhighlightdt,aes(x=time,y=perChange,group=name),color='grey',size=1) +
    geom_line(size=1.5,alpha=0.7,color='dark blue') +
    geom_vline(xintercept=starttime,size=0.2,alpha=0.5) +
    annotate('text',x=starttime,y=yannotation,label=labels,size=3) +
    geom_hline(yintercept=0,size=0.2,alpha=0.5) +
    scale_y_continuous(expand=c(0,0),limits=c(-maxchange-0.2,maxchange+0.2),label=percent) +
    scale_x_continuous(expand=c(0,0),limits=c(minx-0.4,maxx+0.5)) +
    scale_color_discrete(guide='none') +
    theme_bw() +
    ylab(paste('Percentage Change in',toupper(col),'Price')) + xlab('Time') +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_line(linetype=2),
          axis.ticks=element_blank(),
          panel.border=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank()) +
    annotate('text',x=maxx,y=yperchange,label=paste(hlts,'\n',paste(yperchange*100,'%',sep=''),sep=''))+
    annotate('text',x=maxx,y=yperchange_nhl,label=nonhlts,size=3,alpha=0.5)
  return(pcplot)
}


#getPercentageChangePlot(changedt,col,startpt,hlts,symbols,verticalrange)


#calculate the total return
getTotalReturn <- function(changedt,col,startpt,hlts,nshare){
  starttime <- changedt$time[startpt]
  maxtime <- max(changedt$time)
  nshare_num <- as.numeric(nshare)
  startingPrice <- changedt[changedt$time==starttime & changedt$name==hlts ,col]
  PurchaseDateMonth <- changedt[changedt$time==starttime & changedt$name==hlts, 'month']
  Month <- levels(changedt[,'month'])[PurchaseDateMonth]
  PurchaseDateYear <- changedt[changedt$time==starttime & changedt$name==hlts, 'year']
  Year <- levels(changedt[,'year'])[PurchaseDateYear]
  
  PurchaseDateMonthEnd <- changedt[changedt$time==maxtime & changedt$name==hlts, 'month']
  MonthEnd <- levels(changedt[,'month'])[PurchaseDateMonthEnd]
  PurchaseDateYearEnd <- changedt[changedt$time==maxtime & changedt$name==hlts, 'year']
  YearEnd <- levels(changedt[,'year'])[PurchaseDateYearEnd]
  
  timedif <- maxtime - starttime
  endingPrice <- changedt[changedt$time==maxtime & changedt$name==hlts,col]
  totalReturn <- dollar((endingPrice - startingPrice) * nshare_num)
  yearlyReturnRate <- paste(round((endingPrice/startingPrice)**(1/timedif) - 1,4)*100,'%',sep='')
  
  totalReturnText <- paste('For the stock ',hlts,' purchased in ',Month,', ',Year,' at price ',dollar(startingPrice),
                           ', You make ',totalReturn,' if you sell it in ',
                           MonthEnd,', ',YearEnd,'.',' The year-over-year return rate is ',yearlyReturnRate,'.',sep='')
  return(totalReturnText)
}

#getTotalReturn(changedt,col,startpt,hlts,nshare)



