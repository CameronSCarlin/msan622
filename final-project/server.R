
library(shiny)
library(ggplot2)


source('global.r')


shinyServer(function(input, output) {
  
  cat("Press \"ESC\" to exit...\n")
  
  output$istable <- renderTable(
    return(print(iskeyitemtable,row.names=F))  
  )
  
  #save the financial report for key items
  keyItemReport <- reactive({
    return(getFinancialReport(input$tickerFR2))
  })
  
  #create the key items
  keyItems <- reactive({
    itemreport <- keyItemReport()
    if(input$Items=='Key Items'){
      itemreport_new_BS <- getReport(itemreport,'Balance Sheet',input$intervalFR2)
      itemreport_new_IS <- getReport(itemreport,'Income Statement',input$intervalFR2)
      itemreport_new_CF <- getReport(itemreport,'Cash Flow Statement',input$intervalFR2)

      incomeStatement_all <- data.frame(Item=rownames(itemreport_new_IS),itemreport_new_IS)
      incomeStatement <- subset(incomeStatement_all,Item %in% c('Revenue','Cost of Revenue,
                                                                Total','Gross Profit','Income After Tax'))

      balanceSheet_all <- data.frame(Item=rownames(itemreport_new_BS),itemreport_new_BS)
      balanceSheet <- subset(balanceSheet_all,Item %in% c('Cash & Equivalents','Total Inventory','Total Receivables, Net',
                                                          'Total Assets','Total Current Liabilities','Total Liabilities',
                                                          'Total Debt','Total Equity'))

      cashFlowStatement_all <- data.frame(Item=rownames(itemreport_new_CF),itemreport_new_CF)
      cashFlowStatement <- subset(cashFlowStatement_all,Item %in% c('Cash from Operating Activities','Cash from Investing Activities','Cash from Financing Activities'))
      return(list(incomeStatement,balanceSheet,cashFlowStatement))
    }
    if(input$Items=='Specific Item(s)'){
      itemreport_new <- getReport(itemreport,input$TypeFR2,input$intervalFR2)
      if(input$TypeFR2=='Balance Sheet'){
        itemreport_selected <- itemreport_new[input$itemFR21,]
        Item <- input$itemFR21
      }else
        if(input$TypeFR2=='Cash Flow Statement'){
          itemreport_selected <- itemreport_new[input$itemFR22,]
          Item <- input$itemFR22
        }else{
          itemreport_selected <- itemreport_new[input$itemFR23,]
          Item <- input$itemFR23
        }
      itemreport_selected_new <- data.frame(Item,itemreport_selected)
      
      return(itemreport_selected_new)
    }

  })
  
  
  output$keyitemIStable <- renderDataTable({
    if(is.list(keyItems())) keyItems()[[1]]
  },options=list(iDisplayLength=20))
  output$keyitemBStable <- renderDataTable({
    if(is.list(keyItems())) keyItems()[[2]]
  },options=list(iDisplayLength=20))
  output$keyitemCFtable <- renderDataTable({
    if(is.list(keyItems())) keyItems()[[3]]
  },options=list(iDisplayLength=20))
  output$specificitemTable <- renderDataTable({
    if(is.data.frame(keyItems())) keyItems()
  },options=list(iDisplayLength=20))
 
  #save the financial report for full report
  financialReport <- reactive({
    return(getFinancialReport(input$tickerFR3))
  })
  
  #create the full reports
  fullreport <- reactive({
    reports <- financialReport()
    #'Balance Sheet','Cash Flow Statement','Income Statement'
    reports_new <- getReport(reports,input$TypeFR3,input$intervalFR3)
    return(list(reports_new,ncol(reports_new)))
  })
  
  output$selectInputUI <- renderUI({ 
    selectInput(
      'recent',
      'Available Reports:',
      1:fullreport()[[2]],
      multiple=T,
      selected=1:fullreport()[[2]])
  })
    
  somereport <- reactive({
    selectedreports <- as.numeric(input$recent)
    somereport <- data.frame(Item=rownames(fullreport()[[1]]),fullreport()[[1]][,selectedreports])
    return(somereport)
  })
  
  output$reportDataTable <- renderDataTable({
    somereport()
  },options=list(iDisplayLength=100))
  
  #stock overview and ticker search
  output$stockoverviewFR4 <- renderDataTable({
    stock
  },options=list(iDisplayLength=5))
  
  output$currencyoverviewFR4 <- renderDataTable({
    currencyName
  },options=list(iDisplayLength=5))
  
  brushRangeFR4<- reactive({
    var <- input$variableFR4
    maxbrush <- length(unique(stock[,var]))
    return(maxbrush)
  })
  
  output$sliderFR4UI <- renderUI({ 
    sliderInput("brushFR4", "Brush", min=1,max=brushRangeFR4(),
                value=c(1,10),
                step=1,
                format = "0",
                ticks = T )
  })
  
  #create overview FR4 data
  OverviewFR4Data <- reactive({
    return(getOverviewFRData(input$variableFR4))
  })
  
  
  output$overviewFR4Barplot <- renderPlot({
    print(getOverviewBarplotFR4(OverviewFR4Data(),input$brushFR4,input$variableFR4))
  })
  
  
  ##
  yearrange <- reactive({
    yearrange <- input$datadateST1
    startyear <- as.numeric(substring(yearrange[1],1,4))
    endyear <- as.numeric(substring(yearrange[2],1,4))
    return(c(startyear,endyear))
  })
  
  output$sliderInputST1UI <- renderUI({
    sliderInput('startpointST1','Starting Point:',min=yearrange()[1],max=yearrange()[2],value=yearrange()[1],
                step=1/12,round=FALSE,ticks=TRUE,format="####.##",
                animate=animationOptions(interval=4000,loop=TRUE))
  })
  
  
  
  
  
  
  #save the single stock data
  singlestock <- reactive({
    symbol <- input$tickerST1
    if(!(symbol %in% stock$Ticker)) stop("Please use only one or a correct ticker!")
    startdate <- input$datadateST1[1]
    enddate <- input$datadateST1[2]
    granularity <- input$datalevelST1
    singlestock <- getStock(symbol,startdate,enddate,granularity)
    return(singlestock)
  })
  
  #melt single stock data
  singlestock_melt <- reactive({
    singlestock_melt <- melt(singlestock(),id=c('name','date','time','year','month','week'))
    return(singlestock_melt)
  })
  
  #single stock time series overview
  output$singlestockOverviewPlot <- renderPlot({
    print(getSingleStockOverview(singlestock_melt(),input$timeseriesST1,input$startpointST1,input$detailwindowST1))
  })
  #single stock time series detailview
  output$singlestockDetailviewPlot <- renderPlot({
    print(getSingleStockDetailview(singlestock_melt(),input$timeseriesST1,input$startpointST1,input$detailwindowST1) )
  })
  
  
  #quantmod charts data
  quantchartsdata <- reactive({
    symbol <- input$tickerST5
    if(!(symbol %in% stock$Ticker)) stop("Please use only one or a correct ticker!")
    startdate <- input$datadateST5[1]
    enddate <- input$datadateST5[2]
    granularity <- input$datalevelST5
    quantchartsdata<-getXTSforQuantmodCharts(symbol,startdate,enddate,granularity)
  })
  
  #quantmod charts
  output$quantmodCharts <- renderPlot({
    print(getQuantCharts(quantchartsdata(),input$viewsST5,
                         input$tickerST5,input$daterangeST5,input$quantmodindST5))
  })
  
  
  #save the multiple stock data
  multistocks <- reactive({
    pre_symbols <- input$tickerST2
    symbols <- strsplit(pre_symbols,';')[[1]]
    if(!all(symbols %in% stock$Ticker)) stop("Some ticker(s) is wrong or not separated by semicolon.")
    startdate2 <- input$datadateST2[1]
    enddate2 <- input$datadateST2[2]
    granularity2 <- input$datalevelST2
    multistocks <- getStock(symbols,startdate2,enddate2,granularity2)
    return(multistocks) 
  })
  
  #multiple stocks section Uis
  timerangeST2 <- reactive({
    timerange <- input$datadateST2
    startyear <- as.numeric(substring(timerange[1],1,4))
    endyear <- as.numeric(substring(timerange[2],1,4))
    return(c(startyear,endyear))
  })
  
  output$sliderInputST2smallmultipleUI <- renderUI({
    sliderInput('startpointST2','Starting Time:',min=timerangeST2()[1],
                max=timerangeST2()[2],value=timerangeST2()[1],
                step=1,round=FALSE,ticks=TRUE,
                animate=animationOptions(interval=2000,loop=TRUE))
  })
  
  timelengthST2 <- reactive({
    tempdata <- multistocks()
    timelength <- length(unique(tempdata$date))
    return(timelength)
  })
  
  
  output$sliderInputST2bubbleUI <- renderUI({
    sliderInput('startpointbubbleST2','Starting Time:',min=1,max=timelengthST2(),value=1,
                step=1,round=FALSE,ticks=TRUE,
                animate=animationOptions(interval=2000,loop=TRUE))
  })
  
  #melt multistock data
  multistocks_melt_sm <- reactive({
    multistocks_melt_sm <- melt(multistocks(),id=c('name','date','time','year','month','week','Return'))
    return(multistocks_melt_sm)
  })
  
  #multiple stocks small multiple
  output$multiplestockSmallmultiplePLot <- renderPlot({
    print(getMultistockSmallMultiple(multistocks_melt_sm(),input$timeseriesST2,input$startpointST2))
  })
  
  #multiple stocks bubble plot
  output$multiplestockBubblePlot <- renderPlot({
    print(getBubblePlot(multistocks(),input$xvarST2,input$yvarST2,input$sizevarST2,input$startpointbubbleST2))
  })
  
  
  #save additional view data
  additionalViewData <- reactive({
    pre_symbols <- input$tickerST3
    symbols <- strsplit(pre_symbols,';')[[1]]
    if(!all(symbols %in% stock$Ticker)) stop("Some ticker(s) is wrong or not separated by semicolon.")
    startdate3 <- input$datadateST3[1]
    enddate3 <- input$datadateST3[2]
    granularity3 <- input$datalevelST3
    multistocks <- getStock(symbols,startdate3,enddate3,granularity3)
    return(multistocks)
  })
  
  brushvarRangeST3<- reactive({
    timelist <- unique(additionalViewData()$time)
    minbrush <- min(timelist)
    maxbrush <- max(timelist)
    return(c(minbrush,maxbrush))
  })
  
  output$sliderST3UI <- renderUI({ 
    sliderInput("brushST3", "Brush", min=brushvarRangeST3()[1],max=brushvarRangeST3()[2],
                value=c(brushvarRangeST3()[1],brushvarRangeST3()[2]),
                format = "0.00",
                ticks = T )
  })
  
  brushvarRangePCST3<- reactive({
    brushcol <- input$brushvarST3
    min_val <- min(additionalViewData()[,brushcol])
    max_val <- max(additionalViewData()[,brushcol])
    return(c(min_val,max_val))
  })
  
  output$sliderPCST3UI <- renderUI({ 
    sliderInput("brushvarPCST3", "Brush", min=brushvarRangePCST3()[1],max=brushvarRangePCST3()[2],
                value=c(brushvarRangePCST3()[1],brushvarRangePCST3()[2]),
                format = "0.00",
                ticks = T )
  })
  
  #additional view heat map
  output$additionalviewHeatMap <- renderPlot({
    print(getHeatMap(additionalViewData(),input$fillvarST3,input$brushST3,input$midrangeST3))
  })
  
  #merge additional view data with stock dataset for parallel coordinate plot
  additionalViewMergeStock <- reactive({
    df <- merge(additionalViewData(),stock,by.x='name',by.y='Ticker')
    return(df)
  })
  
  timelengthST3 <- reactive({
    tempdata <- additionalViewMergeStock()
    timelength <- length(unique(tempdata$date))
    return(timelength)
  })
  
  output$sliderInputPCST3UI <- renderUI({
    sliderInput('momentST3','Time:',min=1,max=timelengthST3(),value=1,
                step=1,round=FALSE,ticks=TRUE,
                animate=animationOptions(interval=2000,loop=TRUE))
  })
  
  #additional view parallel coordinate
  output$additionalviewParCoordPlot <- renderPlot({
    print(getParCoord(additionalViewMergeStock(),input$columnsST3,
                      input$grpcolumnST3,input$brushvarST3,input$brushvarPCST3,input$momentST3))
  }) 
  
  
  #save dividend data
  dividenddata <- reactive({
    pre_symbols <- input$tickerST4
    symbols <- strsplit(pre_symbols,';')[[1]]
    if(!all(symbols %in% stock$Ticker)) stop("Some ticker(s) is wrong or not separated by semicolon.")
    startdate4 <- input$datadateST4[1]
    enddate4 <- input$datadateST4[2]
    dividenddata <- getDividend(symbols,startdate4,enddate4)
    return(dividenddata)    
  })
  
  brushvarRangeST4<- reactive({
    min_val <- min(as.numeric(as.character(dividenddata()[,'year'])))
    max_val <- max(as.numeric(as.character(dividenddata()[,'year'])))
    return(c(min_val,max_val))
  })
  
  output$sliderST4UI <- renderUI({ 
    sliderInput("brushST4", "Brush:", min=brushvarRangeST4()[1],max=brushvarRangeST4()[2],
                value=c(brushvarRangeST4()[1],brushvarRangeST4()[2]),
                format = "0.00",
                ticks = T )
  })
  
  #multiple bar plot for the dividend data
  output$dividendBarGraph <- renderPlot({
    print(getBarGraph(dividenddata(),input$brushST4))
  })
  
  #save currency exchange rate data
  exchangerateData <- reactive({
    ticker1 <- input$tickerER1
    ticker2 <- input$tickerER2
    granularity <- input$datalevelER1
    start <- input$datadateER1[1]
    end <- input$datadateER1[2]
    exchangerateData <- getExchangeRate(ticker1,ticker2,granularity,start,end)
    return(exchangerateData)
  })
  
  brushvarRangeER1<- reactive({
    min_val <- min(exchangerateData()[,'time'])
    max_val <- max(exchangerateData()[,'time'])
    return(c(min_val,max_val))
  })
  
  output$sliderER1UI <- renderUI({ 
    sliderInput("brushER1", "Brush:", min=brushvarRangeER1()[1],max=brushvarRangeER1()[2],
                value=c(brushvarRangeER1()[1],brushvarRangeER1()[2]),
                format = "0.00",
                ticks = T )
  })
  
  #exchange rate time series
  output$exchangerateTimeSeriesPlot <- renderPlot({
    print(getERTimeSeries(exchangerateData(),input$timeseriesER1,input$brushER1))
  })
  
  #save STL decomposition data
  stldata <- reactive({
    symbol <- input$tickerTS1
    if(!(symbol %in% stock$Ticker)) stop("Please use only one or a correct ticker!")
    startdate <- input$datadateTS1[1]
    enddate <- input$datadateTS1[2]
    granularity <- input$datalevelTS1
    stldata <- getStock(symbol,startdate,enddate,granularity)
    return(stldata)
  })
  
  #create the STL decomposition data frame
  stldecomp <- reactive({
    stlts <- getSTLDecompData(stldata(),input$timeseriesTS1,input$frequencyTS1)
    return(stlts)
  })
  
  brushvarRangeTS1<- reactive({
    min_val <- min(stldecomp()[,'time'])
    max_val <- max(stldecomp()[,'time'])
    return(c(min_val,max_val))
  })
  
  output$sliderTS1UI <- renderUI({ 
    sliderInput("brushTS1", "Brush:", min=brushvarRangeTS1()[1],max=brushvarRangeTS1()[2],
                value=c(brushvarRangeTS1()[1],brushvarRangeTS1()[2]),
                format = "0.00",
                ticks = T )
  })
  
  #STL decomposition plot
  output$STLdecompositionPlot <- renderPlot({
    print(getSTLDecompositionPlot(stldecomp(),input$timeseriesTS1,input$brushTS1,input$scale_indTS1))
  })
  output$STLdecompositionPlotNew <- renderPlot({
    print(getSTLDecompositionPlot(stldecomp(),input$timeseriesTS1,input$brushTS1,input$scale_indTS1))
  })
  
  #STL seasonal adjusted plot
  output$STLSeasonalAdjustedPlot <- renderPlot({
    print(getSTLSeasonalAdjustedPlot(stldecomp(),input$timeseriesTS1,input$brushTS1))
  })
  
  #save exponential smoothing data
  exponentialsmoothingdata <- reactive({
    symbol <- input$tickerTS2
    if(!(symbol %in% stock$Ticker)) stop("Please use only one or a correct ticker!")
    startdate <- input$datadateTS2[1]
    enddate <- input$datadateTS2[2]
    granularity <- input$datalevelTS2
    exponentialsmoothingdata <- getStock(symbol,startdate,enddate,granularity)
    return(exponentialsmoothingdata)
  })
  
  brushvarRangeTS2<- reactive({
    min_val <- min(exponentialsmoothingdata()[,'time'])
    max_val <- max(exponentialsmoothingdata()[,'time'])
    return(c(min_val,max_val))
  })
  
  output$sliderTS2UI <- renderUI({ 
    sliderInput("brushTS2", "Brush:", min=floor(brushvarRangeTS2()[1]),max=ceiling(brushvarRangeTS2()[2]),
                step = 1,
                value=c(floor(brushvarRangeTS2()[1]),ceiling(brushvarRangeTS2()[2])),
                format = "0",
                ticks = T )
  })
  
  #create the exponential smoothing time series (data frame)
  esModelData <- reactive({
    esModelData <- getExponentialSmoothingData(exponentialsmoothingdata(),
                                               input$timeseriesTS2,input$frequencyTS2,
                                               input$modelTS2,input$modeltypeTS2,
                                               input$CITS2,input$predictionTS2,
                                               input$brushTS2,input$datalevelTS2)
    return(esModelData)
  })
  
  #exponential smoothing time series plot with prediction
  output$exponentialsmoothingTSPlot <- renderPlot({
    print(getESTimeSeriesPlot(exponentialsmoothingdata(),esModelData(),
                              input$timeseriesTS2,input$brushTS2))
  })
  
  #some other small things for exponential smoothing model such as the estimated parameters, goodness of fit measures
  output$exponentialsmoothingResidualPlot <- renderPlot({
    print(getESResidualPlot(esModelData()))
  })
  
  
  #save stock data for ARIMA
  arimadata <- reactive({
    symbol <- input$tickerTS3
    if(!(symbol %in% stock$Ticker)) stop("Please use only one or a correct ticker!")
    startdate <- input$datadateTS3[1]
    enddate <- input$datadateTS3[2]
    granularity <- input$datalevelTS3
    arimadata <- getStock(symbol,startdate,enddate,granularity)
    return(arimadata)
  })
  
  #the acf/pacf plot
  output$ARIMAacfPlot <- renderPlot({
    print(getACFPlot(arimadata(),input$timeseriesTS3,input$lagTS3))
  })
  
  #the Box-Ljung and Dickey-Fuller test
  output$BoxLjungTest <- renderPrint({
    print(getBJTest(arimadata(),input$timeseriesTS3,input$bltestTS3))
  })
  
  output$UnitRootTest <- renderPrint({
    print(getUnitRootTest(arimadata(),input$timeseriesTS3))
  })
  
  brushvarRangeTS3<- reactive({
    min_val <- min(arimadata()[,'time'])
    max_val <- max(arimadata()[,'time'])
    return(c(min_val,max_val))
  })
  
  output$sliderTS3UI <- renderUI({ 
    sliderInput("brushTS3", "Brush:", min=floor(brushvarRangeTS3()[1]),max=ceiling(brushvarRangeTS3()[2]),
                step = 1,
                value=c(floor(brushvarRangeTS3()[1]),ceiling(brushvarRangeTS3()[2])),
                format = "0",
                ticks = T )
  })
  
  #create the ARIMA model data
  arimaModelData <- reactive({
    return(getARIMAmodelDataframe(arimadata(),input$timeseriesTS3,input$pTS3,
                                  input$dTS3,input$qTS3,input$seasonalTS3,input$PTS3,
                                  input$DTS3,input$QTS3,input$STS3,
                                  input$olsdetrendTS3,input$boxcoxTS3,input$lambdaTS3,
                                  input$predictionTS3,input$datalevelTS3,input$CITS3,
                                  input$brushTS3))
  })
  
  #create the time series plot for the arima model
  output$TimeSeriesPlotArima <- renderPlot({
    print(getTimeSeriesPlotARIMA(arimaModelData(),input$timeseriesTS3))
  })
  
  #create the residual plot for the arima model
  output$ResidualPlotArima <- renderPlot({
    print(getResidualPlotARIMA(arimaModelData()))
  })
  
  #save investment calculator data
  investmentdata <- reactive({
    pre_symbols <- input$tickerIRC
    symbols <- strsplit(pre_symbols,';')[[1]]
    if(!all(symbols %in% stock$Ticker)) stop("Some ticker(s) is wrong or not separated by semicolon.")
    startdate <- input$datadateIRC[1]
    enddate <- input$datadateIRC[2]
    granularity <- input$datalevelIRC
    investmentdata <- getStock(symbols,startdate,enddate,granularity)
    return(investmentdata)
  })
  
  brushRangeFR4<- reactive({
    var <- input$variableFR4
    maxbrush <- length(unique(stock[,var]))
    return(maxbrush)
  })
  
  output$sliderFR4UI <- renderUI({ 
    sliderInput("brushFR4", "Brush", min=1,max=brushRangeFR4(),
                value=c(1,10),
                step=1,
                format = "0",
                ticks = T )
  })
  
  brushvarRangeIRC<- reactive({
    maxtimelength <- length(unique(investmentdata()[,'time']))
    return(maxtimelength)
  })
  
  output$sliderIRC <- renderUI({ 
    sliderInput("brushIRC", "Starting Time:", min=1,max=brushvarRangeIRC(),
                step=1,
                value=1,
                format = "0",
                ticks = T)
  })
  
  symbolsIRC <- reactive({
    pre_symbols <- input$tickerIRC
    symbols <- strsplit(pre_symbols,';')[[1]]
    return(symbols)
  })
  
  output$highlightIRC <- renderUI({
    selectInput('hightlightTSIRC','Hightlight:',symbolsIRC(),
                selected=c('open'))
  })
  
  #transform the investment data to contain percentage change
  perchangedata <- reactive({
    perchangedata <- getPerChangData(investmentdata(),input$timeseriesIRC,input$brushIRC,symbolsIRC())
    return(perchangedata)
  })
  
  
  verticalrangeIRC <- reactive({
    verticalrange <- max(abs(perchangedata()$perChange))
    return(verticalrange)
  })
  
  output$sliderVerticalRangeIRC <- renderUI({ 
    sliderInput("brushVerticalRangeIRC", "Percentage Range:", min=1,max=20,#verticalrangeIRC(),
                step=1,#verticalrangeIRC()/8,
                value=3,#verticalrangeIRC(),
                format = "0",
                ticks = T)
  })
  
 
  output$PercentageChangePlotIRC <- renderPlot({
    print(getPercentageChangePlot(perchangedata(),input$timeseriesIRC,
                                  input$brushIRC,input$hightlightTSIRC,
                                  symbolsIRC(),input$brushVerticalRangeIRC))
  })
  
  
  #total return
  output$totalReturn <- renderText({
    getTotalReturn(perchangedata(),input$timeseriesIRC,input$brushIRC,
                   input$hightlightTSIRC,input$SharesIRC)
  })
  
  
  
})