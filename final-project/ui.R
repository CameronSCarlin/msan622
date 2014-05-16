
library(shiny)

shinyUI(
  navbarPage("Financial Investment Buddy",
    tabPanel("Financial Reports",
      fluidPage(
        fluidRow(
          column(3,
                 conditionalPanel(
                    condition="input.conditionedFR==1",
                    wellPanel(h4('Guidance'),
                              radioButtons('TypeFR1','Reports:',c('Balance Sheet','Cash Flow Statement','Income Statement')))),
                 
                 conditionalPanel(
                    condition="input.conditionedFR==2",
                    wellPanel(h4('Key Items'),
                              textInput('tickerFR2','Stock Ticker:','MSFT'),
                              radioButtons('intervalFR2','Quarterly or Annual:',c('Quarter','Annual')),
                              radioButtons('Items','Select Items:',c('Key Items','Specific Item(s)')),
                              conditionalPanel(
                                condition="input.Items=='Specific Item(s)'",
                                radioButtons('TypeFR2','Reports:',c('Balance Sheet','Cash Flow Statement','Income Statement'))),
                              conditionalPanel(
                                condition="input.Items=='Specific Item(s)' & input.TypeFR2=='Balance Sheet'",
                                selectInput('itemFR21','Choose Item(s): ',BSitemList,multiple=T,selected = c('Cash & Equivalents'))),
                              conditionalPanel(
                                condition="input.Items=='Specific Item(s)' & input.TypeFR2=='Cash Flow Statement'",
                                selectInput('itemFR22','Choose Item(s): ',CFitemList,multiple=T,selected = c('Net Income/Starting Line'))),
                              conditionalPanel(
                                condition="input.Items=='Specific Item(s)' & input.TypeFR2=='Income Statement'",
                                selectInput('itemFR23','Choose Item(s): ',ISitemList,multiple=T,selected = c('Revenue'))))),
                  
                 conditionalPanel(
                    condition="input.conditionedFR==3",
                    wellPanel(h4('Full Reports'),
                      textInput('tickerFR3','Stock Ticker:','MSFT'),
                      radioButtons('TypeFR3','Reports:',c('Balance Sheet','Cash Flow Statement','Income Statement')),
                      radioButtons('intervalFR3','Quarterly or Annual:',c('Quarter','Annual')),
                      htmlOutput("selectInputUI"),
                      helpText("Smaller number means more recent report."))),
                 
                 conditionalPanel(
                   condition="input.conditionedFR==4",
                   wellPanel(
                     h4('Overviews/Ticker Search'),
                     selectInput('variableFR4','Choose variable to view: ',c('Exchange','Country','Category.Name'),
                                 selected ='Exchange'),
                     htmlOutput('sliderFR4UI')
                     ))
                 
                ),
                    
                    
          column(9,
            tabsetPanel(
              tabPanel("Guidance",value=1,
                conditionalPanel(
                  condition="input.TypeFR1=='Balance Sheet'",
                  includeHTML("balancesheet.html")
                  ),
                conditionalPanel(
                  condition="input.TypeFR1=='Cash Flow Statement'",
                  includeHTML("cashflowstatement.html")
                  ),
                conditionalPanel(
                  condition="input.TypeFR1=='Income Statement'",
                  includeHTML("incomestatement.html"),
                  tableOutput("istable"))),
              tabPanel("Key Items",value=2,
                conditionalPanel(
                  condition="input.Items=='Key Items'",
                  h4('Balance Sheet Key Items:'),
                  dataTableOutput("keyitemBStable"),
                  h4('Cash Flow Statement Key Items:'),
                  dataTableOutput("keyitemCFtable"),
                  h4('Income Statement Key Items:'),
                  dataTableOutput("keyitemIStable")),
                conditionalPanel(
                  condition="input.Items=='Specific Item(s)'",
                  dataTableOutput("specificitemTable"))),
              tabPanel("Full Reports",value=3,
                  dataTableOutput('reportDataTable')),
              tabPanel("Overview of Stocks",value=4,                       
                       fluidRow(
                         column(6,
                                plotOutput('overviewFR4Barplot')),
                         column(6,
                                dataTableOutput('currencyoverviewFR4'))),
                       dataTableOutput('stockoverviewFR4')),              
              id="conditionedFR"))
        )                 
      )                     
    ),
             
    tabPanel('Stocks',
      fluidPage(
        fluidRow(
          column(3,
            conditionalPanel(
              condition="input.conditionedST==1",
              wellPanel(
                h4('Single Stock'),
                textInput('tickerST1','Stock Ticker:','MSFT'),
                helpText("Type the ticker of one single stock"),
                dateRangeInput('datadateST1','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelST1','Data Level:',c('days','weeks','months','quarters','years'),selected='months'),
                selectInput('timeseriesST1','Choose Time Series:',c('open','close','low','high'),
                            multiple=T,selected=c('open','close','low','high')),
                sliderInput('detailwindowST1','Detail View Window:',min=4,max=24,value=12,step=1),
                htmlOutput("sliderInputST1UI"))),
            
            conditionalPanel(
              condition="input.conditionedST==2",
              wellPanel(
                h4('Multiple Stocks'),
                radioButtons('viewsST2','Select Views:',c('Small Multiple','Bubble Plot')),
                textInput('tickerST2','Stock Ticker:',"MSFT;TIF;MAR;BAC"),
                helpText("Type any number of stock tickers separated by semicolon."),
                dateRangeInput('datadateST2','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelST2','Data Level:',c('days','weeks','months','quarters','years'),
                             selected='months'),
                conditionalPanel(
                  condition="input.viewsST2=='Small Multiple'",
                  selectInput('timeseriesST2','Choose Time Series:',c('open','close','low','high'),
                              multiple=T,selected=c('open','close','low','high'))),
                conditionalPanel(
                  condition="input.viewsST2=='Bubble Plot'",
                  selectInput('sizevarST2','Choose Size Variable:',c('Return','volume'),
                              selected='volume'),
                  selectInput('xvarST2','Choose X Variable:',c('low','open'),
                              selected='low'),
                  selectInput('yvarST2','Choose Y Variable:',c('high','close'),
                              selected='high')),
                conditionalPanel(
                  condition="input.viewsST2=='Small Multiple'",
                  htmlOutput("sliderInputST2smallmultipleUI")),
                conditionalPanel(
                  condition="input.viewsST2=='Bubble Plot'",
                  htmlOutput("sliderInputST2bubbleUI"))
              )),
            
            
            
            conditionalPanel(
              condition="input.conditionedST==3",
              wellPanel(
                h4('Additional Views'),
                radioButtons('viewsST3','Select Views:',c('Heat Map','Parallel Coordinate')),
                textInput('tickerST3','Stock Ticker:',"MSFT;TIF;MAR;BAC"),
                helpText("Type any number of stock tickers separated by semicolon."),
                dateRangeInput('datadateST3','Data Date Range:',start='2010-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelST3','Data Level:',
                             c('days','weeks','months','quarters','years'),
                             selected='months'),
                conditionalPanel(
                  condition="input.viewsST3=='Heat Map'",
                  selectInput('fillvarST3','Fill Variable:',
                              c('open','close','low','high','volume','Return'),
                              selected='open'),
                  sliderInput("midrangeST3","Gradient Range:",min = 0,max = 1,value = c(0.45, 0.55),
                              step = 0.05,format = "0.00",ticks = TRUE),
                  htmlOutput('sliderST3UI')),
                conditionalPanel(
                  condition="input.viewsST3=='Parallel Coordinate'",
                  selectInput('columnsST3','Select Variables:',
                    c('open','high','low','close','volume','adjusted','Return'),
                    selected=c('open','high','low','close','volume','adjusted','Return'),
                    multiple=T),
                  selectInput('grpcolumnST3','Group Column:',
                              c('Exchange','Country','Category.Name')),
                  selectInput('brushvarST3','Brush Variable:',
                    c('open','high','low','close','volume','adjusted','Return'),
                    selected='open',multiple=F),
                  htmlOutput('sliderPCST3UI'),
                  htmlOutput('sliderInputPCST3UI')))),
            
            conditionalPanel(
              condition="input.conditionedST==4",
              wellPanel(
                h4('Dividends'),
                textInput('tickerST4','Stock Ticker:',"MSFT;TIF;MAR;BAC"),
                helpText("Type any number of stock tickers separated by semicolon."),
                dateRangeInput('datadateST4','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                htmlOutput('sliderST4UI'))),
            
            conditionalPanel(
              condition="input.conditionedST==5",
              wellPanel(
                h4('Quantmod Charts'),
                radioButtons('viewsST5','Select Views:',c('line','bars','candlesticks','matchsticks')),
                textInput('tickerST5','Stock Ticker:','MSFT'),
                helpText("Type the ticker of one single stock"),
                dateRangeInput('datadateST5','Data Date Range:',start='2000-01-01',end='2014-04-30'),
                radioButtons('datalevelST5','Data Level:',c('days','weeks','months','quarters','years'),selected='months'),
                selectInput('quantmodindST5','Choose Quantmod Indicators:',
                            quantmod_indicators[,1],
                            multiple=T,selected=c('Volume','Bollinger Band Width','Commodity Channel Index')),
                helpText('Choose at least one financial indicator.'),
                dateRangeInput('daterangeST5','Display Range:',start='2000-01-01',end='2014-04-30')
              ))
            
            ),
                  
          column(9,
            tabsetPanel(
              tabPanel("Single Stock",value=1,
                         plotOutput('singlestockDetailviewPlot',height='300px'),
                         plotOutput('singlestockOverviewPlot')),
              tabPanel("Quantmod Charts",value=5,
                       plotOutput('quantmodCharts',height='700px')),
              tabPanel("Multiple Stocks",value=2,
                       conditionalPanel(
                         condition="input.viewsST2 == 'Small Multiple'",
                         plotOutput('multiplestockSmallmultiplePLot',height='700px')),
                       conditionalPanel(
                         condition="input.viewsST2 == 'Bubble Plot'",
                         plotOutput('multiplestockBubblePlot',height='700px'))
                       ),
              tabPanel("Additional Views",value=3,
                       conditionalPanel(
                         condition="input.viewsST3 == 'Heat Map'",
                         plotOutput('additionalviewHeatMap',height='700px')),
                       conditionalPanel(
                         condition="input.viewsST3 == 'Parallel Coordinate'",
                         plotOutput('additionalviewParCoordPlot',height='700px'))),
              tabPanel("Dividends",value=4,
                       plotOutput('dividendBarGraph',height='700px')),
              id="conditionedST")
          )
        )
      )      
    ),
             
    tabPanel('Currency/Exchange Rate',    
      fluidPage(
        fluidRow(
          column(3,
            conditionalPanel(
              condition="input.conditionedER==1",
              wellPanel(
                h4("Currency Exchange Rates"),
                textInput('tickerER1','Currency Ticker 1','USD'),
                textInput('tickerER2','Currency Ticker 2','CNY'),
                dateRangeInput('datadateER1','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelER1','Data Level:',c('days','weeks','months','quarters','years'),
                             selected='months'),
                selectInput('timeseriesER1','Choose Time Series:',c('open','close','low','high'),
                            multiple=T,selected=c('open','close','low','high')),
                htmlOutput('sliderER1UI')))),
          
          column(9,
            tabsetPanel(
              tabPanel("Currency Exchange Rates",value=1,
                       plotOutput('exchangerateTimeSeriesPlot',height='700px')),
              id="conditionedER")
          )
        )
      )                           
    ),
             
    tabPanel('Time Series Analysis',
      fluidPage(
        fluidRow(
          column(3,
            conditionalPanel(
              condition="input.conditionedTS==1",
              wellPanel(
                h4("STL Decomposition"),
                textInput('tickerTS1','Stock Ticker:','MSFT'),
                helpText("Type the ticker of one single stock"),
                dateRangeInput('datadateTS1','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelTS1','Data Level:',c('days','weeks','months','quarters','years'),
                             selected='months'),
                numericInput('frequencyTS1','Time Series Period:',12,min=2,max=24),
                selectInput('timeseriesTS1','Choose Time Series:',c('open','close','low','high'),
                            selected=c('open')),
                htmlOutput('sliderTS1UI'),
                checkboxInput('scale_indTS1','Scale',FALSE),
                checkboxInput('rmseasonalTS1','Remove Seasonal',FALSE),
                checkboxInput('helptextTS1','Display Instruction',FALSE))),
            
            conditionalPanel(
              condition="input.conditionedTS==2",
              wellPanel(
                h4("Exponential Smoothing"),
                textInput('tickerTS2','Stock Ticker:','MSFT'),
                helpText("Type the ticker of one single stock"),
                dateRangeInput('datadateTS2','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelTS2','Data Level:',c('days','weeks','months','quarters','years'),
                             selected='months'),
                selectInput('timeseriesTS2','Choose Time Series:',c('open','close','low','high'),
                            selected=c('open','close','low','high')),
                numericInput('frequencyTS2','Time Series Period:',7,min=2,max=24),
                radioButtons('modelTS2','Select Smoothing Model:',
                             c('Simple Exponential','Double Exponential','Seasonal Exponential','Holt-Winters')),
                conditionalPanel(
                  condition="input.modelTS2 == 'Seasonal Exponential' | input.modelTS2 == 'Holt-Winters'",
                  radioButtons('modeltypeTS2','Seasonal Model Type:',
                               c('additive','multiplicative'))),
                h5("Forecasting:"),
                numericInput('predictionTS2','Predictions Ahead:',4,min=1,max=12),
                sliderInput('CITS2','Confidence Interval:',min=0.8,max=0.99,value=0.95,
                            step=0.01,round=FALSE,ticks=TRUE),
                htmlOutput('sliderTS2UI'),
                checkboxInput('helptextTS2','Display Instruction',FALSE))),
            
            conditionalPanel(
              condition="input.conditionedTS==3",
              wellPanel(
                h4("ARIMA"),
                textInput('tickerTS3','Stock Ticker:','MSFT'),
                helpText("Type the ticker of one single stock"),
                dateRangeInput('datadateTS3','Data Date Range:',start='2000-01-01',
                               end='2014-04-30'),
                radioButtons('datalevelTS3','Data Level:',c('days','weeks','months','quarters','years'),
                             selected='months'),
                selectInput('timeseriesTS3','Choose Time Series:',c('open','close','low','high'),
                            selected=c('open')),
                sliderInput('lagTS3','Lags for ACF and PACF:',min=5,max=25,value=10,
                            step=1,round=FALSE,ticks=TRUE),
                textInput('bltestTS3','Box-Ljung Test Lag Length:','10'),
                numericInput('pTS3','Autoregressive Order:',1,min=0,max=24),
                numericInput('dTS3','Integrated Order:',0,min=0,max=5),
                numericInput('qTS3','Moving Average Order:',1,min=0,max=24),
                checkboxInput('seasonalTS3','Is Seasonal Time Series?',FALSE),
                conditionalPanel(
                  condition="input.seasonalTS3==true",
                  numericInput('PTS3','Seasonal AR Order:',0,min=0,max=24),
                  numericInput('DTS3','Seasonal Integrated Order:',0,min=0,max=3),
                  numericInput('QTS3','Seasonal MA Order:',0,min=0,max=24),
                  numericInput('STS3','Period:',12,min=1,max=24)),
                checkboxInput('olsdetrendTS3','OLS Detrending',FALSE),
                checkboxInput('boxcoxTS3','Box Cox Transformation',FALSE),
                conditionalPanel(
                  condition="input.boxcoxTS3==true",
                  numericInput('lambdaTS3','Lambda for Boxcox Transformation:',0,min=0,max=24,step=0.1)),
                h5("Forecasting:"),
                numericInput('predictionTS3','Predictions Ahead:',4,min=1,max=12),
                sliderInput('CITS3','Confidence Interval:',min=0.8,max=0.99,value=0.95,
                            step=0.01,round=FALSE,ticks=TRUE),
                htmlOutput('sliderTS3UI'),
                checkboxInput('helptextTS3','Display Instruction',FALSE)))
                 
          ),
          
          column(9,
            tabsetPanel(
              tabPanel("STL Decomposition",value=1,
                       conditionalPanel(
                         condition="input.rmseasonalTS1 == false",
                         plotOutput('STLdecompositionPlot',height='700px')),
                       conditionalPanel(
                         condition="input.rmseasonalTS1 == true",
                         plotOutput('STLdecompositionPlotNew'),
                         plotOutput('STLSeasonalAdjustedPlot',height='300px')),
                       conditionalPanel(
                         condition="input.helptextTS1 == true",
                         includeHTML("stlguide.html"))),
              tabPanel("Exponential Smoothing",value=2,
                       plotOutput('exponentialsmoothingTSPlot',height='400px'),
                       plotOutput('exponentialsmoothingResidualPlot',height='300px'),
                       conditionalPanel(
                         condition="input.helptextTS2 == true",
                         includeHTML("exponentialsmoothingguide.html"))),
              tabPanel("ARIMA",value=3,
                       fluidRow(
                         column(9,plotOutput('ARIMAacfPlot',height='380px')),
                         column(3,
                                fluidRow(
                                  column(12,verbatimTextOutput(outputId = "BoxLjungTest"))),
                                fluidRow(
                                  column(12,verbatimTextOutput('UnitRootTest'))))
                         ),
                       plotOutput('TimeSeriesPlotArima'),
                       plotOutput('ResidualPlotArima'),
                       conditionalPanel(
                         condition="input.helptextTS3 == true",
                         includeHTML("arimaguide.html"))
                       ),
              id="conditionedTS")     
          )
        )
      )                          
    ),
    
    
    tabPanel('Investment Return Calculator',
          
          fluidRow(wellPanel(htmlOutput('sliderIRC'))),
          
          fluidRow(plotOutput('PercentageChangePlotIRC',height='500px')),
      
          wellPanel(
             fluidRow(
               column(2,
                      textInput('tickerIRC','Stock Ticker:','MSFT;YHOO;MAR;TIF;BAC'),
                      helpText("Type any number of stock tickers separated by semicolon.")),
               column(1,
                      dateRangeInput('datadateIRC','Data Range:',start='2000-01-01',
                                     end='2014-04-30')),
               column(1,
                      radioButtons('datalevelIRC','Data Level:',c('months','quarters','years'))),
               column(2,
                      selectInput('timeseriesIRC','Choose Time Series:',c('open','close','low','high'),
                                  selected=c('open'))),
               column(2,
                      htmlOutput('highlightIRC'),
                      htmlOutput('sliderVerticalRangeIRC')),
               column(2,
                      textInput('SharesIRC','Number of Shares:','100')),
               column(2,
                      h4('Total Return:'),
                      textOutput('totalReturn'))))
             
    )         
    
  ) 
)