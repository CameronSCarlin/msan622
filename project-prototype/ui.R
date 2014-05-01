
library(shiny)

shinyUI(
  fluidPage(
    title='Financial Investment',
    headerPanel("Financial Investment Buddy"),
    
    fluidRow(
      column(
        3,
        conditionalPanel(
          condition="input.conditionedPanels==1",
        wellPanel(
          h5('Financial Report'),
          radioButtons(
            'Type',
            'Reports:',
            c('Balance Sheet','Cash Flow Statement','Income Statement')
          ),
          radioButtons(
            'Items',
            'Select Items:',
            c('Full Report','Specific Item(s)')
          ),
          conditionalPanel(
            condition="input.Items=='Specific Item(s)' & input.Type=='Balance Sheet'",
            selectInput('item','Choose Item(s): ',
                        BSitemList,
                        multiple=T)
            
          ),
          conditionalPanel(
            condition="input.Items=='Specific Item(s)' & input.Type=='Cash Flow Statement'",
            selectInput('item','Choose Item(s): ',
                        CFitemList,
                        multiple=T)
          ),
          conditionalPanel(
            condition="input.Items=='Specific Item(s)' & input.Type=='Income Statement'",
            selectInput('item','Choose Item(s): ',
                        ISitemList,
                        multiple=T)
          ),
          checkboxInput('guidance','Display Guidance',FALSE),
          radioButtons(
            'interval',
            'Quarterly or Annual: ',
            c('Quarter','Annual')
          )
          
        )
        ),
        
        conditionalPanel(
          condition="input.conditionedPanels==2",
          wellPanel(
            h5('Stocks & Currency'),
            radioButtons(
              'views',
              'Select Views:',
              c('Bubble','Time Series')
            ),
            textInput('ticker','Stock or Currency Ticker:','MSFT'),
            br(),
            dateInput("start", "Start Date", 
                      value = "1990-01-01", format = "dd-M-yyyy", 
                      min = "1990-01-01", max = "2014-12-31", 
                      startview = "month"),
            dateInput("end", "End Date", 
                      value = "1999-12-31", format = "dd-M-yyyy", 
                      min = "1990-01-01", max = "2014-12-31", 
                      startview = "month"),
            br(),
            checkboxInput('prediction','Show Prediction',FALSE),
            conditionalPanel(
              condition="input.prediction==true",
              sliderInput(
                "units", 
                "Units:", 
                min = 5, 
                max = 30,
                value = 5, 
                step = 1
              )
            ),
            br(),
            sliderInput(
              "mon", 
              "Months:", 
              min = 4, 
              max = 24,
              value = 12, 
              step = 1
            ),            
            br(),
            sliderInput(
              "start", 
              "Starting Point:",
              min = 1969, 
              max = 2014,
              value = 1969, 
              step = 1 / 12,
              round = FALSE, 
              ticks = TRUE,
              format = "####.##",
              animate = animationOptions(
                interval = 800, 
                loop = TRUE
              )
            )
            
                   
          )
        ),
        
        conditionalPanel(
          condition="input.conditionedPanels==3",
          wellPanel(
            h5('Multiple Stocks'),
            radioButtons(
              'mviews',
              'Select Views:',
              c('Small Multiple','Heatmap')
            ),
            textInput('ticker','Stock or Currency Ticker:','MSFT'),
            dateInput("start", "Start Date", 
                      value = "1990-01-01", format = "dd-M-yyyy", 
                      min = "1990-01-01", max = "2014-12-31", 
                      startview = "month"),
            dateInput("end", "End Date", 
                      value = "1999-12-31", format = "dd-M-yyyy", 
                      min = "1990-01-01", max = "2014-12-31", 
                      startview = "month")
            
            
          )
        ),
        
        conditionalPanel(
          condition="input.conditionedPanels==4",
          wellPanel(
            h5('Time Series'),
            textInput('ticker','Stock Ticker:','APPL'),
            radioButtons(
              'technique',
              'Select Technique:',
              c('STL Decomposition','Exponential Smoothing','ARIMA')
            )
            
            
            
            
          )
        ),
        
        
        conditionalPanel(
          condition="input.conditionedPanels==5",
          wellPanel(
            h5('Ticker Lookup'),
            textInput('ticker','Stock Ticker:','APPL')
            
            
          )
        )
        
      ),
      
      
      column(
        9,
        tabsetPanel(
          tabPanel("Financial Report",
                   value=1,
                   verbatimTextOutput(outputId = "report_text")
          ),
          tabPanel("Stock & Currency",
                   value=2,
                   plotOutput('overview')
          ),
          tabPanel('Multiple Stocks',
                   value=3,
                   plotOutput('multiple_stock')
          ),
          tabPanel('Time Series Analysis',
                   value=4,
                   plotOutput('timeseriesanalysis')
          ),
          tabPanel('Ticker Lookup',
                   value=5
          ),
          id="conditionedPanels"
          
        )
      )
    
    
    
    )
    
  )  
)