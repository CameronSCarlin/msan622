
library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Road Casualties in Great Britain 1969–84"),
  
  sidebarPanel(
    conditionalPanel(
      condition="input.conditionedPanels==1",
      selectInput('year',
                  'Year:',
                  unique(dt$year),
                  selected=1969:1974,
                  multiple=T
                  ),
      br(),
      selectInput('var',
                  'Y Variable:',
                  c('Drivers Killed'='DriversKilled','Drivers'='drivers','Front'='front',
                    'Rear'='rear','Total Distance'='kms','Petrol Price'='PetrolPrice',
                    'Van Killed'='VanKilled','Total Injured and Killed'='total'),
                  selected='total'
                  ),
      width = 3
      
      ),
    
    conditionalPanel(
      condition="input.conditionedPanels==2",
      radioButtons(
        'kill',
        'Statistics:',
        c('Killed & Injured','Killed')
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
        max = 1984,
        value = 1969, 
        step = 1 / 12,
        round = FALSE, 
        ticks = TRUE,
        format = "####.##",
        animate = animationOptions(
          interval = 800, 
          loop = TRUE
        )
      ),
      
      width = 3
      
      
      ),
    
    conditionalPanel(
      condition="input.conditionedPanels==3",
      selectInput('size',
                  'Size Variable:',
                  c('Drivers Killed'='DriversKilled_scale','Drivers'='drivers_scale',
                    'Front'='front_scale',
                    'Rear'='rear_scale',
                    'Van Drivers Killed'='VanKilled_scale',
                    'Total Injured and Killed'='total_scale'),
                  selected='total'
      ),
      
      br(),
      
      checkboxInput('text','Month Label',TRUE),
      
      br(),
      
      checkboxGroupInput(
        "months",
        "Months:",
        unique(dt$month)
      ),
      
      br(),
      
      sliderInput(
        "startY", 
        "Starting Year:",
        min = 1969, 
        max = 1984,
        value = 1969, 
        step = 1,
        round = FALSE, 
        ticks = TRUE,
        format = "####.##",
        animate = animationOptions(
          interval = 800, 
          loop = TRUE
        )
      ),
      
      width = 3
      
      
      
      )
    
    
    
    ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Star Plot",
               value=1,
               plotOutput('starPlot',width='100%',height='600px')
               ),
      tabPanel("Time Series Plot",
               value=2,
               plotOutput('timeSeriesDetailPlot'), 
               plotOutput('timeSeriesOverviewPlot',height='200px')
               ),
      tabPanel('Bubble Plot',
               value=3,
               plotOutput('bubblePlot',height='550px')
               ),
      id="conditionedPanels"
      )

    )
  
  
  
  
  
  
  ))