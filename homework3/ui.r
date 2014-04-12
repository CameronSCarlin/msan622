
library(shiny)

shinyUI(
  fluidPage(#theme = "bootstrap1.css",
      
            title = "USA State Data Explorer",
            headerPanel("USA State Data Explorer"),
            
            fluidRow(
              column(
                2,
                wellPanel(
                  h5('Heatmap Control'),
                  checkboxGroupInput('region',
                                     'Region Filter:',
                                    c('South','West','Northeast','North Central')),
                  selectInput('fill',
                              'Fill Variable:',
                              c('Population','Income','Illiteracy','Life.Exp','Murder','HS.Grad','Frost'),
                              selected='Murder'),
                  wellPanel(
                    p('Color Scheme'),
                    textInput('low','Low Color:','blue'),
                    textInput('high','High Color:','red')
                    )
                  )
                
                
                ),
              
              column(
                4,
                plotOutput('heatmap',height='400pt')
                ),
              
              column(
                4,
                plotOutput('bubble',height='400pt')
                ),
              
              column(
                2,
                wellPanel(
                  h5('Bubble Control'),
                  selectInput('xvar','X Variable:',
                              c('Population','Income','Illiteracy','Life.Exp','Murder','HS.Grad','Frost'),
                              selected='Income'),
                  selectInput('yvar','Y Variable:',
                              c('Population','Income','Illiteracy','Life.Exp','Murder','HS.Grad','Frost'),
                              selected='Illiteracy'),
                  selectInput('sizevar','Size Variable:',
                              c('Population','Income','Illiteracy','Life.Exp','Murder','HS.Grad','Frost'),
                              selected='Population'),
                  selectInput('colvar','Color Variable',
                              c('Region','Division'),
                              selected='Region'),
                  selectInput('state','Choose States: ',
                              unique(df$Abbrev),
                              multiple=T)
                  )
                )
              ),
            
            hr(),
            
            fluidRow(
              column(
                2,
                wellPanel(
                  selectInput(
                    'columns',
                    'Select Variables:',
                    c('Population'=1,'Income'=2,'Illiteracy'=3,
                      'Life.Exp'=4,'Murder'=5,'HS.Grad'=6,
                      'Frost'=7),
                    selected=1:5,
                    multiple=T
                  ),
                  selectInput(
                    'grpcolumn',
                    'Group Column:',
                    c('Region','Division')
                    ),
                  selectInput(
                    'brushvar',
                    'Brush Variable:',
                    c('Population'=1,'Income'=2,'Illiteracy'=3,
                      'Life.Exp'=4,'Murder'=5,'HS.Grad'=6,
                      'Frost'=7),
                    selected='Murder',
                    multiple=F
                    ),
                  htmlOutput("sliderUI")
                  )
                  ),
              column(
                10,
                plotOutput('parcoor')
                )
              
              ),
            
            
            
            hr(),
            
            fluidRow(
              column(
              2,
              wellPanel(
                checkboxInput('Main','Display Scatterplot Matrix',FALSE)),
                conditionalPanel(
                  condition = "input.Main == true",
                  radioButtons(
                    'all',
                    'Display All or Highlight:',
                    c('All','Highlight')
                    )
                  ),
                conditionalPanel(
                  condition = "input.all == 'Highlight' & input.Main == true",
                  selectInput(
                    'highlight_row',
                    'Row Variable to Highlight:',
                    c('Population'=1,'Income'=2,'Illiteracy'=3,
                      'Life.Exp'=4,'Murder'=5,'HS.Grad'=6)
                  ),
                  selectInput(
                    'highlight_col',
                    'Column Variable to Highlight:',
                    c('Population'=1,'Income'=2,'Illiteracy'=3,
                      'Life.Exp'=4,'Murder'=5,'HS.Grad'=6)
                  )
                )),              
              conditionalPanel(
                condition= "input.Main == true",
                column(
                  10,
                  plotOutput('scatterMatrix')
                )
                
                )
              
              
            ),
            
            hr()
            
            
                  
            
  

))