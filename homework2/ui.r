
library(shiny)

shinyUI(
  
  pageWithSidebar(
    
    headerPanel("IMDB Movie Ratings"),
    
    sidebarPanel(
      
      radioButtons(
        "mpaar",
        "MPAA Rating:",
        c('All','NC-17','PG','PG-13','R')        
      ),
      
      br(),
      
      checkboxGroupInput(
        "gen",
        "Movie Genres:",
        c("Action","Animation","Comedy","Drama","Documentary","Romance","Short")
      ),
      
      selectInput(
        "colorScheme",
        "Color Scheme:",
        choices = c("Default", "Accent", "Set1", "Set2", "Set3", "Dark2", "Pastel1","Pastel2", "Color-Blind Friendly")
      ),
      
      sliderInput(
        "dsize",
        "Dot Size:",
        min = 1, 
        max = 10,
        value = 2, 
        step = 1
      ),
      
      sliderInput(
        "alp",
        "Dot Alpha:",
        min = 0.1, 
        max = 1,
        value = 0.5,
        step = 0.1
      ),
      
      br(),
      
      wellPanel(
        p(strong("Model predictions")),
        checkboxInput(inputId = "mod_linear",    label = "Linear (dot-dash)"),
        checkboxInput(inputId = "mod_quadratic", label = "Quadratic (dashed)")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot",plotOutput("scatterPlot", height="600px")),
                 
        tabPanel("Genre by MPAA rating",tableOutput("table"))
      ),
      
      conditionalPanel("input.mod_linear == true",
                       p(strong("Linear model")),
                       verbatimTextOutput(outputId = "mod_linear_text")
      ),
      
      conditionalPanel("input.mod_quadratic == true",
                       p(strong("Quadratic model")),
                       verbatimTextOutput(outputId = "mod_quadratic_text")
      )
      
    ) 
  )
)
  