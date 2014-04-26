
library(shiny)
library(ggplot2)

source('global.r')


shinyServer(function(input, output) {
  
  cat("Press \"ESC\" to exit...\n")
  
  # Copy the data frame (don't want to change the data
  # frame for other viewers)
  localFrame <- dt
  localFrame_melt <- meltdf
  
  mon <- reactive(
      {
        if (is.null(input$months))
          return('All')
        return(input$months)       
      }  
  )
  
  output$starPlot <- renderPlot({
    print(starplot(localFrame,input$var,input$year))
  })
  
  output$timeSeriesOverviewPlot <- renderPlot({
    print(seriesOverview(localFrame_melt,input$kill,input$start,input$mon))
  })
  
  output$timeSeriesDetailPlot <- renderPlot({
    print(seriesDetail(localFrame_melt,input$kill,input$start,input$mon))
  })
  
  output$bubblePlot <- renderPlot({
    print(bubblePlot(localFrame,input$size,input$startY,input$text,mon()))
  })
  
  
  
})