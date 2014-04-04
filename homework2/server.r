
library(ggplot2)
library(shiny)

# Loads global data to be shared by all sessions.
loadData <- function(){
  data(movies)
  mvs <- movies[movies$budget>0 & is.na(movies$budget) == F & movies$mpaa != '',]
  genre <- rep(NA, nrow(mvs))
  count <- rowSums(mvs[, 18:24])
  genre[which(count > 1)] = "Mixed"
  genre[which(count < 1)] = "None"
  genre[which(count == 1 & mvs$Action == 1)] = "Action"
  genre[which(count == 1 & mvs$Animation == 1)] = "Animation"
  genre[which(count == 1 & mvs$Comedy == 1)] = "Comedy"
  genre[which(count == 1 & mvs$Drama == 1)] = "Drama"
  genre[which(count == 1 & mvs$Documentary == 1)] = "Documentary"
  genre[which(count == 1 & mvs$Romance == 1)] = "Romance"
  genre[which(count == 1 & mvs$Short == 1)] = "Short"
  mvs$genre <- genre
  return(mvs)
}

# Label formatter for numbers in millions
million_formatter <- function(x) {
  return(sprintf("%dm", round(x / 1000000)))
}

# Create plotting function
# mpaar: mpaa rating, gen: genre, alp: alpha, dsize: dot size
getPlot <- function(localFrame, mpaar ,gen, lm_mod, qr_mod , alp, dsize, colorScheme = 'Default'){
  #create a base plot
  localPlot <- ggplot(localFrame,aes(x=budget,y=rating,color=mpaa)) + 
    xlab('Budget') +
    ylab('IMDB Rating') +
    ylim(c(0,10)) +
    scale_x_continuous(labels = million_formatter) +
    theme(legend.position='bottom') +
    theme(panel.grid.minor=element_blank())
  
  if (mpaar == 'All' & gen == 'All' ) {
    localPlot <- localPlot + 
      geom_point(alpha=alp,size=dsize)
    if (lm_mod) {
      localPlot <- localPlot + 
        geom_smooth(method = lm, se=FALSE, size=0.75, linetype="dotdash")
    }
    if (qr_mod) {
      localPlot <- localPlot +
        geom_smooth(method = lm, se=FALSE, formula = y ~ x + I(x^2),
                    size = .75, linetype = 'dashed')
    }
  }
  else if (mpaar != 'All' & gen == 'All') {
    localPlot <- localPlot +
      geom_point(alpha=0.25,size=dsize)
    if (nrow(localFrame[localFrame$mpaa==mpaar,])!=0){
      localPlot <- localPlot + geom_point(data=localFrame[localFrame$mpaa==mpaar,],aes(x=budget,y=rating),color='dark blue',alpha=alp,size=dsize)
      if (lm_mod) {
        localPlot <- localPlot +
          geom_smooth(method = lm, se=FALSE, data = localFrame[localFrame$mpaa==mpaar,], aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dotdash")
      }
      if (qr_mod) {
        localPlot <- localPlot +
          geom_smooth(method = lm, se=FALSE, formula = y ~ x + I(x^2), data = localFrame[localFrame$mpaa==mpaar,], aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dashed")
      }
    }
    else {    
      localPlot <- localPlot + 
        annotate("text",x=0,y=5,label="There is no movie in the selected combination of mpaa and genre!",colour = 'dark blue', size = 20)
    }     
  }
  else if (mpaar == 'All' & gen != 'All' ) {
    localPlot <- localPlot +
      geom_point(alpha=0.25,size=dsize) 
    if (nrow(localFrame[localFrame$genre %in% gen,])!=0){
      localPlot <- localPlot + geom_point(data=localFrame[localFrame$genre %in% gen,],aes(x=budget,y=rating),color='dark blue',alpha=alp,size=dsize)
      if (lm_mod) {
        localPlot <- localPlot +
          geom_smooth(method = lm, se=FALSE, data = localFrame[localFrame$genre %in% gen,], aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dotdash")
      }
      if (qr_mod) {
        localPlot <- localPlot +
          geom_smooth(method = lm, se=FALSE, formula = y ~ x + I(x^2), data = localFrame[localFrame$genre %in% gen,], aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dashed")
      }
    }
    else {
      localPlot <- localPlot + 
        annotate("text",x=0,y=5,label="There is no movie in the selected combination of mpaa and genre!",colour = 'dark blue', size = 20)
    }   
  }
  else {
    localPlot <- localPlot +
      geom_point(alpha=0.25,size=dsize) 
    if (nrow(localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,])!=0){
      localPlot <- localPlot + geom_point(data=localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,],aes(x=budget,y=rating),color='dark blue',alpha=alp,size=dsize) 
      if (lm_mod) {
        if(nrow(localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,])>1){
          localPlot <- localPlot +
            geom_smooth(method = lm, se=FALSE, data = localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,], aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dotdash")
        }
      }
      if (qr_mod) {
        if(nrow(localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,])>1){
          localPlot <- localPlot +
            geom_smooth(data = localFrame[localFrame$mpaa==mpaar & localFrame$genre %in% gen,],method = lm, se=FALSE, formula = y ~ x + I(x^2), aes(x=budget,y=rating),color='dark blue',size=0.75, linetype="dashed")
        }
      }
    }
    else {
      x0 <- (min(localFrame$budget) + max(localFrame$budget))/2
      localPlot <- localPlot + 
        annotate("text",x=x0,y=5,label="There is no movie in the selected combination of mpaa and genre. \n Please reselect!",colour = 'dark blue', size = 7.5)
    }
  }
  
  
  if (colorScheme == 'Accent') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Accent') 
  }
  else if (colorScheme == 'Set1') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Set1')
  }
  else if (colorScheme == 'Set2') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Set2')
  }
  else if (colorScheme == 'Set3') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Set3')
  }
  else if (colorScheme == 'Dark2') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Dark2')
  }
  else if (colorScheme == 'Pastel1') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Pastel1')
  }
  else if (colorScheme == 'Pastel2') {
    localPlot <- localPlot +
      scale_color_brewer(type='qual',palette = 'Pastel2')
  }
  else if (colorScheme == 'Color-Blind Friendly') {
    localPlot <- localPlot +
      scale_color_manual(values = palette1)
  }
  else {
    localPlot <- localPlot
  }
  
  return(localPlot)
}


#### GLOBAL OBJECTS ####

# Shared data
globalData <- loadData()

# Color-blind friendly palette from http://jfly.iam.u-tokyo.ac.jp/color/
palette1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Shiny Server ####

# Create shiny server. Input comes from the UI input
# controls, and the resulting output will be displayed on
# the page.

shinyServer(function(input,output){
  
  cat("Press \"ESC\" to exit...\n")
  
  # Copy the data frame (don't want to change the data
  # frame for other viewers)
  localFrame <- globalData
  
  gen_row <- reactive(
    {
      if (is.null(input$gen))
        return('All')
      return(input$gen)
      
    }  
  )
  
  emp_data <- 
  
  output$table <- renderTable(
    {
      return(xtabs(~genre + mpaa,drop.unused.levels=T,data=localFrame))
    }
    
  )
  
  output$mod_linear_text <- renderPrint(
    {
      if(input$mpaar == 'All' & gen_row() == 'All'){
        return(summary(lm(rating ~ budget, data=localFrame)))
      }
      else if(input$mpaar != 'All' & gen_row() == 'All'){
        if(nrow(localFrame[localFrame$mpaa==input$mpaar,])!=0){
          return(summary(lm(rating ~ budget, data=localFrame[localFrame$mpaa==input$mpaar,])))
        }
        else{
          return("There is no data.")
        }
      }
      else if(input$mpaar == 'All' & gen_row() != 'All'){
        if(nrow(localFrame[localFrame$genre %in% gen_row(),])!=0){
          return(summary(lm(rating ~ budget, data=localFrame[localFrame$genre %in% gen_row(),])))
        }
        else {
          return("There is no data.")
        }     
      }
      else {
        if(nrow(localFrame[localFrame$mpaa==input$mpaar & localFrame$genre %in% gen_row(),])!=0){
          return(summary(lm(rating ~ budget, data=localFrame[localFrame$mpaa==input$mpaar & localFrame$genre %in% gen_row(),])))
        }
        else{
          return("There is no data.")
        }     
      }      
    }
  )
  
  output$mod_quadratic_text <- renderPrint(
    {
      if(input$mpaar == 'All' & gen_row() == 'All'){
        return(summary(lm(rating ~ budget + I(budget^2),data=localFrame)))
      }
      else if(input$mpaar != 'All' & gen_row() == 'All') {
        if (nrow(localFrame[localFrame$mpaa==input$mpaar,])!=0){
          return(summary(lm(rating ~ budget + I(budget^2),data=localFrame[localFrame$mpaa==input$mpaar,])))
        }
        else {
          return("There is no data.")
        } 
      }
      else if(input$mpaar == 'All' & gen_row() != 'All'){
        if (nrow(localFrame[localFrame$genre %in% gen_row(),])!=0){
          return(summary(lm(rating ~ budget + I(budget^2),data=localFrame[localFrame$genre %in% gen_row(),])))
        }
        else {
          return("There is no data.")
        }
      }
      else {
        if (nrow(localFrame[localFrame$mpaa==input$mpaar & localFrame$genre %in% gen_row(),])!=0){
          return(summary(lm(rating ~ budget + I(budget^2), data=localFrame[localFrame$mpaa==input$mpaar & localFrame$genre %in% gen_row(),])))
        }
        else {
          return("There is no data.")
        }
      }
    }
  )
  
  output$scatterPlot <- renderPlot(
    {
      scatterPlot <- getPlot(
        localFrame,
        input$mpaar,
        gen_row(),
        input$mod_linear,
        input$mod_quadratic,
        input$alp,
        input$dsize,
        input$colorScheme
      )
      
      # Output the plot
      print(scatterPlot)
    }
  )  
  
})

# Two ways to run this application. Locally, use:
# runApp()

# To run this remotely, use:
# runGitHub("msan622", "cadancai", subdir = "homework2/")
