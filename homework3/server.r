
library(shiny)
library(ggplot2)
library(ggmap)
library(maps)
library(maptools)
library(grid)
library(GGally)
library(plyr)
library(gridExtra)

df <- data.frame(state.x77,
                 State = state.name,
                 Abbrev = state.abb,
                 Region = state.region,
                 Division = state.division
)
df$region <- mapvalues(df$Region, from = levels(df$Region), to = c('NE','S','NC','W'))
df$state <- tolower(df$State)
df <- df[!(df$State %in% c('Alaska','Hawaii')),]

getbubble <- function(df,xvar,yvar,sizevar,colvar,states,regions){
  localframe <- df[,c(xvar,yvar,sizevar,colvar)]
  names(localframe) <- c('xvar','yvar','sizevar','colvar')
  if(length(states)!=0){
    if(length(regions)==0){
      localframe1 <- localframe[!(df$Abbrev %in% states),]
      localframe2 <- localframe[df$Abbrev %in% states,]
      bubble <- ggplot(localframe1,aes(x=xvar,y=yvar,size=sizevar)) + 
        geom_point(alpha=0.2,color='grey') +
        geom_point(data=localframe2,aes(x=xvar,y=yvar,size=sizevar,color=colvar))
    }
    else{
      localframe1 <- localframe[!(df$Abbrev %in% states & df$Region %in% regions),]
      localframe2 <- localframe[df$Abbrev %in% states & df$Region %in% regions,]
      if(nrow(localframe2)==0){
        bubble <- ggplot(localframe1,aes(x=xvar,y=yvar,size=sizevar)) + 
          geom_point(alpha=0.2,color='grey')
      }
      else{
        if(nrow(localframe1)==0){
          bubble <- ggplot(localframe2,aes(x=xvar,y=yvar,size=sizevar,color=colvar)) +
                    geom_point(alpha=0.6)
        }
        else{
          bubble <- ggplot(localframe1,aes(x=xvar,y=yvar,size=sizevar)) + 
            geom_point(alpha=0.2,color='grey') +
            geom_point(data=localframe2,aes(x=xvar,y=yvar,size=sizevar,color=colvar))
        }
      }

    }
    
  }
  else{
    if(length(regions)==0 | length(regions)==4){
      bubble <- ggplot(localframe,aes(x=xvar,y=yvar,size=sizevar,color=colvar)) +
        geom_point(alpha=0.6, position='jitter')
    }
    else{
      localframe1 <- localframe[-which(df$Region %in% regions),]
      localframe2 <- localframe[which(df$Region %in% regions),]
      bubble <- ggplot(localframe1,aes(x=xvar,y=yvar,size=sizevar)) + 
        geom_point(alpha=0.2,color='grey') +
        geom_point(data=localframe2,aes(x=xvar,y=yvar,size=sizevar,color=colvar))
    }
  }
  
  bubble <- bubble +
    scale_size_area(max_size = 20, guide='none') +
    #coord_fixed(xlim=c(min(df$Income)*0.95,max(df$Income)*1.01),
    #           ylim=c(min(df$Illiteracy)*0.7,max(df$Illiteracy*1.1)),
    #            ratio=800) +
    ggtitle(paste(xvar,' vs ',yvar,', Size = ',sizevar)) +
    theme(legend.direction='horizontal') +
    #theme(legend.position='bottom') +
    theme(legend.background=element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.margin = unit(0, "pt")) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    #theme(legend.text =element_text(angle=45)) +
    theme(legend.text.align=0.2) +
    theme(legend.direction='vertical') +
    guides(colour = guide_legend(override.aes = list(size = 8))) +
    scale_color_brewer(palette='Dark2') +
    xlab(xvar) +
    ylab(yvar) +
    labs(color=colvar)

  
  return(bubble)
}
#getbubble(df,'Income','Illiteracy','Population','Region',c('CA','AL','CO','CT'))
  


getScatterMatrix <- function(df,cols,colvar,all,rhigh,chigh){
  sm <- ggpairs(data=df,
                columns=cols,
                upper=list(continuous='cor'),
                lower=list(continuous='points'),
                diag=list(continuous='density'),
                axisLabels='none',
                colour=colvar,
                title='US States Data',
                params=list(corSize=3)
  )
  for(i in 1:length(cols)){
    for(j in i:length(cols)){
      inner <- getPlot(sm,i,j)
      inner <- inner + #theme(axis.ticks=element_blank()) +
        theme(panel.grid=element_blank()) +
        scale_color_brewer(palette='Dark2')
      sm <- putPlot(sm, inner, i, j) 
    }
  }
  for(i in 2:length(cols)){
    for(j in 1:i-1){
      low <- getPlot(sm,i,j)
      low <- low +
              theme(panel.grid.minor=element_blank()) +
              scale_color_brewer(palette='Dark2')
      sm <- putPlot(sm,low,i,j)
    }
  }
  if(all=='Highlight'){
    i <- as.numeric(rhigh)
    j <- as.numeric(chigh)
    if(i==j){
      hl_dense <- getPlot(sm,i,j)
      return(hl_dense)
    }
    else{
      hl_dense1 <- getPlot(sm,i,i) +
                      theme(axis.ticks=element_blank()) +
                      theme(legend.position=c(1,1),legend.justification=c(1,1),
                            legend.direction='horizontal', legend.background=element_blank()) +
                      theme(axis.title.y=element_blank()) +
                      theme(axis.text=element_blank()) +
                      theme(panel.grid = element_blank())
      hl_dense2 <- getPlot(sm,j,j) +
                  theme(axis.ticks=element_blank()) +
                  theme(legend.position=c(1,1),legend.justification=c(1,1),
                        legend.direction='horizontal', legend.background=element_blank()) +
                  theme(axis.title.y=element_blank()) +
                  theme(axis.text=element_blank()) +
                  theme(panel.grid = element_blank())
      if(j>i){
        hl3 <- getPlot(sm,i,j) + theme(axis.text=element_blank()) + 
          theme(axis.ticks=element_blank())
        hl4 <- getPlot(sm,j,i) +
          theme(legend.position=c(1,0),legend.justification=c(1,0),
                legend.direction='horizontal', legend.background=element_blank()) +
          theme(axis.ticks=element_blank())
      }
      else{
        hl3 <- getPlot(sm,j,i) + theme(axis.text=element_blank()) + 
          theme(axis.ticks=element_blank())
        hl4 <- getPlot(sm,i,j) +
          theme(legend.position=c(1,0),legend.justification=c(1,0),
                legend.direction='horizontal', legend.background=element_blank()) +
          theme(axis.ticks=element_blank())
      }
      
      hl_sm <- grid.arrange(hl_dense1,hl_dense2,hl3,hl4)
      return(hl_sm)
    }
  }
  return(sm)
}
#getScatterMatrix(df,cols=1:6,'region')


getParCoord <- function(df,cols,grpcol,brushvar,range,states){
  localframe <- df
  
  localframe$brushvar <- df[,as.numeric(brushvar)]
  localframe$hide_ind <- ifelse(localframe$brushvar>=range[1] & localframe$brushvar<=range[2],1,0)
  localframe$grpcol <- df[,grpcol]
  
  colnames <- names(df[,as.numeric(cols)])
  meltdt <- melt(localframe,id.vars=c('hide_ind','Abbrev','grpcol','State'),measure.vars=colnames) 
  
  meltdt2 <- ddply(meltdt,.(variable),transform,value2=(value-min(value))/(max(value)-min(value)))
  
  localframe1 <- meltdt2[meltdt2$hide_ind==1 ,]
  localframe2 <- meltdt2[meltdt2$hide_ind==0 ,]
  
  if(length(states)==0){
    if(nrow(localframe1)>0){
      pc <- ggplot(localframe1,aes(x=variable,y=value2,colour=grpcol,group=Abbrev))+
        geom_line(alpha=0.6) +
        scale_color_brewer(palette='Dark2')
      if(nrow(localframe2)>0){
        pc <- pc + geom_line(data=localframe2,aes(x=variable,y=value2,group=Abbrev),colour='grey',alpha=0.2)
      }
    }else{
      pc <- ggplot(localframe2,aes(x=variable,y=value2,group=Abbrev)) + geom_line(colour='grey',apha=0.3)
    }
  }else{
    localframe1 <- localframe1[localframe1$Abbrev %in% states,]
    localframe2 <- meltdt2[meltdt2$hide_ind==0 | !(meltdt2$Abbrev %in% states) ,]
    if(nrow(localframe1)>0){
      pc <- ggplot(localframe1,aes(x=variable,y=value2,colour=grpcol,group=Abbrev))+
        geom_line(alpha=0.6) +
        scale_color_brewer(palette='Dark2')
      if(nrow(localframe2)>0){
        pc <- pc + geom_line(data=localframe2,aes(x=variable,y=value2,group=Abbrev),colour='grey',alpha=0.2)
      }
    }else{
      pc <- ggplot(localframe2,aes(x=variable,y=value2,group=Abbrev)) + geom_line(colour='grey',apha=0.3)
    }
  }
  
  if(nrow(localframe1)>0){
    pc <- ggplot(localframe1,aes(x=variable,y=value2,colour=grpcol,group=Abbrev))+
      geom_line(alpha=0.6) +
      scale_color_brewer(palette='Dark2')
    if(nrow(localframe2)>0){
      pc <- pc + geom_line(data=localframe2,aes(x=variable,y=value2,group=Abbrev),colour='grey',alpha=0.2)
    }
  }else{
    pc <- ggplot(localframe2,aes(x=variable,y=value2,group=Abbrev)) + geom_line(colour='grey',apha=0.3)
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
    labs(color=grpcol)
  #Figure out y-axis range after GGally scales the data
  min_y <- min(pc$data$value2)
  max_y <- max(pc$data$value2)
  pad_y <- (max_y - min_y) * 0.1
  # Calculate label positions for each veritcal bar
  lab_x <- rep(1:length(colnames), times = 2) # 2 times, 1 for min 1 for max
  lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = length(colnames))  
  # Get min and max values from original dataset
  lab_z <- c(sapply(df[, colnames], min), 
             sapply(df[, colnames], max))
  lab_z <- as.character(lab_z)
  pc <- pc + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
  return(pc)
}

#getParCoord(df,cols=1:5,grpcol='Region',brushvar='5',range=c(11,12))


#getHeat(df,'Murder','blue','red',c('GA'),c('West'))

getHeat <- function(df,fillcol,low_col,high_col,states,regions){
  all_states <- map_data("state")
  all_states <- all_states[all_states$region!="district of columbia",]
  colnames(all_states)[5] <- 'state'
  allstates <- merge(all_states,df,by='state')
  localframe <- allstates[,c('long','lat','group',fillcol,'Abbrev')]
  names(localframe)[4] <- 'fillcol'
  if(length(states)!=0){
    if(length(regions)==0){
      localframe1 <- localframe[!(allstates$Abbrev %in% states),]
      localframe2 <- localframe[allstates$Abbrev %in% states,]
      p <- ggplot(localframe1,aes(x=long,y=lat,group=group),fill='grey') +
        geom_polygon(colour='black',alpha=0.2) +
        geom_polygon(data=localframe2,aes(x=long,y=lat,group=group,fill=fillcol),colour='black') +
        scale_fill_gradient2(low=low_col,mid='white',high=high_col,midpoint=(min(localframe$fillcol)+max(localframe$fillcol))/2)
    }else{
      localframe1 <- localframe[!(allstates$Abbrev %in% states & allstates$Region %in% regions),]
      localframe2 <- localframe[allstates$Abbrev %in% states & allstates$Region %in% regions,]
      if(nrow(localframe2)==0){
        p <- ggplot(localframe1,aes(x=long,y=lat,group=group),fill='grey') +
                geom_polygon(colour='black',alpha=0.2)
      }else{
        if(nrow(localframe1)==0){
          p <- ggplot(localframe2,aes(x=long,y=lat,group=group,fill=fillcol),color='black') +
                  geom_polygon() +
              scale_fill_gradient2(low=low_col,mid='white',high=high_col,midpoint=(min(localframe$fillcol)+max(localframe$fillcol))/2)
        }else{
          p <- ggplot(localframe1,aes(x=long,y=lat,group=group),fill='grey') +
            geom_polygon(colour='black',alpha=0.2) +
            geom_polygon(data=localframe2,aes(x=long,y=lat,group=group,fill=fillcol),colour='black') +
            scale_fill_gradient2(low=low_col,mid='white',high=high_col,midpoint=(min(localframe$fillcol)+max(localframe$fillcol))/2)
        }
      }   
    }
  }else{
    if(length(regions)==0 | length(regions)==4){
      p <- ggplot(data=localframe, aes(x=long, y=lat, group = group,fill=fillcol)) +
        geom_polygon(colour="black") +
        scale_fill_gradient2(low=low_col,mid='white',high=high_col,midpoint=(min(localframe$fillcol)+max(localframe$fillcol))/2)
    }else{
      localframe1 <- localframe[!(allstates$Region %in% regions),]
      localframe2 <- localframe[allstates$Region %in% regions,]
      p <- ggplot(localframe1,aes(x=long,y=lat,group=group),fill='grey') +
        geom_polygon(colour='black',alpha=0.2) +
        geom_polygon(data=localframe2,aes(x=long,y=lat,group=group,fill=fillcol),colour='black') +
        scale_fill_gradient2(low=low_col,mid='white',high=high_col,midpoint=(min(localframe$fillcol)+max(localframe$fillcol))/2)
    }
  }

  p <- p + theme_minimal()
  p <- p + theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(axis.title=element_blank()) +
    theme(legend.position='bottom') +
    labs(fill=fillcol,title=paste('Geo-spatial Distribution of',fillcol))
  return(p)
}

#getHeat(df,'Murder','blue','red',c('CA'))

shinyServer(function(input,output){
  
  lcframe <- df
  
  brushvarRange<- reactive({
    brushcol <- as.numeric(input$brushvar)
    min_val <- min(df[,brushcol])
    max_val <- max(df[,brushcol])
    return(c(min_val,max_val))
  })
  
  output$sliderUI <- renderUI({ 
    sliderInput("brush", "Brush", min=brushvarRange()[1],max=brushvarRange()[2],
                value=c(brushvarRange()[1],brushvarRange()[2]),
                format = "0.00",
                ticks = T )
  })
  
  output$heatmap <- renderPlot({
    print(getHeat(df,input$fill,input$low,input$high,input$state,input$region))
  })
  
  output$parcoor <- renderPlot({
    print(getParCoord(df,cols=input$columns,grpcol=input$grpcolumn,
                      brushvar=input$brushvar,input$brush,
                      input$state))
  })

  output$scatterMatrix <- renderPlot({
    print(getScatterMatrix(df,cols=1:6,'region',input$all,input$highlight_row,input$highlight_col))
  })
  
  output$bubble <- renderPlot({
    print(getbubble(df,input$xvar,input$yvar,input$sizevar,input$colvar,
                    input$state,input$region))
  })
  
})

