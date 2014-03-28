

library(ggplot2)
library(plyr)
data(movies)
data(EuStockMarkets)

mvs <- movies[movies$budget>0 & is.na(movies$budget) == F,]
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


eu <- transform(data.frame(EuStockMarkets), time=time(EuStockMarkets))
eu$time <- as.numeric(eu$time)

#plot 1: scatter plot
pt1 <- ggplot(mvs,aes(x=budget,y=rating)) + 
          geom_point(alpha=0.25) +
          ggtitle("Scatterplot of Budget and Rating for Movies") +
          xlab('budget (10million)') +
          scale_x_continuous(labels=c(0,5,10,15,20)) +
          theme_bw()
#print(pt1)
ggsave("hw1-scatter.png",plot=pt1,width = 9, height = 4.25, dpi = 300, units = "in")


#plot 2: bar chart
mvs <- within(mvs,genre <- factor(genre,levels=names(sort(table(genre),decreasing=TRUE))))
pt2 <- ggplot(mvs,aes(x=genre)) +
          geom_bar(colour="black", fill="white",aes(order = desc(genre))) +
          ggtitle("Bar Chart of Genre for Movies") +
          theme_bw()
#print(pt2)
ggsave('hw1-bar.png',plot=pt2,width = 9, height = 4.25, dpi = 300, units = "in")



#plot 3: small multiples
pt3 <- ggplot(mvs,aes(x=budget,y=rating, color=genre)) +
          geom_point(alpha=0.25) +
          facet_wrap(~ genre, ncol = 3) +
          xlab('budget (10million)') +
          scale_x_continuous(labels=c(0,5,10,15,20)) +
          guides(color=FALSE) +
          ggtitle("Small Multiples Plot of Budget and Rating for Movies") +
          theme_bw()
#print(pt3)
ggsave('hw1-multiples.png',plot=pt3,width = 9, height = 4.25, dpi = 300, units = "in")


#plot 4: multi-line chart
eu_long <- reshape(eu,
                   varying = c("DAX","SMI","CAC","FTSE"),
                   v.names = "index",
                   timevar = 'type',
                   times = c("DAX","SMI","CAC","FTSE"),
                   direction = 'long')
pt4 <- ggplot(eu_long, aes(x=time,y=index,group=type,colour=type)) +
          geom_line() +
          scale_x_continuous(breaks=c(1992,1993,1994,1995,1996,1997,1998)) +
          ggtitle("Multi-line Chart for Four Stock Indice") +
          labs(color="Stock Indice") +
          theme_bw()
#print(pt4)
ggsave('hw1-multiline.png',plot=pt4,width = 9, height = 4.25, dpi = 300, units = "in")

