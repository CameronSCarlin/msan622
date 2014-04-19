
library(tm)
library(SnowballC)
library(shiny)
library(ggplot2)
library(wordcloud)

#There 9 novels/drama/fictions included
#Title: Adventures of Huckleberry Finn, Complete; Author: Mark Twain
#Title: Daisy Miller; Author: Henry James
#Title: The Tragedie of Hamlet; Author: William Shakespeare
#Title: Life On The Mississippi, Complete; Author: Mark Twain
#Title: The Tragedie of Macbeth; Author: William Shakespeare
#Title: A Midsummer Night's Dream; Author: William Shakespeare
#Title: Romeo and Juliet; Author: William Shakespeare
#Title: The Taming of the Shrew; Author: William Shakespeare
#Title: The Turn of the Screw; Author: Henry James

#load data for bargraph and word cloud
loadData <- function(){
  novels <- DirSource(directory = file.path("/Users/lihuacai/Desktop/MSAN/MSAN_622/hw4/texts"),
                      encoding = "UTF-8",pattern = "*.txt",recursive = FALSE,ignore.case = FALSE)    
  
  #from the source, create the corpus
  novels_corpus <- Corpus(novels,readerControl = list(reader = readPlain,
                                                      language = "en"))
  
  for (j in seq(novels_corpus)) {
    novels_corpus[[j]] <- gsub("\u0097", " ", novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\n"," ",novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\r"," ",novels_corpus[[j]])
  }
  
  #transform the corpus
  novels_corpus <- tm_map(novels_corpus, tolower)
  novels_corpus <- tm_map(novels_corpus,removePunctuation,
                          preserve_intra_word_dashes = TRUE)
  novels_corpus <- tm_map(novels_corpus,removeNumbers)
  novels_corpus <- tm_map(novels_corpus,removeWords,stopwords("english"))
  #novels_corpus <- tm_map(novels_corpus,stemDocument,lang = "porter")
  novels_corpus <- tm_map(novels_corpus,removeWords,
                          c("will", "can", "get", "that", "year", "let",
                            'gutenberg-tm'))
  novels_corpus <- tm_map(novels_corpus,stripWhitespace)
  
  #get the term document matrix
  novels_tdm <- TermDocumentMatrix(novels_corpus)
  novels_matrix <- as.matrix(novels_tdm)
  
  #convert the matrix into data frame
  novels_df <- data.frame(word = rownames(novels_matrix),novels_matrix,
                          freq = rowSums(novels_matrix),stringsAsFactors = FALSE) 
  novels_df <- novels_df[with(novels_df,order(freq, decreasing = TRUE)), ]
  rownames(novels_df) <- NULL
  colnames(novels_df) <- c('word','AOHF','DM','HT','LOTM','MB','MSN','RJ','TS','TTOTS','TotFreq')
  
  return(novels_df)
}
novels <- loadData()

meltdt <- melt(novels,id=c('word'))
meltdt$author <- 'Shakespeare'
meltdt$author[meltdt$variable %in% c('AOHF','LOTM')] <- "Mark Twain"
meltdt$author[meltdt$variable %in% c('DM','TTOTS')] <- "Henry James"
meltdt$author[meltdt$variable == "TotFreq"] <- "All"

meltdt <- meltdt[order(meltdt$author,-meltdt$value),]
meltdt1 <- ddply(meltdt,.(author),transform,totwords = sum(value),per = value/sum(value))
meltdt2 <- ddply(meltdt1,.(author,word),summarize,value=sum(value),per=sum(per))
meltdt2 <- meltdt2[order(meltdt2$author,-meltdt2$value),]
top10words <- rbind(head(meltdt2[meltdt2$author=='Henry James',],n=10),
      head(meltdt2[meltdt2$author=='Mark Twain',],n=10),
      head(meltdt2[meltdt2$author=='Shakespeare',],n=10))
top10words$word <- factor(top10words$word)

bargraph <- ggplot(top10words, aes(x = word, y = per,fill=author)) +
  geom_bar(stat = "identity",color='dark blue') +
  ggtitle("Top 10 Words in Selected Novels of\nMark Twain, Henry James, and Shakespeare") +
  xlab("Top 10 Word (Stop Words Removed)") +
  ylab("Percentage") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),labels=percent) +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  facet_wrap(~author,nrow=3,scales="free_x") +
  scale_fill_brewer(palette='Dark2') +
  theme(legend.position='none')
print(bargraph)

wordcloud(
  novels$word,
  novels$TotFreq,
  scale = c(0.5, 3),      
  min.freq = 10,          
  max.words = 100,         
  random.order = FALSE,   
  rot.per = 0.3,          
  colors = brewer.pal(8,'Dark2'),
  random.color = TRUE,
  use.r.layout = FALSE    
)


#load data for comparison plot
loadData3 <- function(){
  novels <- DirSource(directory = file.path("/Users/lihuacai/Desktop/MSAN/MSAN_622/hw4/texts"),
                      encoding = "UTF-8",pattern = "*.txt",recursive = FALSE,ignore.case = FALSE)    
  
  #from the source, create the corpus
  novels_corpus <- Corpus(novels,readerControl = list(reader = readPlain,
                                                      language = "en"))
  
  for (j in seq(novels_corpus)) {
    novels_corpus[[j]] <- gsub("\u0097", " ", novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\n"," ",novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\r"," ",novels_corpus[[j]])
  }
  
  #transform the corpus
  novels_corpus <- tm_map(novels_corpus, tolower)
  novels_corpus <- tm_map(novels_corpus,removePunctuation,
                          preserve_intra_word_dashes = TRUE)
  novels_corpus <- tm_map(novels_corpus,removeNumbers)
  novels_corpus <- tm_map(novels_corpus,removeWords,stopwords("english"))
  #novels_corpus <- tm_map(novels_corpus,stemDocument,lang = "porter")
  novels_corpus <- tm_map(novels_corpus,removeWords,
                          c("will", "can", "get", "that", "year", "let",
                            'gutenberg-tm'))
  novels_corpus <- tm_map(novels_corpus,stripWhitespace)
  
  #get the term document matrix
  novels_tdm <- TermDocumentMatrix(novels_corpus)
  novels_matrix <- as.matrix(novels_tdm)
  
  return(novels_matrix)
}
compclouddt <- loadData3()
colnames(compclouddt) <- c("Adventures of Huckleberry Finn","Daisy Miller","The Tragedie of Hamlet",
                           "Life On The Mississippi","The Tragedie of Macbeth",
                           "A Midsummer Night's Dream","Romeo and Juliet",
                           "The Taming of the Shrew","The Turn of the Screw")
comparison.cloud(compclouddt[,1:2],max.words=100,random.order=FALSE,title.size=1)





#load data for the lexical plot
loadData2 <- function(){ 
  nvls <- DirSource(directory = file.path("texts"),
                      encoding = "UTF-8",pattern = "*.txt",recursive = FALSE,ignore.case = FALSE)    
  
  #from the source, create the corpus
  novels_corpus <- Corpus(nvls,readerControl = list(reader = readPlain,
                                                      language = "en"))
  
  for (j in seq(novels_corpus)) {
    novels_corpus[[j]] <- gsub("\u0097", " ", novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\n"," ",novels_corpus[[j]])
    novels_corpus[[j]] <- gsub("\r"," ",novels_corpus[[j]])
  }
   
  #transform the corpus
  novels_corpus <- tm_map(novels_corpus, tolower)
  novels_corpus <- tm_map(novels_corpus,removePunctuation,
                          preserve_intra_word_dashes = TRUE)
  novels_corpus <- tm_map(novels_corpus,removeNumbers)
  novels_corpus <- tm_map(novels_corpus,removeWords,stopwords("english"))
  
  words <- c('money','love','death','friends','free','girl')
  
  nvl1 <- MC_tokenizer(novels_corpus[[1]])
  nvl2 <- MC_tokenizer(novels_corpus[[2]])
  nvl3 <- MC_tokenizer(novels_corpus[[3]])  
  nvl4 <- MC_tokenizer(novels_corpus[[4]])
  nvl5 <- MC_tokenizer(novels_corpus[[5]])
  nvl6 <- MC_tokenizer(novels_corpus[[6]]) 
  nvl7 <- MC_tokenizer(novels_corpus[[7]])
  nvl8 <- MC_tokenizer(novels_corpus[[8]])
  nvl9 <- MC_tokenizer(novels_corpus[[9]])
  
  nvl1 <- nvl1[nvl1!=""]
  nvl2 <- nvl2[nvl2!=""]
  nvl3 <- nvl3[nvl3!=""]
  nvl4 <- nvl4[nvl4!=""]
  nvl5 <- nvl5[nvl5!=""]
  nvl6 <- nvl6[nvl6!=""]
  nvl7 <- nvl7[nvl7!=""]
  nvl8 <- nvl8[nvl8!=""]
  nvl9 <- nvl9[nvl9!=""]
  
  offsetdt <- data.frame()
  for(nvl in paste('nvl',1:9,sep='')){
    noveltmp <- eval(as.name(nvl))
    for(wd in words){
      if(length(which(noveltmp==wd))!=0){
        tmp <- cbind(wd,which(noveltmp==wd),nvl)
        offsetdt <- rbind(offsetdt,tmp)
      }     
    }
  }
  
  colnames(offsetdt) <- c('word','offset','novel')
  offsetdt$author <- 'Shakespeare'
  offsetdt$author[offsetdt$novel %in% c('nvl1','nvl4')] <- 'Mark Twain'
  offsetdt$author[offsetdt$novel %in% c('nvl2','nvl9')] <- 'Henry James'
  offsetdt$offset <- as.numeric(as.character(offsetdt$offset))
  
  return(offsetdt)
}

offsetdt <- loadData2()
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl1"] <- "Adventures of Huckleberry Finn"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl2"] <- "Daisy Miller"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl3"] <- "The Tragedie of Hamlet"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl4"] <- "Life On The Mississippi"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl5"] <- "The Tragedie of Macbeth"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl6"] <- "A Midsummer Night's Dream"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl7"] <- "Romeo and Juliet"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl8"] <- "The Taming of the Shrew"
levels(offsetdt$novel)[levels(offsetdt$novel)=="nvl9"] <- "The Turn of the Screw"
lexicalplot <- ggplot(offsetdt,aes(x=offset,y=word,color=author)) + 
                  geom_point(shape='|') +
                  facet_wrap(~novel,nrow=3,scales="free_x") +
                  theme(panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank()) +
                  theme(axis.ticks=element_blank()) +
                  ggtitle('Lexical Dispersion Plot for Nine Novels') +
                  theme(legend.position='bottom') +
                  scale_color_brewer(palette='Dark2')
print(lexicalplot)
