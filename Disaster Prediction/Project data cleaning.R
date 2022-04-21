library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(readr)
library(syuzhet)
library(DataExplorer)

train<-read.csv("train.csv")
test<-read.csv("test.csv")
df <- bind_rows(train, test)

# sentiments
mytext <- df[,4]
mytext <- gsub("[^:alnum:]]"," ",mytext)
rbind(get_sentiment(mytext,method="syuzhet"),
      get_sentiment(mytext,method="bing"),
      get_sentiment(mytext,method="nrc"),
      get_sentiment(mytext,method="afinn"))
get_nrc_sentiment(mytext)
sent1 <- list(get_nrc_sentiment(mytext))
sent1 = as.data.frame(sent1)
colnames(sent1) <- c("text_anger","text_anticipation","text_disgust",
                     "text_fear","text_joy","text_sadness","text_surprise",
                     "text_trust","text_negative","text_positive")
mytext <- df[,3]
mytext <- gsub("[^:alnum:]]"," ",mytext)
rbind(get_sentiment(mytext,method="syuzhet"),
      get_sentiment(mytext,method="bing"),
      get_sentiment(mytext,method="nrc"),
      get_sentiment(mytext,method="afinn"))
get_nrc_sentiment(mytext)
sent2 <- list(get_nrc_sentiment(mytext))
sent2 = as.data.frame(sent2)
colnames(sent2) <- c("loc_anger","loc_anticipation","loc_disgust",
                     "loc_fear","loc_joy","loc_sadness","loc_surprise",
                     "loc_trust","loc_negative","loc_positive")
sent <- cbind(sent1,sent2)

#keyword
corpus_keyword <- Corpus(VectorSource(df$keyword))
corpus_keyword <- tm_map(corpus_keyword, tolower)
corpus_keyword <- tm_map(corpus_keyword, removePunctuation)
corpus_keyword <- tm_map(corpus_keyword, stemDocument)
dtm_keyword <- DocumentTermMatrix(corpus_keyword)
dtm_keyword
keyword <- as.data.frame(as.matrix(dtm_keyword))
colnames(keyword) <- make.names(colnames(keyword))
colnames(keyword) <- paste0(colnames(keyword), "_key")
#text
Corpus  <- VCorpus(VectorSource(df$text))
Corpus <- tm_map(Corpus, content_transformer(tolower))
Corpus <- tm_map(Corpus,removeNumbers)
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeWords, stopwords())
Corpus <- tm_map(Corpus, stemDocument)
Corpus <- tm_map(Corpus, stripWhitespace)
text <- DocumentTermMatrix(Corpus) 
text
text <- removeSparseTerms(text, 0.99)
text
text <- as.data.frame(as.matrix(text))
colnames(text) <- make.names(colnames(text))
colnames(text) <- paste0(colnames(text), "_text")

dtm <- cbind(keyword, text)
dtm$id <- as.factor(df$id )
dtm$target <- as.factor(df$target)






