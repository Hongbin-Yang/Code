library(RWeka)
library(tm)
library("class")
mystopwords<-c(stopwords(),"just","get","will","can","also","take","subject","re","am","pm","message","sent","origin
al","from","forwarded")
mystopwords<-setdiff(mystopwords,c("not","cant","cannot"))
mydf<-read.csv("Enron56.csv",stringsAsFactors = FALSE)
labels <- mydf$Category
docs<-VCorpus(VectorSource(mydf$Message))
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeWords,mystopwords)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
ctrl<-list(bounds=list(global=c(1,length(docs)-1)),
           wordLengths=c(3,15),
           tokenize = UnigramTokenizer,
           weighting=weightTfIdf
)
dtm<-DocumentTermMatrix(docs,control=ctrl)
