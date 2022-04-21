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

index_train<-sample(1:nrow(dtm), round(nrow(dtm)*0.8))
index_test<-setdiff(1:nrow(dtm), index_train)
train<-dtm[index_train,]
test<-dtm[-index_train,]
train_label<-labels[index_train]
test_label<-labels[-index_train]
pred_label_knn<-knn(train, test, train_label, k = 5)
contable<-table(pred_label_knn,test_label)
