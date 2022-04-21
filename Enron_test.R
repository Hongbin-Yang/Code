#get mylines
mylines<-readLines("Enron5/9205.txt")
#nchararry
nchararray<-nchar(mylines)
#empty index
emptyindex<-which(nchararray==0)
#messagelines
messagelines<-mylines[emptyindex[1]+1:length(mylines)]
#message
messageoneline<-paste(messagelines, collapse=" ")
message<-gsub("\""," ",messageoneline)
message<-paste("\"",message,"\"",sep="")
#get "subject"
subject<-substring(mylines[5],9,nchararray[2])
subject<-gsub("\""," ",subject)
subject<-paste("\"",subject,"\"",sep="")
#Category
Category<-"Empolyee Arrangement"
#Output
csvline<-paste(Category,subject,message,sep=",")





