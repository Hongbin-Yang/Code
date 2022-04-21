output_file<-file("Enron56.csv","wt")
writeLines(paste("Category","Subject","Message",sep=","),
           output_file)
filenames5<-list.files("Enron5",full.names = TRUE)
filenames6<-list.files("Enron6",full.names = TRUE)
for (name in filenames5){
  mylines<-readLines(name)
  nchararray<-nchar(mylines)
  emptyindex<-which(nchararray==0)
  messagelines<-mylines[emptyindex[1]+1:length(mylines)]
  messageoneline<-paste(messagelines, collapse=" ")
  message<-gsub("\""," ",messageoneline)
  message<-paste("\"",message,"\"",sep="")
  subject<-substring(mylines[5],9,nchararray[2])
  subject<-gsub("\""," ",subject)
  subject<-paste("\"",subject,"\"",sep="")
  Category<-"Employment arrangements"
  csvline<-paste(Category,subject,message,sep=",")
  writeLines(csvline,output_file)
}
for (name in filenames6){
  mylines<-readLines(name)
  nchararray<-nchar(mylines)
  emptyindex<-which(nchararray==0)
  messagelines<-mylines[emptyindex[1]+1:length(mylines)]
  messageoneline<-paste(messagelines, collapse=" ")
  message<-gsub("\""," ",messageoneline)
  message<-paste("\"",message,"\"",sep="")
  subject<-substring(mylines[5],9,nchararray[2])
  subject<-gsub("\""," ",subject)
  subject<-paste("\"",subject,"\"",sep="")
  Category<-"Documentediting/checking"
  csvline<-paste(Category,subject,message,sep=",")
  writeLines(csvline,output_file)
}
close(output_file)