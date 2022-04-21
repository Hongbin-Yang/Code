library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
x<-NCI60$data
pr.out<- prcomp(x, scale=TRUE)
par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z2")

plot(pr.out$x[,c(1,3) ], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z3")

pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
     col =" blue")

plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")



library(dplyr)
x1<-scale(nci.data)
mean(x1)
sd(x1)
set.seed(1)
kmeans_k4<- kmeans(x1,4,nstart = 20)
plot(x1, col =(kmeans_k4$cluster +1) , main="K-Means Clustering
Results with K=4", xlab ="", ylab="", pch =20, cex =1)
kmeans_k4$size
100*kmeans_k4$betweenss/kmeans_k4$totss

xsc=scale(x)
set.seed(1)
kmeans_k4<- kmeans(x,4,nstart = 20)
kmeans_k4$size
100*kmeans_k4$betweenss/kmeans_k4$totss
