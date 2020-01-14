install.packages("cluster")
install.packages("NbClust")
install.packages("flexclust")
install.packages("fMultivar")
install.packages("rattle")

library("ggplot2")
library("cluster")
library("NbClust")
library("flexclust")
library("fMultivar")
library("rattle")
library("ggplot2")

data(nutrient, package="flexclust")
head(nutrient, 4)

d <- dist(nutrient)
as.matrix(d)[1:4,1:4]

data(nutrient, package="flexclust")
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)
d <- dist(nutrient.scaled)
fit.average <- hclust(d, method="average")

pdf(file = "test.pdf")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering") 
dev.off()

nc <- NbClust(nutrient.scaled, distance="euclidean",
		min.nc=2, max.nc=15, method="average")
table(nc$Best.n[1,])
pdf(file = "test.pdf")
barplot(table(nc$Best.n[1,]),
		xlab="Numer of Clusters", ylab="Number of Criteria",
		main="Number of Clusters Chosen by 26 Criteria")
dev.off()

clusters <- cutree(fit.average, k=5)
table(clusters)
aggregate(nutrient, by=list(cluster=clusters), median)
aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters),median)
pdf(file = "test.pdf")
plot(fit.average, hang=-1, cex=.8,
		main="Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average, k=5)
dev.off()

wssplot <- function(data, nc=15, seed=1234){
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

data(wine, package="rattle")
head(wine)
df <- scale(wine[-1])
pdf(file = "test1.pdf")
wssplot(df)
dev.off()
set.seed(1234)
devAskNewPage(ask=TRUE)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
pdf(file = "test2.pdf")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
dev.off()
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

pdf(file = "test.pdf")
set.seed(1234)
fit.pam <- pam(wine[-1], k=3, stand=TRUE)
fit.pam$medoids
clusplot(fit.pam, main="Bivariate Cluster Plot")
dev.off()
