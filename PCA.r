library(cluster)
library(factoextra)
View(wine)
help("princomp")
# Droping first column in wine data
View(wine[-1])
#wine[-1] -> Considering only numerical values for applying PCA
data<- wine[-1]
attach(data)
cor(data)
# cor= TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<- princomp(data, cor=TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
# princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
# graph showing importance of principal components 
plot(pcaObj)
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

# Top 3 PCA Scores which represents the whole data
pcaObj$scores[,1:3]

# cbind used to bind the datain column wise
# Considering top 3 principal component scores and binding them with mydata
wine<-cbind(wine,pcaObj$scores[,1:3])
View(wine)

# Hierarchial Clustering
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-wine[,8:10]

# Normalizing the data
# Scale function is used to normalize data
norm_clus<-scale(clus_data)
# method for finding the distance where I am considering Euclidean distance
dist1<-dist(norm_clus,method = "euclidean")

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete")

# Displaying Dendrogram
plot(fit1)
rect.hclust(fit1, k=7, border="red")

# Cutting the dendogram for 7 clusters
groups<- cutree(fit1,7)

# Cluster numbering
membership_1<- as.matrix(groups)

# Binding column wise with original data
final1<- cbind(membership_1, wine)

View(final1)

View(aggregate(final1[,-c(2,16:18)], by=list(membership_1), FUN = mean))

# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)

getwd()

# K-Means Clustering :
library(plyr)

wine<- read.csv("C:/Users/Binita Mandal/Documents/wine_cluster.csv")
str(wine)

View(wine)

normalized_data<-scale(wine[,15:17])

# Determine number of clusters by scree-plot 
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))

for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,wine) # append cluster membership
View(final2)
aggregate(wine[,2:17], by=list(fit$cluster), FUN=mean)

table(fit$cluster)
