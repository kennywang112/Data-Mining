install.packages('cluster')
install.packages('NbClust')
install.packages('factoextra')

library(tidyverse)
data <- read_csv("C:/Users/USER/Desktop/wheat.csv")

data%>%summary()
data
data%>%group_by(category)%>%summarize(count = n())

data2 <- data%>%select(-category)

set.seed(1234)

# K-Means
kmeans.result <- kmeans(data2,  centers = 3)
table(data$category, kmeans.result$cluster)

agree <- kmeans.result$cluster == data$category
kmean_acc<-prop.table(table(agree))
kmean_acc[["TRUE"]]

kmeans.result

plot(data2, col = kmeans.result$cluster)
kmeansoutput<-cbind(data, cluster = kmeans.result$cluster)
# View(kmeansoutput)
kmeansoutput%>%group_by(category, cluster)%>%summarize(count = n())

# K-Medoids
# Silhouette width（輪廓寬度）是輪廓係數中的一個重要部分，它用於評估聚類結果中單個樣本的分離程度
# https://sites.ualberta.ca/~ahamann/teaching/graphics/LabPAM.pdf
library(cluster)
pam.result <- pam(data2, k = 3)
pam.result
table(data$category, pam.result$clustering)
layout(matrix(c(1,2),1,2))
plot(pam.result)
Medoidsoutput <- cbind(data, cluster = pam.result$cluster)
# View(Medoidsoutput)
kmedoids <- (57 + 60 + 70)/ 210
kmedoids

# K的數量
library(NbClust)
result_kmeans <- NbClust(data2, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
# table(data$category, result$Best.partition)
result_kmeans$Best.partition

# 手肘法: 確定K-Means聚類算法中最適合的聚類數量
# 應用在 K-Means
library(factoextra)
data%>%
  fviz_nbclust(FUNcluster = kmeans, method = "wss", k.max = 12)+
  geom_vline(xintercept = 3, linetype = 2)+
  labs(title="Elbow Method for K-Means")

# 應用在 K-Medoid
data2%>%
  fviz_nbclust(FUNcluster = pam, method = "wss", k.max = 12)+
  geom_vline(xintercept = 3, linetype = 2)+
  labs(title="Elbow Method for K-Medoid")

# 輪廓係數: 確定K值的適當數量
# 應用在 K-Means
data%>%
  fviz_nbclust(FUNcluster = kmeans, method = "silhouette", k.max = 6)+
  labs(title="Avg.Silhouette Method for K-Means")

data%>%
  fviz_nbclust(FUNcluster = pam, method = "silhouette", k.max = 6)+
  labs(title="Avg.Silhouette Method for K-Medoid")


# 階層式集群Hierarchical
index <- sample(1:nrow(data2), 210)
irissample <- data2[index,]
hclust.result <- hclust(dist(irissample), method = 'ward.D2') 
hclust.result
layout(matrix(1))
plot(hclust.result, labels = data$category[index])
rect.hclust(hclust.result, k = 3, border="red")
groups <- cutree(hclust.result, k = 3)
table(data$category[index], groups)

hierarchical <- (54 + 63 + 70)/ 210
hierarchical

# 密度基礎集群dbscan
library(fpc)
dbscan.result <- dbscan(data2, eps = 0.8)
dbscan.result
table(data$category, dbscan.result$cluster)
plot(dbscan.result, data2)
# plot(dbscan.result, data2[c(1,4)])
plotcluster(data2, dbscan.result$cluster)
dbscan.result$cluster
dbscanoutput <- cbind(data, cluster = dbscan.result$cluster) 
dbscanoutput
dbscanoutput$cluster%>%unique()

# kmeans的outlier
kmeans.result$centers
centers <- kmeans.result$centers[kmeans.result$cluster,]
centers%>%head()
distances_kmeans <- sqrt(rowSums((data2 - centers)^2))
outliers_kmeans <- order(distances_kmeans, decreasing = T)[1:5]
outliers_kmeans
data2[outliers,]
data%>%colnames()
# plot(data2[c("area", "asymmetry coefficient")], col = kmeans.result$cluster)
# points(kmeans.result$centers[,c("area", "compactness")], col=1:3, pch=4, cex=2)
# points(data[outliers,c("area", "asymmetry coefficient")], col=4, pch='+', cex=2)

# 在每個群的asymmetry coefficient最高點基本上都是outlier
data2%>%ggplot(aes(area, `asymmetry coefficient`))+
  geom_point(aes(color = factor(kmeans.result$cluster)))+
  geom_point(data = data2[outliers_kmeans,], aes(color = "outlier"), size = 3)

# kmedoids的outlier
pam.result$medoids
medoids <- pam.result$medoids[pam.result$clustering,]
medoids%>%head()
distances_medoids <- sqrt(rowSums((data2 - medoids)^2))
outliers_medoids <- order(distances_medoids, decreasing = T)[1:5]
outliers_medoids
data2[outliers,]
data%>%colnames()
data2%>%ggplot(aes(area, `asymmetry coefficient`))+
  geom_point(aes(color = factor(pam.result$clustering)))+
  geom_point(data = data2[outliers_medoids,], aes(color = "outlier"), size = 3)

# kmean_acc[["TRUE"]]、kmedoids、hierarchical的準確率
# accuracy_values <- c(kmean_acc[["TRUE"]], kmedoids, hierarchical)
# methods <- c("kmean_acc", "kmedoids", "hierarchical")
# data <- data.frame(Method = methods, Accuracy = accuracy_values)
# ggplot(data, aes(x = Method, y = Accuracy, fill = Method)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Comparison of Accuracy",
#        x = "Methods",
#        y = "Accuracy") +
#   theme_minimal()
kmean_acc[["TRUE"]]
kmedoids
hierarchical
