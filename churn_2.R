library(dplyr) 
library(cluster)
library(Rtsne) 
library(ggplot2) 
clusterData=telecomDataframe[,c(1,6,9,16,20,19)]

gower_dist <- daisy(clusterData,
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 10)

#to give different cell clustering results
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE,perplexity = 70)
#x%>%f which translates to f(x)
  tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = clusterData[,1])
clus.plot=as.data.frame(tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


test.cluster1 <- subset(clus.plot, cluster==1)
test.cluster2 <- subset(clus.plot, cluster==2)
test.cluster3 <- subset(clus.plot, cluster==3)
test.cluster4 <- subset(clus.plot, cluster==4)
test.cluster5 <- subset(clus.plot, cluster==5)
test.cluster6 <- subset(clus.plot, cluster==6)
test.cluster7 <- subset(clus.plot, cluster==7)
test.cluster8 <- subset(clus.plot, cluster==8)
test.cluster9 <- subset(clus.plot, cluster==9)
test.cluster10 <- subset(clus.plot, cluster==10)
test.level1_cluster <- merge(test.cluster1, telecomDataframe, by.x = "name", by.y = "customerID")
test.level2_cluster <- merge(test.cluster2, telecomDataframe, by.x = "name", by.y = "customerID")
test.level3_cluster <- merge(test.cluster3, telecomDataframe, by.x = "name", by.y = "customerID")
test.level4_cluster <- merge(test.cluster4, telecomDataframe, by.x = "name", by.y = "customerID")
test.level5_cluster <- merge(test.cluster5, telecomDataframe, by.x = "name", by.y = "customerID")
test.level6_cluster <- merge(test.cluster6, telecomDataframe, by.x = "name", by.y = "customerID")
test.level7_cluster <- merge(test.cluster7, telecomDataframe, by.x = "name", by.y = "customerID")
test.level8_cluster <- merge(test.cluster8, telecomDataframe, by.x = "name", by.y = "customerID")
test.level9_cluster <- merge(test.cluster9, telecomDataframe, by.x = "name", by.y = "customerID")
test.level10_cluster <- merge(test.cluster10, telecomDataframe, by.x = "name", by.y = "customerID")


write.csv(test.level1_cluster,"test.cluster1.csv")
write.csv(test.level2_cluster,"test.cluster2.csv")
write.csv(test.level3_cluster,"test.cluster3.csv")
write.csv(test.level4_cluster,"test.cluster4.csv")
write.csv(test.level5_cluster,"test.cluster5.csv")
write.csv(test.level6_cluster,"test.cluster6.csv")
write.csv(test.level7_cluster,"test.cluster7.csv")
write.csv(test.level8_cluster,"test.cluster8.csv")
write.csv(test.level9_cluster,"test.cluster9.csv")
write.csv(test.level10_cluster,"test.cluster10.csv")

summary(test.level1_cluster)
summary(test.level2_cluster)
summary(test.level3_cluster)
summary(test.level4_cluster)
summary(test.level5_cluster)
summary(test.level6_cluster)
summary(test.level7_cluster)
summary(test.level8_cluster)
summary(test.level9_cluster)
summary(test.level10_cluster)

