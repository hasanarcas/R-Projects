library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(datasets)
library(stats)
options(digits = 10)

ggplot(iris, mapping = aes(x = iris$Sepal.Width, y = iris$Sepal.Length, color = iris$Species))+
  geom_point() +
  labs(title = "Scatter Plot of Sepal Width Vs Sepal Length",
       subtitle = "Represent each point color according to  species' color",
       x = "Sepal Width",
       y = "Sepal Length")
  
iris_X <- iris[1:4]
iris_y <- iris[5]

pca <- prcomp(iris_X, scale= T, center = T)     # mantıksız oldugu icin iris datasetten devam ediyoruz

fviz_pca_ind(pca, geom.ind = "point", pointshape = 23,
             pointsize = 2, 
             fill.ind = iris$Species, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Iris Species") +
  ggtitle("2D PCA-plot from 4 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#K MEANS CLUSTERING

set.seed(42)
iris_kmeans <- kmeans(iris_X, 3)        # We use 3 because we already know that there are 3 classes

iris_kmeans_clusters <- iris_kmeans$cluster %>% 
    replace( iris_kmeans_clusters == 1, "setosa") %>% 
    replace( iris_kmeans_clusters == 2, "versicolor") %>% 
    replace( iris_kmeans_clusters == 3, "virginica")


predicted_vs_true <- data.frame(iris_kmeans_clusters, iris_y)
names(predicted_vs_true) <- c("Predicted", "True Value")
predicted_vs_true$Final <- ifelse(as.factor(predicted_vs_true$Predicted) == as.factor(predicted_vs_true$`True Value`), T, F)

sum(predicted_vs_true$Final, na.rm = T) / count(predicted_vs_true)    #Accuracy


#HIERARCHICAL CLUSTERING

d <- dist(iris, method = "euclidean")
hc <- hclust(d, method="average")
plot(hc, cex=0.6, hang=-1)

hc_iris_predicted <- cutree(hc, k = 3, h = 3) %>% 
  replace(hc_iris_predicted == 1, "setosa") %>% 
  replace(hc_iris_predicted == 2, "versicolor") %>% 
  replace(hc_iris_predicted == 3, "virginica")


hc_predicted_vs_true <- data.frame(hc_iris_predicted, iris_y)
names(hc_predicted_vs_true) <- c("Predicted", "True Value")
hc_predicted_vs_true$final <- ifelse(as.factor(hc_predicted_vs_true$Predicted) == as.factor(hc_predicted_vs_true$`True Value`), T, F)

sum(hc_predicted_vs_true$final, na.rm = T) / count(hc_predicted_vs_true)    #Accuracy
