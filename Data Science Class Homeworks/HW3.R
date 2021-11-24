library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(datasets)
library(stats)

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
             pointsize = 3, 
             fill.ind = iris$Species, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


set.seed(42)
iris_kmeans <- kmeans(iris_X, 3)



