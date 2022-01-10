library(tidyverse)
library(ggthemes)
library(showtext)
library(reshape2)
library(janitor)
library(randomForest)
library(class)
library(caret)
library(rpart)
library(rpart.plot)
library(factoextra)


font_add_google("Schoolbell", "bell")
showtext_auto()


zoo <- read.csv("./datasets/zoo.csv")
class_types <- c("Mammal", "Bird", "Reptile", "Fish", "Amphibian", "Bug", "Invertebrate")

for (variable in zoo$class_type){
  zoo$class_type[zoo$class_type == variable] <- class_types[variable]
}
zoo$class_type <- as.factor(zoo$class_type)
zoo_to_show <- cbind(zoo[1:11], zoo[18])

zoo_total <- zoo %>% adorn_totals("row")
tail(zoo_total, n = 1)

############## CORRELATION MATRIX

res <- round(cor(zoo[2:17]), 2)
melted_corr <- melt(res)
ggplot(melted_corr, aes(Var1, Var2, fill=value)) +
  scale_fill_continuous(type = "viridis")+
  guides(x =  guide_axis(angle = 90)) +
  geom_tile()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))


##############


############## Random Forest
set.seed(42)
zoo_X <- zoo[2:17]
zoo_y <- zoo[18]
test_inds <- createDataPartition(zoo_y$class_type, p=0.2, list=F)
zoo_train_data <- zoo[-test_inds, 2:18]
zoo_test_X <- zoo[test_inds, 2:17]
zoo_test_y <- zoo[test_inds, 18]

fit <- randomForest(class_type ~., data = zoo_train_data)
#plot how important are variables for the model
varImpPlot(fit)

prediction_forest <- predict(fit, zoo_test_X)

confusion_forest <- table(prediction_forest, zoo_test_y)
acc_forest <- sum(diag(confusion_forest)) / sum(confusion_forest)
precison_forest <- diag(confusion_forest) / rowSums(confusion_forest)
recall_forest <- diag(confusion_forest) / colSums(confusion_forest)

##################  TREE
set.seed(42)
zoo_X <- zoo[2:17]
zoo_y <- zoo[18]
test_inds <- createDataPartition(zoo_y$class_type, p=0.2, list=F)
zoo_train_data <- zoo[-test_inds, 2:18]
zoo_train_data_X <- zoo[-test_inds, 2:17]
zoo_train_data_y <- zoo[-test_inds, 18]
zoo_test_data <- zoo[test_inds, 2:18]
zoo_test_data_X <- zoo[test_inds, 2:17]
zoo_test_data_y <- zoo[test_inds, 18]

fit.tree <- rpart(class_type ~., zoo_train_data, method = "class")
rpart.plot(fit.tree, extra = 106)
predictions_tree <- predict(fit.tree, zoo_test_data_X, type = "class")  

confusion_tree <- table(predictions_tree, zoo_test_y)
acc_tree <- sum(diag(confusion_tree)) / sum(confusion_tree)
precison_tree <- diag(confusion_tree) / rowSums(confusion_tree)
recall_tree <- diag(confusion_tree) / colSums(confusion_tree)




#-------------- K means Clustering
set.seed(43)
scaled_zoo <- scale(zoo[2:17])
zoo <- cbind(zoo[1], scaled_zoo, zoo[18])

zoo_X <- zoo[2:17]
zoo_y <- zoo[18]

zoo.kmeans <- kmeans(zoo_X, 7)
fviz_cluster(zoo.kmeans, data = zoo_X, ggtheme = theme_fivethirtyeight()) +
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

zoo.kmeans.clusters <- zoo.kmeans$cluster
confusion.kmeans <- table(zoo.kmeans.clusters, zoo_y[, 1])
acc_kmeans <- sum(diag(confusion.kmeans)) / sum(confusion.kmeans)
precison_kmeans <- diag(confusion.kmeans) / rowSums(confusion.kmeans)
recall_kmeans <- diag(confusion.kmeans) / colSums(confusion.kmeans)

#------------- Hierarchical Clustering

d <- dist(zoo, method = "euclidean")
zoo.hc <- hclust(d, method = "average")
plot(zoo.hc, cex=0.6, hang = -1)

fviz_nbclust(zoo_X, FUN = hcut, method = "wss") + 
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

zoo.hc.predicted <- cutree(zoo.hc, k=7, h=5)
confusion.hc <- table(zoo.hc.predicted, zoo_y[, 1])
(acc_hc <- sum(diag(confusion.hc)) / sum(confusion.hc))
(precison_hc <- diag(confusion.hc) / rowSums(confusion.hc))
(recall_hc <- diag(confusion.hc) / colSums(confusion.hc))








