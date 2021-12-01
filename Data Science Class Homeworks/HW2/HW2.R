library(tidyverse)
library(lattice)
library(caret)
library(reshape2)
library(ggplot2)

# Analyzing the data

breast_cancer_data <- read.csv("../datasets/breast-cancer-wisconsin.csv")
breast_cancer_data$X <- NULL
breast_cancer_data_numeric <- select(breast_cancer_data, radius_mean:fractal_dimension_worst)
head(breast_cancer_data_numeric)  
corr_matrix <- round(cor(breast_cancer_data_numeric),2)
melted_corr_matrix <- melt(corr_matrix)
ggplot(melted_corr_matrix, aes(Var1, Var2, fill=value)) +
  guides(x =  guide_axis(angle = 90)) +
  geom_tile()


# prepare the data for training

y = breast_cancer_data %>% 
  select(diagnosis)
X = breast_cancer_data[, -2]

set.seed(42)
test_inds <- createDataPartition(y$diagnosis, p = 0.2, list = F)
train_data <- breast_cancer_data[-test_inds,]
test_data <- breast_cancer_data[test_inds,]


# Applying Logistic regression

fit <- glm(as.factor(diagnosis) ~ .,data = train_data, family = binomial)
predictions <- predict(fit, newdata= test_data)
predictions <- ifelse(predictions > 0 , "M", "B")
mean(test_data$diagnosis == predictions)


# Applying PCA and Logistic regression

PCA <- prcomp(X, scale. = T, center = T)
pca <- as.data.frame(PCA$x[,1:6])
pca$diagnosis <- y$diagnosis

set.seed(2)
test_inds <- createDataPartition(y$diagnosis, p = 0.2, list = F)
train_data_pca <- pca[-test_inds,]
test_data_pca <- pca[test_inds,]

fit_pca <- glm(as.factor(diagnosis) ~ .,data = train_data_pca, family = binomial)
new_predictions <- predict(fit_pca, newdata= test_data_pca)
new_predictions <- ifelse(new_predictions > 0, "M", "B")
mean(test_data_pca$diagnosis == new_predictions)

  





















