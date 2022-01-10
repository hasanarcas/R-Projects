library(tidyverse)
library(ggthemes)
library(showtext)
library(caret)
library(randomForest)
library(mice)
library(nnet)
library(smotefamily)
library(ggfortify)

font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
showtext_auto()


crabs_df <- read.csv("./datasets/CrabAgePrediction.csv")

colnames(crabs_df)[6] <- "Shucked Weight"
colnames(crabs_df)[7] <- "Viscera Weight"
colnames(crabs_df)[8] <- "Shell Weight"

foot_to_cm <- 30.48
ounce_to_kg <- 0.0283495

crabs_df$Length <- crabs_df$Length * foot_to_cm
crabs_df$Diameter <- crabs_df$Diameter * foot_to_cm
crabs_df$Height <- crabs_df$Height * foot_to_cm
crabs_df$Weight <- crabs_df$Weight * ounce_to_kg
crabs_df$`Shucked Weight` <- crabs_df$`Shucked Weight` * ounce_to_kg
crabs_df$`Viscera Weight` <- crabs_df$`Viscera Weight` * ounce_to_kg
crabs_df$`Shell Weight` <- crabs_df$`Shell Weight` * ounce_to_kg





######### COMMENT ABOUT HOW THE DATA IS DISTRIBUTED AND WHY WE CHOOSE MALE
crabs_df %>% 
  ggplot(aes(x = Sex, fill= Sex))+ 
  geom_bar() +
  scale_fill_manual(values= c("#fc53d5", "#8c8f8f","#5d3ef7"))+
  labs(title = "Number of crabs for each Sex group",
       subtitle = "How many male, female and indeterminate crabs do we have in our dataset?",
       y = "Number of crabs",
       x = "Sex")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))
m_crab <- filter(crabs_df, Sex == "M")






################## COMMENT ABOUT HOW THE CRABS ARE DISTRIBUTED FOR THEIR WEIGHT
m_crab %>% 
  ggplot(aes(x = Weight,  fill=cut(Weight, 100))) +
  geom_histogram( binwidth = 0.07, show.legend = F) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70)+
  labs(title = "Number of crabs for the given Weight (kg)",
       subtitle = "Binwidth is set to 0.07", 
       x = "Weight (cm)",
       y = "Number of crabs")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(),  text = element_text(family = "bell", size = 15))





############   COMMENT ABOUT HOW THERE IS A CORRELATION SO IT WOULD BE WISE TO DRAW A HEATMAP
m_crab %>% 
  ggplot(aes(x = Length, y = Diameter, color = Weight))+
  geom_point() +
  scale_color_gradientn(colors= rainbow(5)) + 
  labs(title="Diameter Vs Length Vs Height",
       subtitle = "Scatter plot of how Diameter and Height change based on the Length of Male crabs",
       y = "Diameter (cm)",
       x = "Lenght (cm)")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))


########### LINEAR REGRESSION
set.seed(42)
crab_X <- m_crab[2:8]
crab_y <- m_crab[9]
test_inds <- createDataPartition(crab_y$Age, p=0.2, list=F)
crab_train_data <- m_crab[-test_inds,2:9]
crab_test_data_X <- m_crab[test_inds, 2:8]
crab_test_data_y <- m_crab[test_inds, 9]
fit <- lm(Age ~ .,data = crab_train_data)
predictions <- fit %>% predict(crab_test_data_X)
RMSE(predictions, crab_test_data_y)



############## CORRELATION MATRIX
library(reshape2)
res <- round(cor(m_crab[2:9]), 2)
melted_corr <- melt(res)
ggplot(melted_corr, aes(Var1, Var2, fill=value)) +
  guides(x =  guide_axis(angle = 90)) +
  scale_fill_continuous(type = "viridis")+
  geom_tile()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))



############ PCA AND LINEAR REGRESSION
PCA <- prcomp(crab_X, scale. = T, center = T)
library(ggfortify)
autoplot(PCA, data = m_crab, colour= "Age")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))+
  scale_color_continuous(type = "viridis")
  

var_explained_df <- data.frame(PC = paste0("PC", 1:7), var_explained = (PCA$sdev)^2 / sum((PCA$sdev)^2)) 
var_explained_df %>%
  ggplot(aes(x=PC,y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data",
       subtitle = "Using elbow method plotting to decide the number of PCA's to use")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

pca <- as.data.frame(PCA$x[, 1:2])
pca$Age <- m_crab$Age
set.seed(101)
crab_train_pca <- pca[-test_inds,]
crab_test_pca <- pca[test_inds,]
fit_pca <- lm(Age~ ., data= crab_train_pca)
new_predictions <- fit_pca %>% predict(crab_test_pca)
RMSE(new_predictions, crab_test_pca$Age)



#############  DELETE DATA AND INPUTE     CANCEL 10000 CELLS OUT OF 31144
no_of_NA <- 10000
crab_convert_to_na <- crabs_df[2:9]

for(i in c(1:no_of_NA)){
  crab_convert_to_na[sample(nrow(crab_convert_to_na),1), sample(ncol(crab_convert_to_na), 1)] <- NA
}

crabs_df <- as.data.frame(c(crabs_df[1], crab_convert_to_na))
sum(is.na(crabs_df))

##### classification for missing
crabs_df$Sex <- as.factor(crabs_df$Sex)
crabs_omitted <- na.omit(crabs_df)
set.seed(123)
crabs_X <- crabs_omitted[2:9]
crabs_y <- crabs_omitted[1]

test_inds <- createDataPartition(crabs_y$Sex, p=0.2, list=F)
crab_train_data <- crabs_omitted[-test_inds,]
crab_test_data_X <- crabs_omitted[test_inds, 2:9]
crab_test_data_y <- crabs_omitted[test_inds, 1]

fit.deleted <- multinom(Sex ~ ., crab_train_data)

predictions.deleted <- predict(fit.deleted, crab_test_data_X)

confusion_deleted <- table(predictions.deleted, crab_test_data_y)
acc_deleted <- sum(diag(confusion_deleted)) / sum(confusion_deleted)
precison_deleted <- diag(confusion_deleted) / rowSums(confusion_deleted)
recall_deleted <- diag(confusion_deleted) / colSums(confusion_deleted)


# predictive mean matching =  pmm
imputed_crabs <- mice(crabs_df, m=5, maxit = 50, method = "pmm", seed= 42)
imputed_crabs_df <- complete(imputed_crabs, 2)
set.seed(123)
crabs_X <- imputed_crabs_df[2:9]
crabs_y <- imputed_crabs_df[1]

test_inds <- createDataPartition(crabs_y$Sex, p=0.2, list=F)
crab_train_data <- imputed_crabs_df[-test_inds,]
crab_test_data_X <- imputed_crabs_df[test_inds, 2:9]
crab_test_data_y <- imputed_crabs_df[test_inds, 1]

fit.imputed <- multinom(Sex ~ ., crab_train_data)
predictions.imputed <- predict(fit.imputed, crab_test_data_X)

confusion_imputed <- table(predictions.imputed, crab_test_data_y)
acc_imputed <- sum(diag(confusion_imputed)) / sum(confusion_imputed)
precison_imputed <- diag(confusion_imputed) / rowSums(confusion_imputed)
recall_imputed <- diag(confusion_imputed) / colSums(confusion_imputed)

comparison.accuracy <- t(data.frame(acc_deleted, acc_imputed))
colnames(comparison.accuracy) <- "Accuracy"
comparison.precision <- t(data.frame(precison_deleted, precison_imputed))
comparison.recall <- t(data.frame(recall_deleted, recall_imputed))


############# MAKE DATA UNBALANCED
table(crabs_df$Sex)
imbalanced_crabs <- crabs_df[-sample(which(crabs_df[, "Sex"] == "F"), 1000),]
table(imbalanced_crabs$Sex)


set.seed(1234)
crabs_X <- imbalanced_crabs[2:9]
crabs_y <- imbalanced_crabs[1]

test_inds <- createDataPartition(crabs_y$Sex, p=0.2, list=F)
crab_train_data <- imbalanced_crabs[-test_inds,]
crab_test_data_X <- imbalanced_crabs[test_inds, 2:9]
crab_test_data_y <- imbalanced_crabs[test_inds, 1]

fit.imbalanced <- multinom(Sex ~ ., crab_train_data)

predictions_imbalanced <- predict(fit.imbalanced, crab_test_data_X)

confusion_imbalanced <- table(predictions_imbalanced, crab_test_data_y)

acc_imbalanced <- sum(diag(confusion_imbalanced)) / sum(confusion_imbalanced)
precison_imbalanced <- diag(confusion_imbalanced) / rowSums(confusion_imbalanced)
recall_imbalanced <- diag(confusion_imbalanced) / colSums(confusion_imbalanced)



oversampled_crabs <- SMOTE(imbalanced_crabs[2:9], imbalanced_crabs[1], dup_size = 4)
oversampled_crabs$data$class <- as.factor(oversampled_crabs$data$class)
rename(oversampled_crabs$data, Sex = class) 
table(oversampled_crabs$data$class)

set.seed(1234)
crabs_X <- oversampled_crabs$data[1:8]
crabs_y <- oversampled_crabs$data[9]

test_inds <- createDataPartition(crabs_y$class, p=0.2, list=F)
crab_train_data <- oversampled_crabs$data[-test_inds,]
crab_test_data_X <- oversampled_crabs$data[test_inds, 1:8]
crab_test_data_y <- oversampled_crabs$data[test_inds, 9]

fit <- multinom(class ~ ., crab_train_data)

predictions_oversampled <- predict(fit, crab_test_data_X)

confusion_oversampled <- table(predictions_oversampled, crab_test_data_y)

acc_oversampled <- sum(diag(confusion_oversampled)) / sum(confusion_oversampled)
precison_oversampled <- diag(confusion_oversampled) / rowSums(confusion_oversampled)
recall_oversampled <- diag(confusion_oversampled) / colSums(confusion_oversampled)


  
  
  
  
