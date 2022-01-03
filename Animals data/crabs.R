library(tidyverse)
library(ggthemes)
library(showtext)
library(caret)
library(randomForest)

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
  geom_tile()



############ PCA AND LINEAR REGRESSION
PCA <- prcomp(crab_X, scale. = T, center = T)
pca <- as.data.frame(PCA$x[, 1:2])
pca$Age <- m_crab$Age
set.seed(101)
crab_train_pca <- pca[-test_inds,]
crab_test_pca <- pca[test_inds,]
fit_pca <- lm(Age~ ., data= crab_train_pca)
new_predictions <- fit_pca %>% predict(crab_test_pca)
RMSE(new_predictions, crab_test_pca$Age)






  