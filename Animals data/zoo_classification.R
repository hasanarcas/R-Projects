library(tidyverse)
library(ggthemes)
library(showtext)
library(reshape2)
library(janitor)

zoo <- read.csv("./datasets/zoo.csv")
class_types <- c("Mammal", "Bird", "Reptile", "Fish", "Amphibian", "Bug", "Invertebrate")

for (variable in zoo$class_type){
  zoo$class_type[zoo$class_type == variable] <- class_types[variable]
}
zoo$class_type <- as.factor(zoo$class_type)



############## CORRELATION MATRIX

res <- round(cor(zoo[2:17]), 2)
melted_corr <- melt(res)
ggplot(melted_corr, aes(Var1, Var2, fill=value)) +
  guides(x =  guide_axis(angle = 90)) +
  geom_tile()


##############
zoo_total <- zoo %>% adorn_totals("row")

