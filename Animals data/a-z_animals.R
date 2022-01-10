library(tidyverse)
library(ggthemes)
library(showtext)
library(scales)
library(gridExtra)


font_add_google("Schoolbell", "bell")
showtext_auto()
animals <- read.csv("datasets/taxonomy.csv")
animals <- animals[2:8]

animals %>% 
  ggplot(aes(x = Phylum, fill= Phylum))+
  geom_bar()+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Number of animals for each Phylum",
       subtitle = "Grouped by Phylum",
       y = "Number of animals",
       x = "Phylum")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))
  
class_bar <- animals %>% 
  ggplot(aes(x = Class, fill= Class))+
  geom_bar()+
  labs(title = "Number of animals for each Class",
       subtitle = "Grouped by Class",
       y = "Number of animals",
       x = "Class")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))

class_gruped <- animals %>% count(Class, sort = T) %>% 
  mutate(percentages= round(n / 936, 2))
class_pie <- class_gruped[1:7,] %>% 
  ggplot(aes(x="", y=n, fill= Class))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))
grid.arrange(class_bar, class_pie, ncol=2)

mammals <- filter(animals, Class == "Mammalia")

mammals %>% 
  ggplot(aes(x= Order, fill= Order))+
  geom_bar()+
  labs(title = "Number of animals for each Order",
       subtitle = "Grouped by Order",
       y = "Number of animals",
       x = "Order")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))

mammals$Order[mammals$Order == " Carnivora" | mammals$Order == "Carnivors"] <- "Carnivora"
mammals$Order[mammals$Order == "Artiodactlya"| mammals$Order == "Atriodactyla"] <- "Artiodactyla"

mammals %>% 
  ggplot(aes(x= Order, fill= Order))+
  geom_bar()+
  labs(title = "Number of animals for each Order",
       subtitle = "Grouped by Order",
       y = "Number of animals",
       x = "Order")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))


summary <- mammals %>% 
  count(Scientific.Name, sort = T)
head(summary)

mammals$Scientific.Name[mammals$Scientific.Name == "Canis lupus" | mammals$Scientific.Name == "Canis lupus familiaris"] <- "Canis Lupus"

summary <- mammals %>% 
  count(Scientific.Name, sort = T)

canis <- filter(mammals, Genus == "Canis")

class_gruped <- animals %>% count(Class, sort = T) %>% 
  mutate(percentages= round(n / 936, 2))
class_gruped


class_gruped[1:7,] %>% 
  ggplot(aes(x="", y=n, fill= Class))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))


