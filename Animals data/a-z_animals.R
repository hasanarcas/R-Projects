library(tidyverse)
library(ggthemes)
library(showtext)


font_add_google("Gochi Hand", "gochi")
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
  theme(axis.title = element_text(), text = element_text(family = "gochi", size = 15))
  

animals %>% 
  ggplot(aes(x = Class, fill= Class))+
  geom_bar()+
  labs(title = "Number of animals for each Class",
       subtitle = "Grouped by Class",
       y = "Number of animals",
       x = "Class")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "gochi", size = 15), axis.text.x = element_text(angle = 90))

mammals <- filter(animals, Class == "Mammalia")

mammals %>% 
  ggplot(aes(x= Order, fill= Order))+
  geom_bar()+
  labs(title = "Number of animals for each Order",
       subtitle = "Grouped by Order",
       y = "Number of animals",
       x = "Order")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "gochi", size = 15), axis.text.x = element_text(angle = 90))

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
  theme(axis.title = element_text(), text = element_text(family = "gochi", size = 15), axis.text.x = element_text(angle = 90))




