library(rvest)
library(dplyr)

link <- "https://a-z-animals.com/animals/"
page <- read_html(link)

animals_list <- page %>% 
  html_nodes(".col-sm-6") %>% 
  html_text()

animals_list <- gsub(" ", "-", animals_list)



for(i in animals_list){
  new_link <- paste(link,i,sep="")
}
