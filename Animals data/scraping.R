library(rvest)
library(dplyr)
library(RCurl)    ## check for URL exists

link <- "https://a-z-animals.com/animals/"
page <- read_html(link)

animals_list <- page %>% 
  html_nodes(".col-sm-6") %>% 
  html_text()

get_animal_link <- function(animals_list){
  animal_page <- paste(link, animals_list, sep="")
  return(animal_page)
}

links <- sapply(animals_list,get_animal_link,USE.NAMES = F)

animals <- data.frame(matrix(ncol = 7, nrow = 0))
col_names <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Scientific Name")
colnames(animals) <- col_names

get_animal_features <- function(link){
  if (url.exists(link)){
    new_page <- read_html(link)
    features <- new_page %>% 
      html_nodes(".col-sm-9") %>% 
      html_text()
    if (length(features) == 7 ) {
      animals[nrow(animals) + 1,] = features
    }
  }
  
}

sapply(links, get_animal_features)
