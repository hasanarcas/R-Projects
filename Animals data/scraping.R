library(rvest)
library(dplyr)
library(RCurl)    ## check for URL exists

link <- "https://a-z-animals.com/animals/"
page <- read_html(link)

animals_list <- page %>% 
  html_nodes(".col-sm-6") %>% 
  html_text()

get_animal_link <- function(animals_list){
  animal_page <- paste(link, animals_list, sep="") %>% 
    gsub(" ", "-", ., fixed = T) %>% 
    tolower()
  return(animal_page)
}

links <- sapply(animals_list[0:35],get_animal_link,USE.NAMES = F)
links <- "https://a-z-animals.com/animals/albatross/"

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
  print(link)
  return(animals)
}

animals <- t(sapply(links, get_animal_features, USE.NAMES = F))

animals_df <- as.data.frame(animals) %>% 
  apply( 2, as.character)


#write.csv(animals_df, "taxonomy.csv")


#------------------------------------------------------------------------------------------------------------------------

animals <- read.csv("./taxonomy.csv")
animals <- animals[2:8]


for (row in c(1:nrow(animals))) {
  if(animals[row,1] == "logical(0)"){
    animals <- animals[-row, ]
  }
}
