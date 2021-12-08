library(rvest)
library(dplyr)

link <- "https://www.imdb.com/search/title/?genres=adventure&sort=user_rating,desc&title_type=feature&num_votes=25000,"
page <- read_html(link)

name <- page %>% html_nodes(".lister-item-header a") %>% html_text()
movie_links <- page %>% html_nodes(".lister-item-header a") %>% 
  html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
year <- page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating <- page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis <- page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()

get_cast <-  function(movie_links){
  movie_page <- read_html(movie_links)
  movie_cast <- movie_page %>% html_nodes(".primary_photo+ td a") %>% 
    html_text()
  return(movie_cast)
}

cast <- sapply(movie_links, get_cast, USE.NAMES = F)


movies <- data.frame(name, year, rating, synopsis, cast, stringsAsFactors = F)

