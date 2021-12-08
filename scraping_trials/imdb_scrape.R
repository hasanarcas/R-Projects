library(rvest)
library(dplyr)

link <- "https://www.imdb.com/search/title/?genres=adventure&sort=user_rating,desc&title_type=feature&num_votes=25000,"
page <- read_html(link)

name <- page %>% html_nodes(".lister-item-header a") %>% html_text()
year <- page %>% html_nodes(".text-muted.unbold") %>% html_text()
rating <- page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
synopsis <- page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()

movies <- data.frame(name, year, rating, synopsis, stringsAsFactors = F)
