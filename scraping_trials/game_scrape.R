library(rvest)
library(dplyr)
library(RCurl)

#       New Releases

all_games_link <- "https://www.metacritic.com/browse/games/score/metascore/year/pc/filtered"
page <- read_html(all_games_link)
all_games_list <- page %>% 
  html_nodes(".title h3") %>% 
  html_text()

games <- data.frame(matrix(ncol=5, nrow=0))
col_names <- c("Name", "Developer", "Genres", "# Of Players", "Rating")
colnames(games) <- col_names

get_game_features <- function(link){
  if (url.exists(link)){
    game_link <- read_html(link)
    features <- game_link %>% 
      html_nodes("h1 , .product_rating .data , .product_players .data , .product_genre .label+ .data , .button") %>% 
      html_text()
    if (length(features) == 5 ) {
      games[nrow(games) + 1,] = features
    }
    games[nrow(games) + 1, ] <- features
    print(link)
    return(games)
  }
}

get_links <- function(game_link){
    game_page <- paste("https://www.metacritic.com/game/pc/",game_link, sep = "") %>% 
      gsub(" ", "-", ., fixed = T) %>%
      gsub(":", "-", ., fixed = T) %>%
      gsub("'", "", ., fixed = T) %>% 
      tolower()
}
  

links <- sapply(all_games_list, get_links, USE.NAMES = F)


games <- t(sapply(links, get_game_features, USE.NAMES = F))
