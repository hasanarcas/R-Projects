library(rvest)
library(dplyr)
library(RCurl)
library(httr)
#       New Releases
fetch_new_releases <- function(){
  all_games_link <- "https://www.metacritic.com/browse/games/score/metascore/year/pc/filtered"
  page <- read_html(all_games_link)
  all_games_list <- page %>% 
    html_nodes(".title h3") %>% 
    html_text()
  
  games <- data.frame(matrix(ncol=5, nrow=0))
  col_names <- c("name", "point", "summary", "dev", "genre")
  colnames(games) <- col_names
  
  get_links <- function(game_link){
    game_link <- game_link %>% 
      gsub(" ", "-", ., fixed = T) %>%
      gsub(":", "", ., fixed = T) %>%
      gsub("'", "", ., fixed = T) %>% 
      tolower()
    
    game_page <- paste("https://www.metacritic.com/game/pc/",game_link, sep = "")
    return(game_page)
  }
  links <- sapply(all_games_list, get_links, USE.NAMES = F)
  
  get_game_features <- function(link){
    print(link)
    game_link <- read_html(link)
    features <- game_link %>% 
      html_nodes(".product_summary .data span , .product_genre .label+ .data , .button , .positive span , h1") %>% 
      html_text()
    if(length(features) > 5){
      features <- features[c(1, 2, 5, 8, 9)]
    }
    games[nrow(games) + 1,] =  features
    return(games)
  }
  
  links <- c("https://www.metacritic.com/game/pc/disco-elysium-the-final-cut", 
             "https://www.metacritic.com/game/pc/forza-horizon-5",
             "https://www.metacritic.com/game/pc/chicory-a-colorful-tale")
  
  games <- t(sapply(links, get_game_features, USE.NAMES = F))
  
  games_df <- as.data.frame(games) %>% 
    apply( 2, as.character)
  
  games_df_2 <- games_df
  
  #write.csv(games_df,"games_data.csv")
}




#       images for new releases

session <- html_session("https://www.metacritic.com/game/pc/disco-elysium-the-final-cut") #for authentication
img_src <- session %>%
  read_html() %>%
  html_node(xpath = '') %>%
  html_attr("src")
