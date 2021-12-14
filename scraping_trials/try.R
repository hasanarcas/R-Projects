game_link <- read_html("https://www.metacritic.com/game/pc/it-takes-two")
features <- game_link %>% 
  html_nodes(".product_summary .data span , .product_genre .label+ .data , .button , .positive span , h1") %>% 
  html_text()
if(length(features) > 5){
  features <- features[c(1, 2, 5, 8, 9)]
}
print(features)




##   name point summary  dev  genre