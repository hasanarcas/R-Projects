library(rvest)

frog <- read_html("https://a-z-animals.com/animals/poison-dart-frog/")
classification <- frog %>% 
  html_elements(".col-sm-9") %>% 
  html_text()

