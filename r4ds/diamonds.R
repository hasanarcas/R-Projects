library(tidyverse)

ggplot(diamonds, aes(carat, price)) +
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")





coffee <- read_csv("C:/Users/hasan/Desktop/datasets/Coffee_dataset.csv")

coffee_points <- coffee %>% 
  select(Aroma:Total.Cup.Points)



preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg <- pivot_longer(preg, c(male, female), names_to = "gender", values_to = "number")


who1 <- who %>% 
  pivot_longer(
    cols= new_sp_m014 : newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = T) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>% 
  select(-new, -iso2, -iso3) %>% 
  separate("sexage",c("sex", "age"), sep = 1)


who2 <- group_by(who1, country, year, sex) %>% 
  filter(year > 1995) %>% 
  summarise(cases = sum(cases)) %>% 
  unite(country_sex, country, sex, remove= F) %>% 
  ggplot(aes(year, cases, group= country_sex, colour = sex)) +
  geom_line()


