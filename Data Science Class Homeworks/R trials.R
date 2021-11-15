#----------------------------------------------------------------------
plot(mtcars$wt, mtcars$mpg,
     pch = 19,
     cex = 1.5, 
     col = "#cc0000",
     main = "MPG as a function of Weight of Cars",
     xlab = "Weight (in 1000 pounds",
     ylab = "MPG")

#----------------------------------------------------------------------

hist(lynx,
     breaks = 14,       # Suggests 14 bins
     freq = FALSE,      # Axis shows density, not frequency
     col = "yellow",
     main = paste("Histogram of annual Canadian Lynx",
                  "Trappings", "1821 - 1934"),
     xlab = "number of Lynx Trapped")

# add a normal distribution
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "blue",
      lwd = 2,          # line width = 2 pixels
      add = TRUE)       # Superimpose on previous graph

# add two kernel density estimators
lines(density(lynx), col = "red", lwd = 2)
lines(density(lynx, adjust = 3), col = "purple", lwd = 2)

# add a rug plot
rug(lynx, lwd = 2, col = "gray")


#----------------------------------------------------------------------

z = iris
summary(iris$Species)
summary(iris$Sepal.Length)
summary(iris)

#-----------------------------------------------------------------------


describe(iris$Sepal.Length)
describe(iris)


#----------------------------------------------------------------------

head(iris)

hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal length for versicolor species")

hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = "Petal length < 2")

hist(iris$Petal.Length[iris$Species == "virginica" &
                       iris$Petal.Length < 5.5],
     main = "virginica iris with length < 5.5",
     col = "yellow")

#--------------------------------------------------------------------

setosa = iris[iris$Species == "setosa", ]     #it takes all the setosa species, and all the rows of the species because we left blank after the comma

#--------------------------------------------------------------------

#data structures
# VECTORS
v1 <- c(1, 2, 3, 4, 5)      # c stands for combine

v2 <- c("a", "b", "c")

# MATRIX

m1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m2 <- matrix(c("a", "b",
               "c", "d"),
             nrow = 2,
             byrow = T)

# ARRAY

a1 <- array(c(1:24), c(4, 3, 2))      #for the given data (1:24), give dimensions (rows, columns, tables)

# DATA FRAME

vNumeric <- c(1,2,3)
vChar <- c("a","b","c")
vLogical <- c(T,F,T)

df <- as.data.frame(cbind(vNumeric, vChar, vLogical))

# LIST

o1 <- c(1,2,3)
o2 <- c("a","b","c","d")
o3 <- c(T,F,T,T,F)

list1 <- list(o1,o2,o3)
list2 <- list(o1,o2,o3,list1)

#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------






car_df <-  mpg

ggplot(car_df) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class), position = "jitter")

#--------------------------------------------------------------------------------------------------------------------


ggplot(car_df) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  facet_grid( drv ~ .)

#--------------------------------------------------------------------------------------------------------------------


ggplot(data = car_df, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) +
  geom_smooth()
  

#--------------------------------------------------------------------------------------------------------------------


dia <- diamonds

ggplot(data = dia)+
  geom_bar(mapping = aes(x=cut, fill = clarity), position="dodge")

#---------------------------------------------------------------------------------------------------------------------

jan1 <- filter(flights, month == 1, day ==1)        #all flights occured on January 1st

delay_less_2_hours <- filter(flights, arr_delay >= 120)
total_na_dep_time <- sum(is.na(flights$dep_time))


arrange(flights, year, month, day)    # it sorts all the flights by the year, for equal years it sorts by month, for equal months it sorts by day

select(flights, year, month, day, air_time)

select(flights, year:day)       #select all cols between year and day (inclusive)

select(flights, -(year:day))    #select all cols except between year and day (inclusive)

#-------------------------------------------------------------------------------------------------------------------------

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

transmute(flights,
          gain = dep_delay - arr_delay,           #transmute keeps only the new generated attributes
          hours = air_time / 60,
          gain_per_hour = gain / hours)


summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#---------------------------------------------------------------------------------------------------------------------------

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
summarise(group_by(flights, dest), delay = mean(dep_delay, na.rm = TRUE))

#-------------------------------------------------------------------------------------------------------------------------


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")


delays <- flights %>% 
  group_by(dest) %>% 
  summarise(                                                           # THIS 2 CODES MAKE THE SAME THING
    count = n(),                                                       # the second form used is called pipe, it takes the result and makes it the first argument of the next function
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")


#-----------------------------------------------------------------------------------------------------------------------------

not_cancelled <- flights %>%                               #filter all non na values
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)



#-----------------------------------------------------------------------------------------------------------------------------




pokemon <- read.csv("./datasets/pokemon.csv")

pokemon_df <- as_tibble(select(pokemon, pokedex_number, name, type1, attack, defense, hp, generation, is_legendary))

sorted_non_legendary_first_and_second_generation <- pokemon_df %>%
  filter(is_legendary==0 & (generation==1 | generation==2)) %>%
  mutate(total_points = attack + defense + hp) %>%
  arrange(desc(total_points))

ggplot(sorted_non_legendary_first_and_second_generation) +
  geom_bar(mapping=aes(x = type1, fill= type1))


ggplot(sorted_non_legendary_first_and_second_generation) +
  geom_point(mapping=aes(x= attack, y=total_points))
  

ggplot(sorted_non_legendary_first_and_second_generation) +
  geom_point(mapping=aes(x= attack, y=total_points, color = hp))


#-----------------------------------------------------------------------------------------------------------------------------

#   EXPLORATORY DATA ANALYSIS

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut),  fill= "green")


small_carat_diamonds <- diamonds %>% 
  filter(carat < 3)

ggplot(small_carat_diamonds, mapping= aes(x=carat))+
  geom_histogram(binwidth = 0.1)

ggplot(small_carat_diamonds, mapping=aes(x = carat, color = cut))+
  geom_freqpoly(binwidth=0.1)


ggplot(diamonds) +
  geom_histogram(mapping = aes(x = carat))
  
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y<3 | y>20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

#-------------------------------------------------------------------------------------------------------------


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()


ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()



nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(y = sched_dep_time, x = cancelled)) + 
  geom_boxplot()



ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))



diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))




diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))



flights %>% 
  group_by(month, dest) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")


ggplot(data = small_carat_diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))


ggplot(data = small_carat_diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))




diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()


#----------------------------------------------------------------------------------------------------------


