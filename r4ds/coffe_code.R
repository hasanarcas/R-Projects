library(tidyverse)

df <- read.csv("Coffee_dataset.csv")
df <- df[, 1:39]

i=1
for (i in c(1:1339)) {
  j = 1
  for(j in c(1:39)){
    if(is.na(df[i,j])){
      df[i,j] <- ""
    }
  }
}



coffee <- select(df, ID, Grading.Date, Expiration)
col_names <- row.names(t(df))
numeric_type <- as.data.frame(c(col_names[19:32],col_names[11],col_names[34],col_names[38],col_names[39]))
colnames(numeric_type) <- "NumericTypeName"
string_type <- as.data.frame(c(col_names[2:7], col_names[9:10], col_names[12:14], col_names[17:18], col_names[33], col_names[36:37]))
colnames(string_type) <- "StringTypeName"

strings_idcoffee <- vector()
i <- 1
for (x in c(1:21424)) {
  strings_idcoffee <- append(strings_idcoffee, i)
  if(x %% 16 == 0){
    i = i+1
  }
}
strings_typeid <- vector()
i <- 1
for (x in c(1:21424)) {
  strings_typeid <- append(strings_typeid, i)
  i= i+1
  if(x %% 16 == 0){
    i = 1
  }
}

df_string <- select(df, 2:7, 9:10, 12:14, 17:18, 33, 36:37)
df_string_values <- as.vector(t(df_string))
strings_value <- vector()
i <- 1
for (x in c(1:21424)) {
  strings_value <- append(strings_value, df_string_values[i])
  i = i +1
}

Coffee2String2Value <- data.frame(strings_idcoffee, strings_typeid, strings_value)
#---------------------------------------------------------

int_idcoffee <- vector()
i <- 1
for (x in c(1:24102)) {
  int_idcoffee <- append(int_idcoffee, i)
  if(x %% 18 == 0){
    i = i+1
  }
}
int_typeid <- vector()
i <- 1
for (x in c(1:24102)) {
  int_typeid <- append(int_typeid, i)
  i= i+1
  if(x %% 18 == 0){
    i = 1
  }
}

df_int <- select(df, 19:32, 11, 34, 38, 39)
df_int_values <- as.vector(t(df_int))
int_values <- vector()
i <- 1
for (x in c(1:24102)) {
  int_values <- append(int_values, df_int_values[i])
  i = i +1
}

Coffee2Numeric2Value <- data.frame(int_idcoffee, int_typeid, int_values)

colnames(Coffee2Numeric2Value) <- c("coffeid", "typeid", "value")
colnames(Coffee2String2Value) <- c("coffeid", "typeid", "value")

write.csv(coffee, "./tables/coffee.csv")
write.csv(numeric_type, "./tables/numeric_type.csv")
write.csv(string_type, "./tables/string_type.csv")
write.csv(Coffee2Numeric2Value, "./tables/coffee2numeric2value.csv")
write.csv(Coffee2String2Value, "./tables/coffee2string2value.csv")


# i=1
# for(i in c(1:1102)){
#   if(df[i, 37] != "ft" & df[i, 37] != "m"){
#     print(i)
#   }
#   i=i+1
# }
# df <- df[-c(221, 629, 651, 920, 962, 1084), ]




