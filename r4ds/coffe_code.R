library(tidyverse)

#df_new <- read.csv("Coffee_dataset_modified.csv")

df <- read.csv("Coffee_dataset.csv")
df <- df[, 1:39]

i=1
for (i in c(1:1339)) {
  j = 1
  for(j in c(1:39)){
    if(is.na(df[i,j]) | df[i,j] == ""){
      df[i,j] <- "NULL"
    }
  }
}

i=1
for(i in c(1:1102)){
  if(df[i, 37] != "ft" & df[i, 37] != "m"){
    print(i)
  }
  i=i+1
}
df <- df[-c(221, 629, 651, 920, 962, 1084), ]


counter = 0
i=1
for (i in c(1:1339)) {
  j = 1
  for(j in c(1:39)){
    if(df[i,j] == "NULL"){
      counter = counter + 1
    }
  }
}

#write.csv(df, "Coffee_dataset_modified.csv", row.names = F)





# df <- df[complete.cases(df), ]
# i = 1
# for (attr in df[, ]) {
#     print(colnames(df[i]))
#     print(sum(is.na(attr)))
#     i = i + 1
# }
# 
# i = 1
# for (attr in df[, ]) {
#   print(colnames(df[i]))
#   print(sum(attr == ""))
#   i = i + 1
# }
# 
# 



