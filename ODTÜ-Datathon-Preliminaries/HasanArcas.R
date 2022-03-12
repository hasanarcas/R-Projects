library(dplyr)
library(ggplot2)
library(reshape2)

df <- read.csv("data.set.csv",header = F)[1]

data <- data.frame(matrix(ncol=14, nrow=1))
columns <- strsplit(df[1,], ";")
colnames(data) <- c("Sütun1", "id", "fiyat", "oda_salon_sayisi", "net_m2", "bina_yasi", "isinma_tipi", "krediye_uygunluk", "bulundugu_kat", "banyo_sayisi", "ilce", "nüfus", "eðitim", "okuma_yazma_bilmeyen")
for (i in c(2:4415)) {
  if(lengths(strsplit(df[i,], ";")) == 14){
    a = strsplit(df[i,], ";")
    a <- do.call(rbind.data.frame, a)
    for (j in c(1:14)){
      data[i-1, j] <- a[1, j]
    }
  }
}
data$Sütun1 <- strtoi(data$Sütun1)
data$id <- strtoi(data$id)
data$fiyat <- strtoi(data$fiyat)
data$oda_salon_sayisi <- strtoi(data$oda_salon_sayisi)
data$net_m2 <- strtoi(data$net_m2)
data$bina_yasi <- strtoi(data$bina_yasi)
data$isinma_tipi <- as.factor(data$isinma_tipi)
data$krediye_uygunluk <- as.factor(data$krediye_uygunluk)
data$bulundugu_kat <- as.factor(data$bulundugu_kat)
data$banyo_sayisi <- strtoi(data$banyo_sayisi)
data$ilce <- as.factor(data$ilce)
data$nüfus <- strtoi(data$nüfus)
data$eðitim <- as.factor(data$eðitim)
data$okuma_yazma_bilmeyen <- as.factor(data$okuma_yazma_bilmeyen)


cor(data$fiyat, data$net_m2, use = "complete.obs")

data %>%
  ggplot(aes(net_m2, fiyat))+
  geom_point(alpha = 0.1)+
  labs(title="Price vs Net Square Meters",
       subtitle = "Scatter plot of how the prices changes based on the size of the houses",
       y = "Price (TL)",
       x = "Size (Square Meter)")




central_heating <- data[data$isinma_tipi == "Merkezi(PayÖlçer)" | data$isinma_tipi == "Merkezi",]
central_heating$bulundugu_kat <- as.character(central_heating$bulundugu_kat)
for (i in c(1:length(central_heating$bulundugu_kat))) {
  if(!is.na(central_heating$bulundugu_kat[i])){
    if(!is.na(as.numeric(substr(central_heating$bulundugu_kat[i], 1,1)))){
      central_heating$bulundugu_kat[i] <- strtoi(substr(central_heating$bulundugu_kat[i], 1,1))
    }
  }
}

central_above2 <- central_heating[strtoi(central_heating$bulundugu_kat) >= 2,]
central_above2 <- na.omit(central_above2)
central_above2$ilce[central_above2$ilce == "cankaya"] <- "Çankaya"
number_per_distric <- central_above2 %>% group_by(ilce) %>% summarise(number_of_houses=n())
number_per_distric <- number_per_distric[2:6, ]
ggplot(central_above2, aes(x = ilce))+
  geom_bar()







below_5000 <- data[data$fiyat < 5000,]
below_5000 <- na.omit(below_5000)
ggplot(below_5000, aes(x = ilce))+
  geom_bar()
below_5000_grouped <- below_5000 %>% group_by(ilce) %>% summarise(number_of_houses=n())

below_undergraduate <- below_5000[as.character(below_5000$eðitim)== "Lisans",]
ggplot(below_undergraduate, aes(x=ilce))+
  geom_bar()




data$ilce[data$ilce == "cankaya"] <- "Çankaya"
districts <- na.omit(data %>% group_by(ilce, isinma_tipi))
districts <- districts %>% summarise(mean_price=mean(fiyat))
districts <- districts[4:21,]
ordered <- districts[order(districts$ilce, districts$mean_price), ]
ordered

ggplot(ordered, aes(x=ilce, y=mean_price, fill= isinma_tipi))+
  geom_bar(stat="identity",position="dodge")

