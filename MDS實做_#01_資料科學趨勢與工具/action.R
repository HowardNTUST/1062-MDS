## 問題一
setwd("C:/Users/zino/Desktop/")
data<-read.csv("Carseats.csv", header=T, sep=",")


## 問題二
data$Sales
max(data$Sales)

# 排序
data[order(data$Sales),]
data[order(-data$Sales),]

desorder <- data[order(-data$Sales),]
top10 <- desorder[1:10,]
top10
mean(top10$Sales)

data
## 問題三
for (n in 1:length(data$US)) {
  if(data$US[n] == 'Yes'){
     data$Price[n] <- data$Price[n]*1.05
  }
}
data

colnames(data)

install.packages('ggplot2')
library(ggplot2)

ggplot(data, aes(x = Population, y = Sales)) +
  geom_point()

ggplot(data, aes(x = Population, y = Sales)) + 
  geom_point(aes(color = Price)) 

ggplot(data, aes(x = Population, y = Sales)) + 
  geom_point(aes(color = ShelveLoc)) 



