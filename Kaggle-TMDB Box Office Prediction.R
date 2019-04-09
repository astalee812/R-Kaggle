install.packages("magrittr")
library(magrittr)
setwd("C:/Users/ASUS/Desktop/TMDB Box Office Prediction")
install.packages("reader")
library(readr)
test<-read_csv("C:/Users/ASUS/Desktop/TMDB Box Office Prediction/test.csv")
train<-read_csv("C:/Users/ASUS/Desktop/TMDB Box Office Prediction/train.csv")
str(train)
str(test)

test$revenue<-NA
all<-rbind(train,test)
trainset<-which(!is.na(all$revenue))

#revenue呈現右偏
library(ggplot2)
ggplot(all,aes(x=revenue,fill=length(as.factor(train$id))))+
  geom_histogram()

#先看看numeric的變數
numericVar<-which(sapply(all,is.numeric))
library(corrplot)
all_numVar<-all[,numericVar]
cor_all<-cor(all_numVar,use = "pairwise.complete.obs")
cor_all_matrix<-corrplot.mixed(cor_all,to.col="black",tl.pos = "lt")

#看看遺漏值
nacl<-which(colSums(is.na(all))>0)
sort(colSums(sapply(all[,nacl], is.na)),decreasing = TRUE)
length(sort(colSums(sapply(all[,nacl], is.na)),decreasing = TRUE))

str(all$belongs_to_collection)