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

library(ggplot2)
ggplot(all,aes(x=revenue,fill=length(as.factor(all$id))))+
  geom_histogram(binwidth = 5000)+
  ggtitle("Histogram of revenue")+
  xlab("revenue")+
  ylab("number of movie")

missing<-all[!complete.cases(all),]

numericVar<-which(sapply(all,is.numeric))