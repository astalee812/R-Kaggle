install.packages("magrittr")
library(magrittr)
setwd("C:/Users/ASUS/Desktop/TMDB Box Office Prediction")
install.packages("reader")
library(readr)
test<-read_csv("C:/Users/ASUS/Desktop/TMDB Box Office Prediction/test.csv")
train<-read_csv("C:/Users/ASUS/Desktop/TMDB Box Office Prediction/train.csv")

install.packages("plyr")
library(plyr)
install.packages("data.table")
library(data.table)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringi")
library(stringi)

#先做一個function用來整理json的正規表達式
map.func = function(x, y = 2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[,.:""]')[[1]][y]}) %>% 
    sub("[[:punct:]]", '',.) %>% 
    sub("'",'',.) %>% as.factor() %>% as.numeric()
  return(map)
}

#這邊要做budget跟revenue單位的修正
for (i in 1:nrow(train)) {
  if(train[i,"budget"] > 1000 & train[i,"revenue"] < 100){
    train[i,"revenue"] = train[i,"revenue"] * 10^6
  }
}

train1.id = train$id
label = train$revenue
test1.id = test$id

#我先把id跟revenue抽走，然後將兩個train跟test做結合
train<-train %>% within(rm("id","revenue"))
test<-test %>% within(rm("id"))
df<-rbind(train,test)
train_raw<-df

#針對df資料裡面release date做整理，讓他變成一致的格式
date.format<-as.Date(df$release_date,format = "%m/%d/%Y")

#針對空值的處理，全部轉成NA，然後有15個欄位是有NA值的
df[df== ""] <- NA
na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
length(na.cols)
