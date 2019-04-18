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

#先做一個function用來整理json的正規表達式，這部分是抓其他人的code，全部都是抓取id的位子
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

#針對release date做統整
install.packages("lubridate")
library(lubridate)
#這邊可以看到取出來的數值是尾數兩個，需要再做處理，先做一個年份處理的function
df$year<-year(date.format)
year.fun<-function(x){
  if(is.na(x)){
    return(paste0("2000"))
  }else if(x<10){
    return(paste0("200",x))
  }else if(x>=10&x<=18){
    return(paste0("20",x))
  }else{
    return(paste0("19",x))
  }
}
df$year<-sapply(df$year,year.fun)
df$year<-as.numeric(df$year)
#把月份抽取出來
df$month<-month(date.format)
#把星期抽取出來，一開始出來的會是中文字，先轉成factor之後再轉成numeric就會變成數字
df$weekday<-weekdays(date.format)
df$weekday<-as.factor(df$weekday)
df$weekday<-as.numeric(df$weekday)
#把季抽取出來，一開始抽取出來會是Q1-Q4，先轉factor再轉numeric
df$quarter<-quarters(date.format)
df$quarter<-as.factor(df$quarter)
df$quarter<-as.numeric(df$quarter)
# 把日期+3，"%%"餘數計算
df$dayofweek2<-(as.numeric(days(date.format))+3)%%7
#計算時間是第幾周
df$week<-week(date.format)

df$cast_len<-stri_length(df$cast)
df$cast_nword<-stri_count(df$cast, regex="name")
df$p_comp_len<-stri_length(df$production_companies)
df$P_comp_nword<-stri_count(df$production_companies,regex="name")
df$tag_length<-stri_length(df$tagline)
df$tagline_nword<-stri_count(df$tagline,regex = "name")
df$tag_hasNA<-ifelse(is.na(df$tagline),0,1)
df$lan_isEN<-ifelse(df$original_language=="en",0,1)
df$collection_id<-map.func(df$belongs_to_collection)
df$collection_hasNA<-ifelse(is.na(df$belongs_to_collection),0,1)
df$homepage_hasNA<-ifelse(is.na(df$homepage),0,1)
df$sametitle<-ifelse(df$title==df$original_title,1,0)
df$crew_len<-stri_length(df$crew)
df$crew_nword<-stri_count(df$crew,regex = "name")
df$keywords_len<-stri_length(df$Keywords)
df$keywords_nword<-stri_count(df$Keywords,regex = "name")
df$production_countries<-map.func(df$production_countries)
df$com1<-map.func(df$production_companies)
df$spoken_languages<-map.func(df$spoken_languages)
df$genres<-map.func(df$genres)
df$status<-as.numeric(as.factor(df$status))
df$original_title<-as.numeric(as.factor(df$original_title))
df$original_language<-as.numeric(as.factor(df$original_language))

df<-df[,c("budget","original_language","popularity","spoken_languages",
          "production_countries","genres","runtime","tagline_nword",
          "tag_hasNA","lan_isEN","sametitle","crew_len","cast_len",
          "homepage_hasNA","collection_hasNA",
          "tag_length","keywords_len","year","month","weekday","week",
          "p_comp_len","dayofweek2","cast_nword")]


install.packages("tidyr")
library(tidyr)
train<-train_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE)

train$collectionID<-ifelse(is.na(train$idPart)==FALSE,gsub("\\D","",train$idPart),train$idPart)
train$collectionID<-ifelse(is.na(train$collectionID),0,train$collectionID)
train$partofcollection<-ifelse(is.na(train$idPart),0,1)
train$hashomepage<-ifelse(is.na(train$homepage),0,1)
train$genres<-ifelse(is.na(train$genres),"Nogen",train$genres)
train$genComedy<-ifelse(stri_detect_fixed(train$genres,"Comedy"),1,0)
train$genDrama<-ifelse(stri_detect_fixed(train$genres,"Drama"),1,0)
train$genThriller<-ifelse(stri_detect_fixed(train$genres,"Thriller"),1,0)
train$genAction<-ifelse(stri_detect_fixed(train$genres,"Action"),1,0)
train$genAnimation<-ifelse(stri_detect_fixed(train$genres,"Animation"),1,0)
train$genHorror<-ifelse(stri_detect_fixed(train$genres,"Horror"),1,0)
train$genDocumentary<-ifelse(stri_detect_fixed(train$genres,"Documentary"),1,0)
train$genAdventure<-ifelse(stri_detect_fixed(train$genres,"Adventure"),1,0)
train$genCrime<-ifelse(stri_detect_fixed(train$genres,"Crime"),1,0)
train$genMystery<-ifelse(stri_detect_fixed(train$genres,"Mystery"),1,0)
train$genFantasy<-ifelse(stri_detect_fixed(train$genres,"Fantasy"),1,0)
train$genWar<-ifelse(stri_detect_fixed(train$genres,"War"),1,0)
train$genScienceFiction<-ifelse(stri_detect_fixed(train$genres,"Science Fiction"),1,0)
train$genRomance<-ifelse(stri_detect_fixed(train$genres,"Romance"),1,0)
train$genMusic<-ifelse(stri_detect_fixed(train$genres,"Music"),1,0)
train$genWestern<-ifelse(stri_detect_fixed(train$genres,"Family"),1,0)
train$genHistory<-ifelse(stri_detect_fixed(train$genres,"History"),1,0)
train$genForeign<-ifelse(stri_detect_fixed(train$genres,"Foreign"),1,0)
train$genTVMovie<-ifelse(stri_detect_fixed(train$genres,"TV Movie"),1,0)
train$production_companies<-ifelse(is.na(train$production_companies)==TRUE,"NoProd",train$production_companies)
train$proUniversal<-ifelse(stri_detect_fixed(train$production_companies,"Universal Pictures"),1,0)
train$proParamount<-ifelse(stri_detect_fixed(train$production_companies,"Paramount Pictures"),1,0)
train$proTCF<-ifelse(stri_detect_fixed(train$production_companies,"Twentieth Century Fox Film Corporation"),1,0)
train$proColumbia<-ifelse(stri_detect_fixed(train$production_companies,"Columbia Pictures"),1,0)
train$proWarner<-ifelse(stri_detect_fixed(train$production_companies,"Warner Bros."),1,0)
train$proNLC<-ifelse(stri_detect_fixed(train$production_companies,"New Line Cinema"),1,0)
train$proDisney<-ifelse(stri_detect_fixed(train$production_companies,"Walt Disney Pictures"),1,0)



