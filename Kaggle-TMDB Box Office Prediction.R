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
ggplot(train,aes(x=revenue,fill=length(as.factor(train$id))))+
  geom_histogram()

#先看看numeric的變數
numericVar<-which(sapply(train,is.numeric))
library(corrplot)
all_numVar<-train[,numericVar]
cor_all<-cor(all_numVar,use = "pairwise.complete.obs")
cor_all_matrix<-corrplot.mixed(cor_all,to.col="black",tl.pos = "lt")

#看看遺漏值
nacl<-which(colSums(is.na(train))>0)
sort(colSums(sapply(train[,nacl], is.na)),decreasing = TRUE)
length(sort(colSums(sapply(train[,nacl], is.na)),decreasing = TRUE))
str(train$belongs_to_collection)

#處理最多的遺漏值變數:belong to collection
#這個系列電影data其實是json字串格是，使用正規表達式做字串切割
install.packages("stringr")
library(stringr)
train$collection_id <- str_extract(train$belongs_to_collection, "(?<=id\\'\\:\\s{1})\\d{1,}")
train$collection_name <- str_extract(train$belongs_to_collection, "(?<=name\\'\\:\\s{1}\\').+(?=\\'\\,\\s{1}\\'poster)")

#看看系列電影的數量有多少，並且排名
library(dplyr)
train %>%
  group_by(collection_name) %>%
  summarise(numberOfMovies = n()) %>%
  arrange(desc(numberOfMovies)) %>%
  filter(!is.na(collection_name)) %>%
  head(20)

#系列電影的各系列平均收入有多少，並且排名
train %>%
  group_by(collection_name) %>%
  summarise(collectionAvgRevenue = mean(revenue)) %>%
  arrange(desc(collectionAvgRevenue)) %>%
  filter(!is.na(collection_name)) %>%
  head(10)

#系列電影的數量
train%>%
  filter(!is.na(collection_name))%>%
  summarise(numberofmovies=n())
#系列電影的平均收入
train%>%
  filter(!is.na(collection_name))%>%
  summarise(noncollectionRevenue=mean(revenue))

#非系列電影的數量
train%>%
  filter(is.na(collection_name))%>%
  summarise(numberofmovies=n())
#非系列電影的平均收入
train%>%
  filter(is.na(collection_name))%>%
  summarise(noncollectionRevenue=mean(revenue))

#轉換系列跟非系列電影的變數
train$belongs_to_collection[!is.na(train$belongs_to_collection)] <- 1
train$belongs_to_collection[is.na(train$belongs_to_collection)] <- 0

#genre這個欄位也是json格是，是個很長的字串，一樣用正規表示法將字串分開
#先處理看看有一個電影會有幾個分類
genreCount <- str_count(train$genres, "\\}")
train$numberOfGenres <- genreCount
numberOfSplitCols <- max(na.omit(genreCount))
genreCount %>%
  as.data.frame(stringsAsFactors = F) %>%
  ggplot(aes(genreCount)) +
  geom_histogram(stat = "count")+
  scale_x_discrete(limits=1:numberOfSplitCols) +
  theme_light() +
  xlab("Number Of Genres Per Movie") +
  ylab("Number Of Movies") +
  ggtitle("Histogram Of Number Of Genres Per Movie")

#把多個分類先分開
genresSplit <- as.data.frame(str_split_fixed(train$genres, "\\}\\,\\s\\{", numberOfSplitCols), stringsAsFactors = F)
