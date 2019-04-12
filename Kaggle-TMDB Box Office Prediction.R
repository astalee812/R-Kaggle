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
head(genresSplit, 5)
#把電影分類的ID用正規表示法抽取出來，變成一個data frame，然後變成numeric的形式
#若只用data frame做的話，沒辦法針對個別電影所分到的分類做成表格
#使用sapply會將輸出內容變成矩陣，不過在function部分就要自己寫一個
genreIds <- as.data.frame(sapply(genresSplit, function(x) str_extract(x, "(?<=id\\'\\:\\s{1})\\d{1,}")), stringsAsFactors = F)
genreIds <- as.data.frame(sapply(genreIds, function(x) as.numeric(x)), stringsAsFactors = F)
genreIds[is.na(genreIds)] <- ""
#把電影分類的名稱用正規表示法抽取出來，變成data frame
genreNames <- as.data.frame(sapply(genresSplit, function(x) str_extract(x, "(?<=name\\'\\:\\s{1}\\').+(?=\\')")), stringsAsFactors = F)
genreNames[is.na(genreNames)] <- ""

#現在有了分類名稱跟分類ID，可以來看一下電影分類的數量分布
#直接使用table會產生多項量的計算，但我想要全部加總，使用length只會回傳7，用看看unlist
#unlsit可以將向量list的東西打散變成一個一個的資料，之後還是要把她變成data frame的格式才可以使用
gs<-as.data.frame(table(unlist(genreNames)))
#來畫個圖，但我要把第一個去掉，因為是空白
nrow(gs)
gs<-gs[2:21,]
#畫圖時間，我把圖形翻轉，因為直條圖看起來很不友善，在數據部分我也重新排序
ggplot(data=gs, aes(x = reorder(Var1, Freq),
                    y = Freq,
                    fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number Of Movies") +
  ggtitle("Number of Movies Containing Specific Tag") +
  theme(legend.position="none")

