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

#把genre的id跟名字拆出來之後，就要把它跟原先的train的資料集結合，然後把原本的grnre拿掉，col=4
train<-cbind(train,genreNames)
train<-train[,-4]

#我把train ID 跟分類名稱拿出來做成另外一個表格，使用melt函數把多變數變成少變數但比較長的資料集
#因為我想要看分類的收入平均跟中位數，我把revenue也加入新表格中，然後作圖看看個別的資料
install.packages("reshape2")
library(reshape2)
genreLongFormat <- cbind(train$id, genreNames)
genreLongFormat <- melt(genreLongFormat, id.vars = 'train$id') %>% 
  select(-variable) %>% 
  filter(value != "")
colnames(genreLongFormat) <- c("id", "value")
genreLongFormat <- left_join(genreLongFormat, train, by = "id")
genreLongFormat <- genreLongFormat %>% select(id, value, revenue)

#我要先計算平均數跟中位數
medians <- genreLongFormat %>% 
  group_by(value) %>% 
  summarise(median = median(revenue))

means <- genreLongFormat %>% 
  group_by(value) %>% 
  summarise(mean = mean(revenue))

#我繼續使用melt做資料集的轉換
central <- left_join(means, medians, by = "value")
centralLong <- melt(as.data.frame(central), id.vars = "value")
colnames(centralLong) <- c("genre", "central", "revenue")
ggplot(centralLong, 
       aes(x = reorder(genre, revenue), 
           y = revenue)) + 
  geom_bar(aes(fill = central), 
           stat = "identity", 
           position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("Revenue") +
  ggtitle("Mean and Median Revenue By Genre") +
  guides(fill=guide_legend(title="Measure"))

#cast part的抽取實在有點困難，我這邊是使用其他人的code來做清理
cast_list <- list()

for (i in seq_along(train$cast)) {
  cast_list[[i]] <- train$cast[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>%  #extract everything between {}
    str_split("(?<=[:digit:]|[:punct:]), ",
              n=Inf,simplify = TRUE) %>%       #split on ","
    str_extract_all('(?<=\\:[:space:]).*') %>% #get the part after the semicolon
    str_replace_all("'|\"","") %>% #clean the unwanted punctuation
    matrix( ncol = 8,  byrow = TRUE,dimnames=list(c(),
                                                  c("cast_id","character","credit_id","gender","id",
                                                    "name","order","profile_path"))) %>% #convert to matrix
    as_tibble(stringsAsFactors = FALSE)#convert the matrix to tibble
}

names(cast_list) <- c(1:3000)
cast_df <- bind_rows(cast_list, .id = 'movie_id')

#製作公司的抽取
production_companies_list <- list()
for (i in seq_along(train$production_companies)) {
  production_companies_list[[i]] <- train$production_companies[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>% 
    str_split("(?<=[:digit:]|[:punct:]), ",n=Inf,simplify = TRUE) %>% 
    str_extract_all('(?<=\\:[:space:]).*') %>% 
    str_replace_all("[:punct:]","") %>% 
    matrix( ncol = 2,  byrow = TRUE,dimnames=list(c(),
                                                  c("name","id"))) %>% 
    as_tibble(stringsAsFactors = FALSE)
}

names(production_companies_list) <- c(1:3000)
production_companies_df <- bind_rows(production_companies_list, .id = 'movie_id')

#團隊的抽取
crew_list <- list()

for (i in seq_along(train$crew)) {
  crew_list[[i]] <- train$crew[[i]] %>%
    str_extract_all('(?<=\\{).*?(?=\\})') %>% 
    str_split("(?<=[:digit:]|[:punct:]), ",n=Inf,simplify = TRUE) %>% 
    str_extract_all('(?<=\\:[:space:]).*') %>% 
    str_replace_all("[:punct:]","") %>% 
    matrix( ncol = 7,  byrow = TRUE,dimnames=list(c(),
                                                  c("credit_id","department","gender","id","job","name","profile_path"))) %>% 
    as_tibble(stringsAsFactors = FALSE)
}

names(crew_list) <- c(1:3000)
crew_df <- bind_rows(crew_list, .id = 'movie_id')


