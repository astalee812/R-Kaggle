#先把資料讀進R裡面，要開始做EDA
setwd("C:/Users/ASUS/Desktop/house prices predict")
train<-read.csv("C:/Users/ASUS/Desktop/house prices predict/train.csv")
test<-read.csv("C:/Users/ASUS/Desktop/house prices predict/test.csv")

#把有遺失值的資料記錄起來，complete.case會檢查是否有遺漏值，前面加!把所有有遺漏值的資訊選取起來
#如果沒有加!，則會把所有有包含遺漏值的資訊刪除
missing_value<-train[!complete.cases(train),]
#我們有1460筆的資料有遺漏值，跟原始input的比數相同
nrow(missing_value)

#看看train的資料型態
str(train)
summary(train)

#來看一下SalePrice的分布是否為常態分配
summary(train$SalePrice)
#看起來不妙，畫個圖清楚看看分配，發現崩潰阿!是右偏分配阿
install.packages("ggplot2")
library(ggplot2)
ggplot(train,aes(x=SalePrice,fill=length(as.factor(train$Id))))+
  geom_histogram(binwidth = 5000)+
  ggtitle("Histogram of SalePrice")+
  xlab("House Price")+
  ylab("number of House")

#做線性回歸的一項大條件就是要為常態分配，所以我們要對我們的data做調整讓他變成常態分配
#我要把saleprice做成對數分配，在作圖時binwidth的數值也要改小，就會變成神奇的常態分配
train$logSalePrice<-log(train$SalePrice)
ggplot(train,aes(x=train$logSalePrice,fill=length(Id)))+
  geom_histogram(binwidth = 0.05)+
  ggtitle("Histogram of LogSalePrice")+
  xlab("Log term of House Price")+
  ylab("number of House")

#現在來找看看哪個數值變相跟slaeprice會最有關聯，這時候就要用到關係矩陣(correlation matrix)
#先找看看有幾個numeric variable
install.packages("dplyr")
library(dplyr)
install.packages("plyr")
library(plyr)
#先找出我們有幾個numeric variable,看來有39個
numericVar<-which(sapply(train,is.numeric))
#把numeric variable的資料抽取出來要做correlation
#cor的use寫pairwise.complete.obs，是做每筆的比對，同時比較兩行，只留下兩兩沒有NA的橫列
#若用use-everything就會留下NA
train_numVar<-train[,numericVar]
cor_train<-cor(train_numVar,use = "pairwise.complete.obs")




#現在要來處理資料中的dummy variable




