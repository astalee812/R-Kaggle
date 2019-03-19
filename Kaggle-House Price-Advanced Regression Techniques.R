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




