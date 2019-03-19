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
#看起來不妙，畫個圖清楚看看分配
install.packages("ggplot2")
library(ggplot2)
ggplot()