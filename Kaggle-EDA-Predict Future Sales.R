setwd("C:/Users/ASUS/Desktop/Predict Future Sales")
sales_train<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/sales_train_v2.csv")
testdata<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/test.csv")
shop<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/shops.csv")
item<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/items.csv")
item_categories<-read.csv("C:/Users/ASUS/Desktop/Predict Future Sales/item_categories.csv")

#看一下各檔案的資料
head(sales_train)
head(testdata)
head(shop)
head(item)
head(item_categories)

#把資料串在一起!全部給他合起來
salesdata<-merge(sales_train,shop,by="shop_id")
salesdata2<-merge(salesdata,item,by="item_id")
salesdata3<-merge(salesdata2,item_categories,by="item_category_id")

#看看整體資料長怎樣
str(salesdata3)
summary(salesdata3)

#檢查有沒有遺失值
sum(is.na(salesdata3))

#從日期下手!這個日期形式跟一般認知不同，要先確認好日期是factor
str(salesdata3$date)

#下載日期整理套件，新增年月日與星期還有季的欄位
install.packages("lubridate")
library(lubridate)
install.packages("stringi")
library(stringi)
library(lubridate)
salesdata3$date<-as.Date(salesdata3$date,"%d.%m.%Y")


#看一下資料結構，把數值換成factor
str(salesdata3)
salesdata3$year<-year(salesdata3$date)
salesdata3$year<-as.factor(salesdata3$year)
salesdata3$month<-month(salesdata3$date)
salesdata3$month<-as.factor(salesdata3$month)
salesdata3$day<-day(salesdata3$date)
salesdata3$day<-as.factor(salesdata3$day)
salesdata3$weekday<-weekdays(salesdata3$date)
salesdata3$weekday<-as.factor(salesdata3$weekday)

#先來更新一下R的版本
installr::updateR(keep_install_file = TRUE)

#來看看有幾間分店，有60家分店阿!
install.packages("dplyr")
library(dplyr)
length(unique(salesdata3$shop_id))

#那看看哪家分店比較夯?是第31號店阿!!出現的頻率很高阿
sort(table(salesdata3$shop_id))

#先下載piping的套件
install.packages("magrittr")
library(magrittr)

#看看哪家分店的銷售比較好，賣了很多商品
total_sale<-


install.packages("ggplot2")
library(ggplot2)



