setwd("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales")
sales_train<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales/sales_train_v2.csv")
testdata<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales/test.csv")
shop<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales/shops.csv")
item<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales/items.csv")
item_categories<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/competitive-data-science-predict-future-sales/item_categories.csv")

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
salesdata3$year<-as.integer(salesdata3$year)
salesdata3$month<-month(salesdata3$date)
salesdata3$month<-as.integer(salesdata3$month)
salesdata3$day<-day(salesdata3$date)
salesdata3$day<-as.integer(salesdata3$day)
salesdata3$weekday<-weekdays(salesdata3$date)
salesdata3$weekday<-as.factor(salesdata3$weekday)
salesdata3$weekday<-as.integer(salesdata3$weekday)

#先來更新一下R的版本
installr::updateR(keep_install_file = TRUE)

#來看看有幾間分店，有60家分店阿!
install.packages("dplyr")
require(dplyr)

#這邊發現dplyr一直裝不好，重新裝一次
install.packages("dplyr", dependencies=TRUE)
length(unique(salesdata3$shop_id))

#那看看哪家分店比較夯?是第31號店阿!!出現的頻率很高阿
sort(table(salesdata3$shop_id))

#先下載piping的套件
install.packages("magrittr")
library(magrittr)

#看看哪家分店的銷售比較好，賣了很多商品，發現一樣是31號店賣出的東西最多
total_sale<-salesdata3%>%group_by(shop_id,shop_name)%>%summarise(total_qty=sum(item_cnt_day))%>%
  arrange(desc(total_qty))

#用張圖來表示一下分店銷售排行
install.packages("ggplot2")
library(ggplot2)
#ace=aesthetic mappings，把素材綁到X跟Y軸，要畫長條圖必須要加stat = "identity"，不然會畫不出來
#colour是線的顏色，fill是填滿的顏色
ggplot(total_sale,aes(x=as.factor(shop_id),y=total_qty,fill=as.factor(shop_id)))+geom_bar(stat = "identity")+
  labs(title = "Most popular shop by quantity volume",x="shop",y="quantity volume",fill="shop_id")

#計算共有幾個商品項目，共有21807個商品
length(unique(salesdata3$item_id))

#哪個商品是被賣出最多次的，n()=the number of observations within a group，ungroup=Return data to non-grouped.
item_freq<-salesdata3%>%group_by(item_id,item_name)%>%summarise(freq=n())%>%ungroup()%>%arrange(desc(freq))

#來看看每一家分店賣最好的東西分別是什麼
popular_item<-salesdata3%>%group_by(shop_id,item_id)%>%summarise(quantity_sold=sum(item_cnt_day))%>%
  filter(quantity_sold==max(quantity_sold))%>%arrange(desc(quantity_sold))

#來看看我們有多少品類
length(unique(salesdata3$item_category_id))

#每家分店分別有多少品類，並畫圖
category_shop<-salesdata3%>%group_by(shop_id)%>%distinct(item_category_id)%>%summarise(category_n=n())
ggplot(category_shop,aes(x=as.factor(category_shop$shop_id),y=category_shop$category_n),fill=as.factor(category_shop$shop_id))+
  geom_bar(stat = "identity")+
  labs(title = "Category in shop",x="shop",y="number of category",fill="shop_id")

#哪個品類被賣出的頻率最高，25的品類賣出頻率很高
category_frq_shop<-salesdata3%>%group_by(shop_id)%>%summarise(count=n_distinct(item_category_id))%>%
  arrange(desc(count))

#看看哪個品類貢獻度最高，是40的品類貢獻最多
category_sale<-salesdata3%>%group_by(item_category_id)%>%summarise(total_qty=sum(item_cnt_day))%>%
  arrange(desc(total_qty))

#看看每個月的銷售PIC數，畫個圖
monthly_sale<-salesdata3%>%group_by(month)%>%summarise(total_qty=sum(item_cnt_day))
ggplot(monthly_sale,aes(x = as.factor(month), y = total_qty, fill =as.factor(month)))+
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total unit sales', x = 'Month', title = 'Total Sales by Month')

#看看星期幾的銷售PIC比較好，畫個圖
weekday_sale<-salesdata3%>%group_by(weekday)%>%summarise(total_qty=sum(item_cnt_day))
ggplot(weekday_sale,aes(x = as.factor(weekday), y = total_qty, fill =as.factor(weekday)))+
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total unit sales', x = 'Weekday', title = 'Total Sales by Weekday')

#看看整體的時間趨勢並畫圖
salesdata3 %>%
  ggplot(aes(date)) +
  geom_freqpoly(color = "blue", binwidth = 10, size = 1.2)

#算每日銷售囉
Total_Day_sale<-salesdata3%>%group_by(day)%>%summarise(total_s=sum(item_cnt_day*item_price))
ggplot(Total_Day_sale,aes(x = as.factor(day), y = total_s, fill =as.factor(day)))+
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total unit sales', x = 'day', title = 'Total Sales by day')

#做個時間序列圖，依照每家店每個月的銷售PIC
shop_sale<-salesdata3%>%group_by(shop_id,month)%>%summarise(total_qty=sum(item_cnt_day))%>%arrange(month)
install.packages("timeSeries")
install.packages("timeDate")
library(timeDate)
library(timeSeries)
ts_mon_sales<-ts(shop_sale$total_qty, start =c(2013,1), end =c(2015,10), frequency= 12)
plot(ts_mon_sales)
#-----------------------------------------針對test data相同的資料整理--------------------------------------
salesdata_T<-merge(testdata,shop,by="shop_id")
salesdata_T2<-merge(salesdata_T,item,by="item_id")
salesdata_T3<-merge(salesdata_T2,item_categories,by="item_category_id")

#-----------------------------------------結束EDA----------------------------------------------------------

#開始來做預測by商家
install.packages("forecast")
library(forecast)
ts_mon_sales_model<-auto.arima(ts_mon_sales)
ts_mon_sales_model
#預測未來五個月的銷售狀況
predict_sales<- predict(ts_mon_sales_model ,n.ahead = 5, se.fit=T)
predict_sales
forecast_sales<- forecast(object=ts_mon_sales_model, h=5)
forecast_sales
plot(forecast_sales)
acf(forecast_sales$residuals, lag.max=20)
plot.ts(forecast_sales$residuals)
#預測by商品
shop_item_mon_sales<- salesdata3%>% group_by(item_id,month)%>% summarise(items_sold = sum(item_cnt_day), total_sales = sum(item_price*item_cnt_day))%>% ungroup()%>% arrange(desc(items_sold))
ts_shop_item_mon_sales<- ts(shop_item_mon_sales$items_sold, start =c(2013,1), end =c(2015,10), frequency= 12)
plot(ts_shop_item_mon_sales)
ts_shop_item_mon_sales_model<- auto.arima(ts_shop_item_mon_sales)
ts_shop_item_mon_sales_model
#預測未來五個月的銷售狀況
predict_ID_sales<- predict(ts_shop_item_mon_sales_model ,n.ahead = 5, se.fit=T)
predict_ID_sales
forecast_ID_sales<- forecast(object=ts_shop_item_mon_sales_model, h=5)
forecast_ID_sales
plot(forecast_ID_sales)

#--------------------------------------------做出了預測但不是比賽要的格式------------------------------------------------------------------------------------------------
install.packages("xgboost")
library(xgboost)
salesdata4<-salesdata3[,c(-8:-10)]
str(salesdata4)
salesdata4$item_category_id<-as.factor(salesdata4$item_category_id)
salesdata4$item_id<-as.factor(salesdata4$item_id)
salesdata4$shop_id<-as.factor(salesdata4$shop_id)
salesdata4$totalsale<-salesdata4$item_price*salesdata4$item_cnt_day

#salematrix的欄位名稱要依樣之外，連順序都要一樣
salematrix<-data.matrix(salesdata4[,c(3,2)])#將自變數轉化為矩陣
library(Matrix)
saledata_t<-Matrix(salematrix,sparse=T) #利用Matrix函数，將sparse參數設置TRUE，轉化為稀疏矩陣
saledate_d<-data.matrix(salesdata4[,7]) #將因變數轉化成矩陣
salesdata5<-list(data=saledata_t,label=saledate_d) #自變數跟因變數變成一個list
dtrain<-xgb.DMatrix(salesdata5$data,label=salesdata5$label) #建構模型需要的xgb.DMatrix對象,處理對象為稀疏矩陣

#----------------------------設定Xgboost參數-------------------------
library(xgboost)
testdata1 <- data.matrix(testdata[,c(2:3)])
dtest <- xgb.DMatrix(data = testdata1)
param<-list(max_depth=15,
            eta=0.03,
            eval_metric = "error"
            )
model<-xgboost(params = param,data = dtrain,nrounds = 15)

#---------------------------調整參數---------------------------------

model<-xgboost(data = dtrain,
               colsample_bytree = 0.5, 
               subsample = 0.5, 
               eta = 0.05,
               max_depth=3,
               nrounds = 15,
               eval_metric = "rmse",
               gamma=0,
               seed= 42
)

param2<-list(colsample_bytree = 0.5, 
             subsample = 0.5, 
             eta = 0.05,
             max_depth=3,
             nrounds = 15,
             eval_metric = "error",
             gamma=0,
             seed= 42
)

#為了找出最佳nround
cv.model = xgb.cv(
  params = param2, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=100,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 20, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
) 


tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_error_mean, col='red', xlab="nround", ylab="error", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$train_error_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )






#---------------------做預測-------------------------------
pred <- predict(model,dtest)
sub<-data.frame(ID=testdata$ID,item_cnt_month =pred)

write.csv(sub,"sub.csv",row.names = F)


