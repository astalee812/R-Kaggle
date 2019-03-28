#先把資料讀進R裡面，要開始做EDA
setwd("C:/Users/ASUS/Desktop/house prices predict")
train<-read.csv("C:/Users/ASUS/Desktop/house prices predict/train.csv",stringsAsFactors = FALSE)
test<-read.csv("C:/Users/ASUS/Desktop/house prices predict/test.csv",stringsAsFactors = FALSE)
#發現test資料少一欄saleprice，把它塞進去
test$SalePrice<-NA
all<-rbind(train,test)

#把有遺失值的資料記錄起來，complete.case會檢查是否有遺漏值，前面加!把所有有遺漏值的資訊選取起來
#如果沒有加!，則會把所有有包含遺漏值的資訊刪除
missing_value<-all[!complete.cases(all),]
#我們有2919筆的資料有遺漏值，跟原始input的比數相同
nrow(missing_value)

#看看all的資料型態
str(all)
summary(all)

#來看一下SalePrice的分布是否為常態分配
summary(all$SalePrice)
#看起來不妙，畫個圖清楚看看分配，發現崩潰阿!是右偏分配阿
install.packages("ggplot2")
library(ggplot2)
ggplot(all,aes(x=SalePrice,fill=length(as.factor(all$Id))))+
  geom_histogram(binwidth = 5000)+
  ggtitle("Histogram of SalePrice")+
  xlab("House Price")+
  ylab("number of House")

#做線性回歸的一項大條件就是要為常態分配，所以我們要對我們的data做調整讓他變成常態分配
#我要把saleprice做成對數分配，在作圖時binwidth的數值也要改小，就會變成神奇的常態分配
all$logSalePrice<-log(all$SalePrice)
ggplot(all,aes(x=all$logSalePrice,fill=length(Id)))+
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
numericVar<-which(sapply(all,is.numeric))
#把numeric variable的資料抽取出來要做correlation
#cor的use寫pairwise.complete.obs，是做每筆的比對，同時比較兩行，只留下兩兩沒有NA的橫列
#若用use-everything就會留下NA
install.packages("corrplot")
library(corrplot)
all_numVar<-all[,numericVar]
cor_all<-cor(all_numVar,use = "pairwise.complete.obs")
#畫圖時間，結果...太多參數好亂
cor_all_matrix<-corrplot.mixed(cor_all,to.col="black",tl.pos = "lt")
#擷取一下corrlation>0.5的變數出來，並且畫成一張圖
cor_sort<-as.matrix(sort(cor_all[,"SalePrice"],decreasing = TRUE))
all_numVar2<-all_numVar[,c("SalePrice","OverallQual","GrLivArea","GarageCars","GarageArea","TotalBsmtSF",
                               "X1stFlrSF","FullBath","TotRmsAbvGrd","YearBuilt","YearRemodAdd")]
cor_all2<-cor(all_numVar2,use = "pairwise.complete.obs")
cor_all_matrix<-corrplot.mixed(cor_all2,tl.col="black",tl.pos = "lt")

#現在要來處理資料中的dummy variable，漫長的填滿因素，先來看看我們有多少要填補吧
#因為test資料沒有saleprice跟logsaleprice，所以我們有34個變相要處理
nacl<-which(colSums(is.na(all))>0)
sort(colSums(sapply(all[,nacl], is.na)),decreasing = TRUE)
length(sort(colSums(sapply(all[,nacl], is.na)),decreasing = TRUE))

#1 PoolQC:游泳池的品質，EX=Excellent,Gd=Good,TA=Average,FA=fair,NA=nopool，我需要把這些變成數值
table(all$PoolQC)
all$PoolQC<-ifelse(all$PoolQC=="Ex",5,ifelse(all$PoolQC=="Gd",4,ifelse(all$PoolQC=="Ta",3,ifelse(all$PoolQC=="Fa",2,all$PoolQC))))
all$PoolQC<-ifelse(is.na(all$PoolQC),"None",all$PoolQC)

#2 MiscFeature:雜項功能，不需要計分，並非等級的概念
table(all$MiscFeature)
all$MiscFeature<-ifelse(is.na(all$MiscFeature),"None",all$MiscFeature)
#畫圖來了解一下有這些其他功能跟售價會不會有關係，不過這個變相跟預測房價好像沒啥關係
all$MiscFeature<-as.factor(all$MiscFeature)
ggplot(all,aes(x=MiscFeature,y=SalePrice))+
  geom_bar(stat = "summary")+
  geom_label(stat = "count",aes(label=..count..,y=..count..))

#3 Aelly: Grvl=Gravel,Pave=Paved,NA=no alley，不需要計分，並非等級的概念
all$Alley[is.na(all$Alley)]="None"
table(all$Alley)

#4 Fence:圍欄質量，不需要計分，並非等級的概念，GdPrv=good privacy,MnPrv=minimun privacy,GdWo=good wood,MnWw=minimun wood/wire,NA=no fence
all$Fence[is.na(all$Fence)] <- "None"
table(all$Fence)

#5 FireplaceQu: 防火的數量，他是一個等級的概念，所以我要把它轉換為數值
#Ex=Excellent，Gd=good，TA=Average，Fa=Fair，Po=poor，NA=no fireplace
all$FireplaceQu[is.na(all$FireplaceQu)]<-"None"
table(all$FireplaceQu)
Qualities
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu,Qualities))
table(all$FireplaceQu)

#5 LotFrontage: NA=0，我其實看不太懂這個變相
all$LotFrontage[is.na(all$LotFrontage)]<-0
table(all$LotFrontage)

#6 GarageYtBlt: 車庫的興建年分，NA可以看成他沒有車庫，同樣的這159個資料有關車庫的都會是NA，要檢查是否為等級的變相
all$GarageYrBlt[is.na(all$GarageYrBlt)]<-0

all$GarageFinish[is.na(all$GarageFinish)]<-"None"
finish<-c("Fin"=3,"RFn"=2,"Unf"=1,"None"=0)
all$GarageFinish<-as.integer(revalue(all$GarageFinish,finish))
table(all$GarageFinish)

all$GarageQual[is.na(all$GarageQual)]<-"None"
all$GarageQual<-as.integer(revalue(all$GarageQual,Qualities))
table(all$GarageQual)

all$GarageCond[is.na(all$GarageCond)]<-"None"
all$GarageCond<-as.integer(revalue(all$GarageCond,Qualities))
table(all$GarageCond)

#這邊有點怪怪的，garagetype有157個，但應該要有159個，而garagecars.graragearea都只有1個NA，還都是2577列
all$GarageType[is.na(all$GarageType)]<-"None"
which(is.na(all$GarageCars))
which(is.na(all$GarageArea))
#把2577列的資料叫出來，發現在車庫類型上市detchd，所以我需要調整一下2577列的資料
all[2577,]
all$GarageType[2577]<-"None"
all$GarageCars[2577]<-0
all$GarageArea[2577]<-0

#7 地下室相關變相，這邊也是很詭異，NA值都不太一樣，先找出都是NA的正常地下室數值，一共有79個沒地下室的
length(which(is.na(all$BsmtQual)&is.na(all$BsmtCond)&is.na(all$BsmtExposure)&is.na(all$BsmtFinType1)&is.na(all$BsmtFinType2)))
#現在要找出錯誤的NA值，使用條件有na都抓出來，|=或
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
#因為這些數值看來都是有地下室的，而地下室的其他數值就沒有填補完成，用該變相中最常出現的資料填補進去
#第333列的bsmtfintype2處理
sort(table(all$BsmtFinType2),decreasing = TRUE)
all$BsmtFinType2[333]<-"Unf"
#第949,1488,2349列的bsmtexposue
sort(table(all$BsmtExposure),decreasing = TRUE)
all$BsmtExposure[c(949,1488,2349)]<-"No"
#第2041,2186,2525的bsmtcond
sort(table(all$BsmtCond),decreasing = TRUE)
all$BsmtCond[c(2041,2186,2525)]<-"TA"
#第2218,2219的bsmtqual
sort(table(all$BsmtQual),decreasing = TRUE)
all$BsmtQual[c(2218,2219)]<-"TA"

#處理完畢之後我們來處理等級傳換成數字
table(all$BsmtCond)
all$BsmtCond[is.na(all$BsmtCond)]<-"None"
all$BsmtCond<-as.integer(revalue(all$BsmtCond,Qualities))

table(all$BsmtQual)
all$BsmtQual[is.na(all$BsmtQual)]<-"None"
all$BsmtQual<-as.integer(revalue(all$BsmtQual,Qualities))

table(all$BsmtExposure)
all$BsmtExposure[is.na(all$BsmtExposure)]<-"None"
exposure<-c("Gd"=4,"Av"=3,"Mn"=2,"No"=1,"None"=0)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure,exposure))

table(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)]<-"None"
fintype<-c("GLQ"=6,"ALQ"=5,"BLQ"=4,"Rec"=3,"LwQ"=2,"Unf"=1,"None"=0)
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1,fintype))

table(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)]<-"None"
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2,fintype))

which(is.na(all$BsmtFullBath))
all$BsmtFullBath[is.na(all$BsmtFullBath)]<-0
table(all$BsmtFullBath)

which(is.na(all$BsmtHalfBath))
all$BsmtHalfBath[is.na(all$BsmtHalfBath)]<-0
table(all$BsmtHalfBath)

which(is.na(all$BsmtFinSF1))
all$BsmtFinSF1[is.na(all$BsmtFinSF1)]<-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)]<-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)]<-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)]<-0