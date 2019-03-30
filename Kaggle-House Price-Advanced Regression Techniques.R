#先把資料讀進R裡面，要開始做EDA
setwd("C:/Users/ASUS/Desktop/house prices predict")
train<-read.csv("C:/Users/ASUS/Desktop/house prices predict/train.csv",stringsAsFactors = FALSE)
test<-read.csv("C:/Users/ASUS/Desktop/house prices predict/test.csv",stringsAsFactors = FALSE)
#發現test資料少一欄saleprice，把它塞進去
test$SalePrice<-NA
all<-rbind(train,test)
all1<-rbind(train,test)
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
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC[is.na(all$PoolQC)]<-"None"
all$PoolQC<-as.integer(revalue(all$PoolQC,Qualities))

#2 MiscFeature:雜項功能，不需要計分，並非等級的概念
table(all$MiscFeature)
all$MiscFeature[is.na(all$MiscFeature)]<-"None"
all$MiscFeature<-as.factor(all$MiscFeature)

#畫圖來了解一下有這些其他功能跟售價會不會有關係，不過這個變相跟預測房價好像沒啥關係
all$MiscFeature<-as.factor(all$MiscFeature)
ggplot(all,aes(x=MiscFeature,y=SalePrice))+
  geom_bar(stat = "summary")+
  geom_label(stat = "count",aes(label=..count..,y=..count..))

#3 Aelly: Grvl=Gravel,Pave=Paved,NA=no alley，不需要計分，並非等級的概念
all$Alley[is.na(all$Alley)]="None"
table(all$Alley)
all$Alley<-as.factor(all$Alley)

#4 Fence:圍欄質量，不需要計分，並非等級的概念，GdPrv=good privacy,MnPrv=minimun privacy,GdWo=good wood,MnWw=minimun wood/wire,NA=no fence
all$Fence[is.na(all$Fence)] <- "None"
table(all$Fence)
all$Fence<-as.factor(all$Fence)

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
all$LotFrontage<as.integer(all$LotFrontage)
#LotShape要轉換成數值
all$LotShape<-as.integer(revalue(all$LotShape,c("Reg"=3,"IR1"=2,"IR2"=1,"IR3"=0)))
table(all$LotShape)
#LotConfig 要轉換成factor
all$LotConfig <- as.factor(all$LotConfig)

#6 GarageYtBlt: 車庫的興建年分，NA可以看成他沒有車庫，同樣的這159個資料有關車庫的都會是NA，要檢查是否為等級的變相
all$GarageYrBlt[is.na(all$GarageYrBlt)]<-0
#GarageFinish
all$GarageFinish[is.na(all$GarageFinish)]<-"None"
finish<-c("Fin"=3,"RFn"=2,"Unf"=1,"None"=0)
all$GarageFinish<-as.integer(revalue(all$GarageFinish,finish))
table(all$GarageFinish)
#GarageQual
all$GarageQual[is.na(all$GarageQual)]<-"None"
all$GarageQual<-as.integer(revalue(all$GarageQual,Qualities))
table(all$GarageQual)
#GarageCond
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
all$GarageType<-as.factor(all$GarageType)


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
#BsmtQual
table(all$BsmtQual)
all$BsmtQual[is.na(all$BsmtQual)]<-"None"
all$BsmtQual<-as.integer(revalue(all$BsmtQual,Qualities))
#BsmtExposure
table(all$BsmtExposure)
all$BsmtExposure[is.na(all$BsmtExposure)]<-"None"
exposure<-c("Gd"=4,"Av"=3,"Mn"=2,"No"=1,"None"=0)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure,exposure))
#BsmtFinType1
table(all$BsmtFinType1)
all$BsmtFinType1[is.na(all$BsmtFinType1)]<-"None"
fintype<-c("GLQ"=6,"ALQ"=5,"BLQ"=4,"Rec"=3,"LwQ"=2,"Unf"=1,"None"=0)
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1,fintype))
#BsmtFinType2
table(all$BsmtFinType2)
all$BsmtFinType2[is.na(all$BsmtFinType2)]<-"None"
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2,fintype))
#BsmtFullBath
which(is.na(all$BsmtFullBath))
all$BsmtFullBath[is.na(all$BsmtFullBath)]<-0
table(all$BsmtFullBath)
#BsmtHalfBath
which(is.na(all$BsmtHalfBath))
all$BsmtHalfBath[is.na(all$BsmtHalfBath)]<-0
table(all$BsmtHalfBath)
#BsmtFinSF1
which(is.na(all$BsmtFinSF1))
all$BsmtFinSF1[is.na(all$BsmtFinSF1)]<-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)]<-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)]<-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)]<-0

#8 Masvnrtype:我不知道這是啥，某種貼磁磚的，但MasVnrType應該要與MasVnrArea相同，來看看發生了什麼事情
length(which(is.na(all$MasVnrType)&is.na(all$MasVnrArea)))
all[is.na(all$MasVnrType)&!is.na(all$MasVnrArea),c("MasVnrType","MasVnrArea")]
#針對2611列做調整，選取第二個Brkface做填入
sort(table(all$MasVnrType),decreasing = TRUE)
all$MasVnrType[2611]<-"BrkFace"
#MasVnrType是有分等級的!用價格的中位數來看
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
install.packages("magrittr")
library(magrittr)
all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice)) %>% arrange(median)
#發現none比brkcmn的價格還高，我們來看一下是不是交易比數太少的關係
all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)
#確定是因為brkcmn交易比數太少的關係，開始做轉換數值
Masonry <- c("None"=0, "BrkCmn"=0, "BrkFace"=1, "Stone"=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType,Masonry))
table(all$MasVnrType)

all$MasVnrArea[is.na(all$MasVnrArea)]<-0

#9  MSZoning: 分區的變相，不用做數字轉換
sort(table(all$MSZoning),decreasing = TRUE)
all$MSZoning[is.na(all$MSZoning)]<-"RL"
table(all$MSZoning)
all$MSZoning<-as.factor(all$MSZoning)

#10 Utilities 設備
table(all$Utilities)
which(is.na(all$Utilities))
all$Utilities[is.na(all$Utilities)]<-"AllPub"
all$Utilities<-as.factor(all$Utilities)

#11 Functional 房屋功能評估，一看就知道是等級的，要乖乖轉換數值
table(all$Functional)
sort(table(all$Functional),decreasing = TRUE)
all$Functional[is.na(all$Functional)]<-"Typ"
fuction<-c("Typ"=7,"Min1"=6,"Min2"=5,"Mod"=4,"Maj1"=3,"Maj2"=2,"Sev"=1,"Sal"=0)
all$Functional<-as.integer(revalue(all$Functional,fuction))

#12 Exterior1 外牆相關變相
sort(table(all$Exterior1st),decreasing = TRUE)
all$Exterior1st[is.na(all$Exterior1st)]<-"VinylSd"
all$Exterior1st<-as.factor(all$Exterior1st)
#Exterior2 外牆材料2
sort(table(all$Exterior2nd),decreasing = TRUE)
all$Exterior2nd[is.na(all$Exterior2nd)]<-"VinylSd"
all$Exterior2nd<-as.factor(all$Exterior2nd)
#ExterQual 外牆品質，要換數值
all$ExterQual<-as.integer(revalue(all$ExterQual,Qualities))
table(all$ExterQual)
#ExterCond 外牆狀況，要換數值
all$ExterCond<-as.integer(revalue(all$ExterCond,Qualities))
table(all$ExterCond)

#13 Electrical 電子系統
sort(table(all$Electrical),decreasing = TRUE)
all$Electrical[is.na(all$Electrical)]<-"SBrkr"
all$Electrical<-as.factor(all$Electrical)
table(all$Electrical)

#14 KitchenQual 廚房品質
sort(table(all$KitchenQual),decreasing = TRUE)
all$KitchenQual[is.na(all$KitchenQual)]<-"TA"
all$KitchenQual<-as.integer(revalue(all$KitchenQual,Qualities))
table(all$KitchenQual)

#15 Saletype
sort(table(all$SaleType),decreasing = TRUE)
all$SaleType[is.na(all$SaleType)]<-"WD"
all$SaleType<-as.factor(all$SaleType)
all$SaleCondition<-as.factor(all$SaleCondition)

#來看一下資料的型態，看看我們有沒有漏掉什麼，還有15個變相阿.....
str(all)
Charcol<-names(all[,sapply(all,is.character)])
Charcol
#LandContour
all$LandContour <- as.factor(all$LandContour)
#LandSlope
table(all$LandSlope)
all$LandSlope<-as.integer(revalue(all$LandSlope, c("Gtl"=2,"Mod"=1,"Sev"=0)))
#Roofstyle
all$RoofStyle <- as.factor(all$RoofStyle)
#RoofMatl
all$RoofMatl <- as.factor(all$RoofMatl)
#Foundation
all$Foundation<-as.factor(all$Foundation)
#Heating
all$Heating <- as.factor(all$Heating)
#HeatingQC，這傢伙要轉換數值
table(all$HeatingQC)
all$HeatingQC<-as.integer(revalue(all$HeatingQC,Qualities))
#CentralAir，轉換數值
table(all$CentralAir)
all$CentralAir<-as.integer(revalue(all$CentralAir,c("Y"=1,"N"=0)))
#street
table(all$Street)
all$Street<-as.integer(revalue(all$Street, c("Pave"=1,"Grvl"=0)))
#Neighborhood
table(all$Neighborhood)
all$Neighborhood<-as.factor(all$Neighborhood)
#condition1
table(all$Condition1)
all$Condition1<-as.factor(all$Condition1)
#condition2
table(all$Condition2)
all$Condition2<-as.factor(all$Condition2)
#BldgType
table(all$BldgType)
all$BldgType<-as.factor(all$BldgType)
#housetype
table(all$HouseStyle)
all$HouseStyle<-as.factor(all$HouseStyle)
#PavedDrive
table(all$PavedDrive)
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c("Y"=2,"P"=1,"N"=0)))

all$MoSold<-as.factor(all$MoSold)
all$MoSold<-as.factor(all$YrSold)
all$MSSubClass<-as.factor(all$MSSubClass)
table(all$MSSubClass)
mssubclassname<-c("20"="1-STORY 1946 & NEWER ALL STYLES","30"="1-STORY 1945 & OLDER","40"="1-STORY W/FINISHED ATTIC ALL AGES",
                  "45"="1-1/2 STORY - UNFINISHED ALL AGES","50"="1-1/2 STORY FINISHED ALL AGES","60"="2-STORY 1946 & NEWER",
                  "70"="2-STORY 1945 & OLDER","75"="2-1/2 STORY ALL AGES","80"="SPLIT OR MULTI-LEVEL","85"="SPLIT FOYER",
                  "90"="DUPLEX - ALL STYLES AND AGES","120"="1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
                  "150"="1-1/2 STORY PUD - ALL AGES","160"="2-STORY PUD - 1946 & NEWER","180"="PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
                  "190"="2 FAMILY CONVERSION - ALL STYLES AND AGES")
all$MSSubClass<-revalue(all$MSSubClass,mssubclassname)
str(all$MSSubClass)

#來看一下整體資料的狀況，我有56個數值變數跟25個類別變數
length(which(sapply(all, is.numeric)))
numericVars <- which(sapply(all, is.numeric))
length(which(sapply(all, is.factor)))
factorVars <- which(sapply(all, is.factor))

#針對numeric再做一次迴歸分析
all_numVar<-all[,numericVars]
cor_numVar<-cor(all_numVar,use = "pairwise.complete.obs")
cor_sorted<-as.matrix(sort(cor_numVar[,"SalePrice"],decreasing = TRUE))
cor_sorted
corrplot.mixed(cor_numVar,to.col="black",tl.pos = "lt")
name1<-c("OverallQual","GrLivArea","ExterQual","KitchenQual","GarageCars","GarageArea","TotalBsmtSF","X1stFlrSF","BsmtQual","FullBath",
"GarageFinish","TotRmsAbvGrd","YearBuilt","FireplaceQu","YearRemodAdd")
cor_numVar2<-cor_numVar[name1,name1]
corrplot.mixed(cor_numVar2,tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

#有些數值可以互相加總
#total number of bathrooms
all$TotalBathrooms<-all$FullBath+(all$HalfBath*0.5)+all$BsmtFullBath+(all$BsmtHalfBath*0.5)
install.packages("Scales")
library(scales)
#看看totalbathroom跟房價之間的關係
ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(TotalBathrooms), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#看看totalbathroom跟交易量之間的關係
ggplot(data=all, aes(x=as.factor(TotalBathrooms))) +
  geom_histogram(stat='count')

#house remodeled(0=沒重新裝修，1=有重新裝修)
all$Remod<-ifelse(all$YearBuilt==all$YearRemodAdd,0,1)
all$Age<-as.numeric(all$YrSold)-all$YearRemodAdd
ggplot(data=all[!is.na(all$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#看一下屋齡跟房價之間的關係，correlation=-0.5090787，屋齡愈高房價越低
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])
#重新裝潢跟黃價之間的關係，correlation=-0.0219326，有重新裝潢過的房子房價會比較低
cor(all$SalePrice[!is.na(all$SalePrice)], all$Remod[!is.na(all$SalePrice)])
#新房子!!新房子跟房價的關係，correlation=0.2248073，新房子房價比較高
all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)
cor(all$SalePrice[!is.na(all$SalePrice)], all$IsNew[!is.na(all$SalePrice)])

#社區鄰居，把社區鄰居依照房價分群，可以看到有三個社區房價特高，那我也挑選三個社區房價低的
arrange(aggregate(SalePrice~Neighborhood,all,FUN = "mean"),SalePrice)
arrange(aggregate(SalePrice~Neighborhood,all,FUN = "median"),SalePrice)
#%in%:包含
all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(all$NeighRich)
#total square feet，correlation between saleprice & total aquare feet is 0.7668127
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF
cor(all$SalePrice, all$TotalSqFeet, use= "pairwise.complete.obs")
#total porch square feet，correlation between saleprice & total porch square feet is 0.192567
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
cor(all$SalePrice, all$TotalPorchSF, use= "pairwise.complete.obs")
#remove outlier
all <- all[-c(524, 1299),]

#找出真正的numeric variable
all$SalePrice<-as.numeric(all$SalePrice)
numericVars <- which(sapply(all, is.numeric))
numericVarNames <- names(numericVars)
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))]
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))
DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

length(DFnumeric)
length(DFfactors)


#要來修正一下所有數值變相的偏態(只要偏態係數>0.8就要做對數調整)
install.packages("psych")
library(psych)
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}
#將所有的資料轉成可用的資料
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

#要把dummy variable做處理，使用model.matrix做轉換，要注意資料集全部都要是factor
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
#找出那些變相再test資料集中根本沒出現的，然後刪除這些變相
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest]
#找出那些變相再train資料集中根本沒出現的，然後刪除這些變相
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]
#刪除資料中只出現10個以下的變相
fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes]
dim(DFdummies)
#把dummy跟數值變相的資料和在一起
combined <- cbind(DFnorm, DFdummies)

#看一下saleprice的偏態，偏態係數=1.877427，要做對數調整
skew(all$SalePrice)
all$SalePrice <- log(all$SalePrice)
skew(all$SalePrice)
#接下來要把資料分成訓練集跟測試集
train1<-combined[!is.na(all$SalePrice),]
test1<-combined[is.na(all$SalePrice),]

#Lasso regression model:先設定隨機數，然後開始製作參數
set.seed(27042018)
#traincontrol函數產生的參數是為了要去控制用可能的值去建立模型
#method表示抽樣方法，number表示跌代次數或者交叉驗證的數目
my_control <-trainControl(method="cv", number=5)
#做交叉驗證，列出所有可能性的列表
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
#建立lasso模型
lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
#把lasso模型中的最小RMSE抓出來，看看模型的效果如何
min(lasso_mod$results$RMSE)
#VarImp是用來計算特徵重要性
lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance
#選取重要的特徵
varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
#做出預測!!然後把預測的結果撈出來看一下
LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred)
head(predictions_lasso)

write.csv(predictions_lasso,file = "predition_lasso.csv")

