Train<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/titanic/train.csv")
Test<-read.csv("C:/Users/ASUS/Desktop/Asta/Kaggle/titanic/test.csv")

#檢查一下有沒有NA值，會發現ge部分爆炸多NA值
colSums(is.na(full))

#處理NA值的時候到了
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)
full$Embarked[full$Embarked==""]="C"

#train跟test的欄位不同，無法用rbind做合併，則使用bind_rows做合併
install.packages("dplyr")
library(dplyr)
full<-bind_rows(Train, Test)

#再檢查一次NA值
colSums(is.na(full))

#了解一下合併的資料長怎樣
str(full)
summary(full)

#設定factor
full$Survived=as.factor(full$Survived)
full$Sex=as.factor(full$Sex)
full$Pclass=as.factor(full$Pclass)
full$Embarked=as.factor(full$Embarked)

str(full)
update.packages()
install.packages("devtools")
devtools::install_github("tidyverse/ggplot2")

#畫圖時間
install.packages("ggplot2")
library(ggplot2)

#看看性別跟生存與否之間有沒有關係，看來快男人死光光
LT=dim(Train)[1]
ggplot(data = full[1:LT,],aes(x=Sex,fill=Survived))+geom_bar()

#看看Embarked跟生存與否之間有沒有關係
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

#看看Pclss跟生存與否之間有沒有關係，等級越高生存率越高
ggplot(data = full[1:LT,],aes(x=Pclass,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")

#看看Pclss跟Embarked與生存與否之間有沒有關係
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+facet_wrap(~Pclass)

#看看年紀跟生存會有什麼差別
full$Age[is.na(full$Age)] <- mean(full$Age,na.rm=T)
sum(is.na(full$Age))
ggplot(full[1:LT,],aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)

#看看手足跟生存會有什麼差別
full$SibSp[is.na(full$SibSp)] <- mean(full$SibSp,na.rm=T)
sum(is.na(full$SibSp))
ggplot(full[1:LT,],aes(x=SibSp,fill=Survived))+geom_histogram(binwidth =3)

#看看父母跟生存會有什麼差別
full$Parch[is.na(full$Parch)] <- mean(full$Parch,na.rm=T)
sum(is.na(full$Parch))
ggplot(full[1:LT,],aes(x=Parch,fill=Survived))+geom_histogram(binwidth =3)

#看看Fare跟生存會有什麼差別
full$Fare[is.na(full$Fare)] <- mean(full$Fare,na.rm=T)
ggplot(data = full[1:LT,],aes(x=Fare,fill=Survived))+geom_histogram(binwidth =20, position="fill")


#開始做預測囉
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
gender<-read.csv("C:/Users/ASUS/Desktop/titanic/gender_submission.csv")

#畫個決策樹
full_train<-full[1:891,]
full_test<-full[891:1309,]

model<-rpart(Survived~Age+Sex+Pclass+Embarked+SibSp+Parch+Fare,data=full_train,method = "class")
rpart.plot(model, type = 2, box.palette = c("red", "green"))

model<-rpart(Survived~Age+Sex+Pclass+Embarked,data=full_train,method = "class")
rpart.plot(model, type = 2, box.palette = c("red", "green"))



#預測
install.packages("caret")
library(caret)
Predict_train<-predict(model,data=full_train,type="class")
table(Predict_train)
print(Predict_train)
#混淆矩陣
full_train$Survived<-as.factor(full_train$Survived)
install.packages("e1071")
library(e1071)
confusionMatrix(Predict_train,full_train$Survived)


#隨機森林
install.packages("randomForest")
library(randomForest)

set.seed(123)
rf_model<-randomForest(factor(Survived) ~ Pclass+ Sex + Fare + Embarked + Age + SibSp + Parch, 
                       data = full_train,importance=TRUE,ntree=200)


set.seed(123)
rf_model<-randomForest(factor(Survived) ~ Pclass+ Sex + Fare + Age, 
                       data = full_train,importance=TRUE,ntree=200)

print(rf_model)
plot(rf_model)
varImpPlot(rf_model)
importance(rf_model)

tbl.rf <- rf_model$confusion[,c(1,2)]
accuracy <- sum(diag(tbl.rf)) / sum(tbl.rf)
accuracy



#做出預測!!!(我快哭了)
Prediction<-predict(rf_model,newdata = full_test)
solution <- data.frame(Survived = Prediction, PassengerID = full_test$PassengerId)
write.csv(solution, file = 'rf_model_sol.csv', row.names = F)



