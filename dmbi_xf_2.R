rm(list = ls())
gc()

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(timeDate)
library(readr)
library(randomForest)

#weight certain features more by duplication, not sure if helpful?
#train$tDays = 360*(train$year-2010) + (train$month-1)*30 + train$day
#train$days30 = (train$month-1)*30 + train$day


train <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/trainV6.csv")
submission <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/sampleSubmission.csv/sampleSubmission.csv")
test <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/test.csv/test_v1.csv")

#data preparation
train$dayHoliday = train$IsHoliday*train$Days
train$logsales = log(4990+train$Weekly_Sales)
test$dayHoliday = test$IsHoliday*test$Days

#Run model
#random forest
tmpR0 = nrow(submission)
j=1
while (j < tmpR0) {
  print(j/tmpR0)#keep track of progress
  #select only relevant data for the store and department tuple
  tmpId = as.character(submission$Id[j])
  tmpStr = unlist(strsplit(tmpId,"_"))
  tmpStore = tmpStr[1]
  tmpDept = tmpStr[2]
  dataF1 = train[train$Dept==tmpDept,]
  tmpL = nrow(dataF1[dataF1$Store==tmpStore,])
  #since MAE is weighted, increase weights of holiday data by 5x
  tmpF = dataF1[dataF1$IsHoliday==1,]
  tmph1 = dataF1[dataF1$Week==47,]#high2 #previous week
  tmph2 = dataF1[dataF1$Week==49,]#high1
  tmph3 = dataF1[dataF1$Week==50,]#high2
  tmph4 = dataF1[dataF1$Week==51,]#high3
  tmph5 = dataF1[dataF1$Week==52,]#high2
  dataF1 = rbind(dataF1,do.call("rbind", replicate(10, tmpF, simplify = FALSE)))
  dataF1 = rbind(dataF1,do.call("rbind", replicate(50, tmph1, simplify = FALSE)))
  dataF1 = rbind(dataF1,do.call("rbind", replicate(25, tmph2, simplify = FALSE)))
  dataF1 = rbind(dataF1,do.call("rbind", replicate(50, tmph3, simplify = FALSE)))
  dataF1 = rbind(dataF1,do.call("rbind", replicate(75, tmph4, simplify = FALSE)))
  dataF1 = rbind(dataF1,do.call("rbind", replicate(50, tmph5, simplify = FALSE)))
  dataF2 = dataF1[dataF1$Store==tmpStore,]  
  testF1 = test[test$Dept==tmpDept,]
  testF1 = testF1[testF1$Store==tmpStore,]
  testRows = nrow(testF1)
  if (tmpL<10) {#sample size restrictions since rf can fail if there isn't enough data
    #this model uses all dept data (since that store + dept pair does not exist in the training set)
    #randomforest
    tmpModel =  randomForest(Weekly_Sales~Store_Size+StoreTypeFac+ Year + Month + Day + Days + dayHoliday + totaldays + days30+ Week, 
                             ntree=50, replace=TRUE, mtry=4, data=dataF1)}
  else {
    #this model is trained on store+dept filtered data
    #randomforest
    tmpModel =  randomForest(Weekly_Sales ~ Store_Size+StoreTypeFac+ Year + Month + Day + Days + dayHoliday + totaldays + days30+ Week, 
                             ntree=50, replace=TRUE, mtry=3, data=dataF2)}
  tmpP = exp(predict(tmpModel,testF1))-4990
  k = j + testRows - 1
  submission$Weekly_Sales[j:k] = tmpP
  j = k+1
}

write.table(x=submission,
            file='C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/outputFinalrf.csv',
            sep=',', row.names=FALSE, quote=FALSE)