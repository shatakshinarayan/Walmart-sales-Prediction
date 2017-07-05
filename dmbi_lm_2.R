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

train <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/trainV6.csv")
sub <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/sampleSubmission.csv/sampleSubmission.csv")
test <- read.csv("C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/Project/test.csv/test_v1.csv")

#data preparation
train$dHoliday = train$IsHoliday*train$Days
train$log_sale = log(4990+train$Weekly_Sales)
test$dHoliday = test$IsHoliday*test$Days


#Run model
#linear regression
nrow_subm = nrow(sub)
i=1
rsq = data.frame()
while (i < nrow_subm) {
  print(i/nrow_subm)#Track progress
  #select data for the store and department
  t_Id = as.character(sub$Id[i])
  t_Str = unlist(strsplit(t_Id,"_"))
  t_Store = t_Str[1]
  t_Dept = t_Str[2]
  data_F1 = train[train$Dept==t_Dept,]
  nrow_DF1 = nrow(data_F1[data_F1$Store==t_Store,])
  tmp_h0 = data_F1[data_F1$IsHoliday==1,]
  tmp_h1 = data_F1[data_F1$Week==47,]#high2
  tmp_h2 = data_F1[data_F1$Week==49,]#high1
  tmp_h3 = data_F1[data_F1$Week==50,]#high2
  tmp_h4 = data_F1[data_F1$Week==51,]#high3
  tmp_h5 = data_F1[data_F1$Week==52,]#high2
  #since MAE is weighted, assigning weights to holiday weeks
  data_F1 = rbind(data_F1,do.call("rbind", replicate(10, tmp_h0, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(50, tmp_h1, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(25, tmp_h2, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(50, tmp_h3, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(75, tmp_h4, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(50, tmp_h5, simplify = FALSE)))
  data_F2 = data_F1[data_F1$Store==t_Store,]
  test_F1 = test[test$Dept==t_Dept,]
  test_F1 = test_F1[test_F1$Store==t_Store,]
  nrow_test = nrow(test_F1)
  if (nrow_DF1<10) {#sample size restrictions
    #this model uses all dept data as store + dept pair does not exist in the training set
    tmp_Mod = lm(log_sale~Store_Size+StoreTypeFac+ Year + Month + Day + Days + dHoliday + totaldays + days30+ Week, 
                  data=data_F1)
    r2= summary(tmp_Mod)$r.squared}
  else {
    #Model trained on store+dept filtered data
    #linear regression
    tmp_Mod =  lm(log_sale ~ Year + Month + Day + Days + dHoliday + totaldays + days30+ Week, 
                   data=data_F2)
    r2= summary(tmp_Mod)$r.squared}
  Pred = exp(predict(tmp_Mod,test_F1))-4990
  k = i + nrow_test - 1
  sub$Weekly_Sales[i:k] = Pred
  rsq = rbind(rsq, r2)
  i = k+1
}

write.table(x=sub,
            file='C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/model_result_lm_v3.csv',
            sep=',', row.names=FALSE, quote=FALSE)
write.table(x=rsq,
            file='C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/rsq_v3.csv',
            sep=',', row.names=FALSE, quote=FALSE)

#weights variation
nrow_subm = nrow(sub)
i=1
rsq1 = data.frame()
while (i < nrow_subm) {
  print(i/nrow_subm)#Track progress
  #select data for the store and department
  t_Id = as.character(sub$Id[i])
  t_Str = unlist(strsplit(t_Id,"_"))
  t_Store = t_Str[1]
  t_Dept = t_Str[2]
  data_F1 = train[train$Dept==t_Dept,]
  nrow_DF1 = nrow(data_F1[data_F1$Store==t_Store,])
  tmp_h0 = data_F1[data_F1$IsHoliday==1,]
  tmp_h1 = data_F1[data_F1$Week==47,]#high2
  tmp_h2 = data_F1[data_F1$Week==49,]#high1
  tmp_h3 = data_F1[data_F1$Week==50,]#high2
  tmp_h4 = data_F1[data_F1$Week==51,]#high3
  tmp_h5 = data_F1[data_F1$Week==52,]#high2
  #since MAE is weighted, assigning weights to holiday weeks
  data_F1 = rbind(data_F1,do.call("rbind", replicate(10, tmp_h0, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(50, tmp_h1, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(25, tmp_h2, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(30, tmp_h3, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(100, tmp_h4, simplify = FALSE)))
  data_F1 = rbind(data_F1,do.call("rbind", replicate(30, tmp_h5, simplify = FALSE)))
  data_F2 = data_F1[data_F1$Store==t_Store,]
  test_F1 = test[test$Dept==t_Dept,]
  test_F1 = test_F1[test_F1$Store==t_Store,]
  nrow_test = nrow(test_F1)
  if (nrow_DF1<10) {#sample size restrictions
    #this model uses all dept data as store + dept pair does not exist in the training set
    tmp_Mod = lm(log_sale~Store_Size+StoreTypeFac+ Year + Month + Day + Days + dHoliday + totaldays + days30+ Week, 
                 data=data_F1)
    r2= summary(tmp_Mod)$r.squared}
  else {
    #Model trained on store+dept filtered data
    #linear regression
    tmp_Mod =  lm(log_sale ~ Year + Month + Day + Days + dHoliday + totaldays + days30+ Week, 
                  data=data_F2)
    r2= summary(tmp_Mod)$r.squared}
  Pred = exp(predict(tmp_Mod,test_F1))-4990
  k = i + nrow_test - 1
  sub$Weekly_Sales[i:k] = Pred
  #sub$r2[i:k] = r2
  rsq1 = rbind(rsq1, r2)
  i = k+1
}

write.table(x=sub,
            file='C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/model_result_lm_v4.csv',
            sep=',', row.names=FALSE, quote=FALSE)
write.table(x=rsq1,
            file='C:/Users/Wishi/Downloads/Data Mining and Business Intelligence/rsq_v4.csv',
            sep=',', row.names=FALSE, quote=FALSE)
