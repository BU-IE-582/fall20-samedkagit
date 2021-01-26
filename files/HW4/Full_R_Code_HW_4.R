#IE 582 Homework 4
library(ggplot2)
library(dplyr)
library(yardstick)
library(broom)
library(data.table)
library(stringr)
library(tidyr)
require(glmnet)
library(cvAUC)
library(pROC)
library(caret)
library(ranger)
library(rpart)
library(MLmetrics)


## Read Data

data_popularity <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW4/OnlineNewsPopularity.csv")
data_ufc    <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW4/ufc-master.csv")
data_hr  <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW4/HR-Employee-Attrition.csv")
data_activity_train  <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW4/activity-train.csv")
data_activity_test  <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW4/activity-test.csv")

data_popularity <- data_popularity %>% select(-url)

unique.names <- make.unique(colnames(data_activity_train))
colnames(data_activity_test) <- unique.names
colnames(data_activity_train) <- unique.names


activity_train <- data_activity_train %>% select(-subject)
activity_test <- data_activity_test %>% select(-subject)
rm(data_activity_test,data_activity_train)


colnames(activity_train) <- make.names(colnames(activity_train))
colnames(activity_test) <- make.names(colnames(activity_test))


data_ufc <- data_ufc %>% select(-ends_with("_rank"),-ends_with("_bout"),-R_fighter,-B_fighter,-date,-location,-country,-weight_class,-gender,-constant_1,
                                -starts_with("finish")) %>%drop_na() 

colnames(data_ufc) <- make.names(colnames(data_ufc))

data_hr <- data_hr %>% select(-Over18)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}



#Data_Popularity 

#Train-Test
set.seed(42)
popularity_train_index <- sample(nrow(data_popularity),38644)
popularity_train <- data_popularity[popularity_train_index,]
popularity_test <- data_popularity[-popularity_train_index,]
rm(data_popularity)

set.seed(42)
myFolds1 <- createMultiFolds(popularity_train$shares,k=10,times = 1)

myControlReg1 <-trainControl(method="cv", verboseIter=TRUE,index=myFolds1,allowParallel=TRUE)


myGridGlmnet1 <- expand.grid(alpha= 1, lambda = c(seq(0.0001,40,length=6)))
myGridForest1 <- expand.grid(mtry = c(2,5,7,10) ,min.node.size=5, splitrule = "variance")
myGridGbm1 <- expand.grid(interaction.depth=c(1,3,5), n.trees = (3:5)*50,shrinkage=c(0.05,0.01,0.02), n.minobsinnode=10)
myGridDt1 <- expand.grid(cp=c(0.005,0.01,0.003,0.007,0.0005,0.001))


#Model 1 - Lasso Regression

glmnet1 <- train(shares ~., data=popularity_train, method="glmnet", tuneGrid= myGridGlmnet1, trControl= myControlReg1,preProcess=c("center","scale"))

#Model 2 - Decision Tree - Manually Tune minbucket 
set.seed(42)
dt1 <- train(shares~., data=popularity_train,method ="rpart",tuneGrid = myGridDt1,trControl=myControlReg1, control = rpart.control(minbucket=c(1)))
set.seed(42)
dt2 <- train(shares~., data=popularity_train,method ="rpart",tuneGrid = myGridDt1,trControl=myControlReg1, control = rpart.control(minbucket=c(5)))
set.seed(42)
dt3 <- train(shares~., data=popularity_train,method ="rpart",tuneGrid = myGridDt1,trControl=myControlReg1, control = rpart.control(minbucket=c(10)))

#dt3 is the best among them

#Model 3 - Random Forest
set.seed(42)
forest1 <- train(shares ~., data=popularity_train, method="ranger", tuneGrid= myGridForest1, trControl= myControlReg1)

#Model 4 - SGB
set.seed(42)
gbm1 <- train(shares ~., data=popularity_train, method="gbm", tuneGrid= myGridGbm1, trControl= myControlReg1)


#Compare Models in train data

model_list1 <- list(Glmnet=glmnet1, DT=dt3, Forest = forest1, SGBM = gbm1)
resamp1 <- resamples(model_list1)
bwplot(resamp1,metric ="RMSE")

#Compare Models in test data
prediction_popularity_glm <- predict(glmnet1,popularity_test)
prediction_popularity_dt <- predict(dt3,popularity_test)
prediction_popularity_forest <- predict(forest1,popularity_test)
prediction_popularity_gbm <- predict(gbm1,popularity_test)

Test_Performances1 <- data.table(
Glm_Test_RMSE = RMSE(popularity_test$shares,prediction_popularity_glm),
Dt_Test_RMSE = RMSE(popularity_test$shares,prediction_popularity_dt),
Forest_Test_RMSE = RMSE(popularity_test$shares,prediction_popularity_forest),
SGBM_Test_RMSE = RMSE(popularity_test$shares,prediction_popularity_gbm)
)


#Data UFC
#Train-Test
set.seed(42)
ufc_train_index <- sample(nrow(data_ufc),2638)
ufc_train <- data_ufc[ufc_train_index,]
ufc_test <- data_ufc[-ufc_train_index,]
rm(data_ufc)

set.seed(42)
myFolds2 <- createMultiFolds(ufc_train$Winner,k=10,times = 1)

myControlClass2<-trainControl(method="cv",verboseIter=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE,savePredictions=TRUE,index=myFolds2,allowParallel=TRUE)


myGridGlmnet2 <- expand.grid(alpha= 1, lambda = c(seq(0.0001,0.1,length=6)))
myGridForest2 <- expand.grid(mtry = c(2,5,7,10) ,min.node.size=5, splitrule = "gini")
myGridGbm2 <- expand.grid(interaction.depth=c(1,3,5), n.trees = (3:5)*50,shrinkage=c(0.05,0.01, 0.005), n.minobsinnode=10)
myGridDt2 <- expand.grid(cp=c(0.005,0.01,0.003,0.007,0.1,0.03))

model_weights <- ifelse(ufc_train$Winner == "red",
                        (1/table(ufc_train$Winner)[1]) * 0.5,
                        (1/table(ufc_train$Winner)[2]) * 0.5)


#Model 1 - Lasso Regression

glmnet2<-train(Winner~.,data=ufc_train,method="glmnet",tuneGrid=myGridGlmnet2,trControl=myControlClass2,preProcess=c("center","scale"),weights =model_weights)

#Model 2 - Decision Tree - Manually Tune minbucket 
set.seed(42)
dt4 <- train(Winner~., data=ufc_train,method ="rpart",tuneGrid = myGridDt2,trControl=myControlClass2, control = rpart.control(minbucket=c(1)),weights =model_weights)
set.seed(42)
dt5 <- train(Winner~., data=ufc_train,method ="rpart",tuneGrid = myGridDt2,trControl=myControlClass2, control = rpart.control(minbucket=c(5)),weights =model_weights)
set.seed(42)
dt6 <- train(Winner~., data=ufc_train,method ="rpart",tuneGrid = myGridDt2,trControl=myControlClass2, control = rpart.control(minbucket=c(10)),weights =model_weights)

#All the same

#Model 3 - Random Forest
set.seed(42)
forest2 <- train(Winner ~., data=ufc_train, method="ranger", tuneGrid= myGridForest2, trControl= myControlClass2,weights =model_weights)

#Model 4 - SGB
set.seed(42)
gbm2 <- train(Winner ~., data=ufc_train, method="gbm", tuneGrid= myGridGbm2, trControl= myControlClass2,weights =model_weights)


#Compare Models in train data

model_list2 <- list(Glmnet=glmnet2, DT=dt6, Forest = forest2, SGBM = gbm2)
resamp2 <- resamples(model_list2)
bwplot(resamp2,metric ="ROC")

#Compare Models in test data
prediction_ufc_glm <- predict(glmnet2,ufc_test,type="prob")
prediction_ufc_dt <- predict(dt6,ufc_test,type="prob")
prediction_ufc_forest <- predict(forest2,ufc_test,type="prob")
prediction_ufc_gbm <- predict(gbm2,ufc_test,type="prob")

Test_Performances2 <- data.table(
  Glm_Test_ROC = roc(ufc_test$Winner,prediction_ufc_glm[,1])$auc,
  Dt_Test_ROC = roc(ufc_test$Winner,prediction_ufc_dt[,1])$auc,
  Forest_Test_ROC =roc(ufc_test$Winner,prediction_ufc_forest[,1])$auc,
  SGBM_Test_ROC = roc(ufc_test$Winner,prediction_ufc_gbm[,1])$auc
)

#Data Activity
set.seed(42)
myFolds3 <- createMultiFolds(activity_train$Activity,k=10,times =1)

myControlClass3<-trainControl(method="cv",verboseIter=TRUE,summaryFunction=multiClassSummary,classProbs=TRUE,savePredictions=TRUE,index=myFolds3,allowParallel=TRUE)

myGridGlmnet3 <- expand.grid(alpha= 1, lambda = c(seq(0.0001,0.02,length=6)))
myGridForest3 <- expand.grid(mtry = c(2,5,7,10) ,min.node.size=5, splitrule = "gini")
myGridGbm3 <- expand.grid(interaction.depth=c(1,3,5), n.trees = (3:5)*50,shrinkage=c(0.05,0.01, 0.005), n.minobsinnode=10)
myGridDt3 <- expand.grid(cp=c(0.005,0.01,0.003,0.007,0.1,0.03))

#Model 1 - Lasso Regression

glmnet3<-train(Activity~.,data=activity_train,method="glmnet",tuneGrid=myGridGlmnet3,metric="logLoss",trControl=myControlClass3,preProcess=c("zv","center","scale"))

#########
#Model 2 - Decision Tree - Manually Tune minbucket 
set.seed(42)
dt7 <- train(Activity~., data=activity_train,method ="rpart",tuneGrid = myGridDt3,metric="logLoss",trControl=myControlClass3, control = rpart.control(minbucket=c(1)))
set.seed(42)
dt8 <- train(Activity~., data=activity_train,method ="rpart",tuneGrid = myGridDt3,metric="logLoss",trControl=myControlClass3, control = rpart.control(minbucket=c(5)))
set.seed(42)
dt9 <- train(Activity~., data=activity_train,method ="rpart",tuneGrid = myGridDt3,metric="logLoss",trControl=myControlClass3, control = rpart.control(minbucket=c(10)))

#All are the same

#Model 3 - Random Forest
set.seed(42)
forest3 <- train(Activity ~., data=activity_train, method="ranger", tuneGrid= myGridForest3,metric="logLoss",trControl= myControlClass3)

#Model 4 - SGB
set.seed(42)
gbm3 <- train(Activity ~., data=activity_train, method="gbm", tuneGrid= myGridGbm3,metric="logLoss",trControl= myControlClass3)


#Compare Models in train data

model_list3 <- list(Glmnet=glmnet3, DT=dt9, Forest = forest3, SGBM = gbm3)
resamp3 <- resamples(model_list3)
bwplot(resamp3,metric ="logLoss")

#Compare Models in test data
prediction_activity_glm <- predict(glmnet3,activity_test,type="prob")
prediction_activity_dt <- predict(dt9,activity_test,type="prob")
prediction_activity_forest <- predict(forest3,activity_test,type="prob")
prediction_activity_gbm <- predict(gbm3,activity_test,type="prob")


Test_Performances3 <- data.table(
  Glm_Test_LL = MultiLogLoss(y_true =activity_test$Activity,y_pred =data.matrix(prediction_activity_glm)),
  Dt_Test_LL =MultiLogLoss(y_true =activity_test$Activity,y_pred =data.matrix(prediction_activity_dt)),
  Forest_Test_LL =MultiLogLoss(y_true =activity_test$Activity,y_pred =data.matrix(prediction_activity_forest)),
  SGBM_Test_LL = MultiLogLoss(y_true =activity_test$Activity,y_pred =data.matrix(prediction_activity_gbm))
)



#Data HR
#Train-Test
set.seed(42)
hr_train_index <- sample(nrow(data_hr),1170)
hr_train <- data_hr[hr_train_index,]
hr_test <- data_hr[-hr_train_index,]

rm(data_hr)

set.seed(42)
myFolds4 <- createMultiFolds(hr_train$Attrition,k=10,times = 1)

myControlClass4<-trainControl(method="cv",verboseIter=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE,savePredictions=TRUE,index=myFolds4,allowParallel=TRUE)


myGridGlmnet4 <- expand.grid(alpha= 1, lambda = c(seq(0.00001,0.001,length=6)))
myGridForest4 <- expand.grid(mtry = c(2,5,7,10,15,20) ,min.node.size=5, splitrule = "gini")
myGridGbm4 <- expand.grid(interaction.depth=c(1,3,5), n.trees = (3:5)*50,shrinkage=c(0.05,0.01, 0.005), n.minobsinnode=10)
myGridDt4 <- expand.grid(cp=c(0.01,0.007,0.1,0.03))

model_weights <- ifelse(hr_train$Attrition == "Yes",
                        (1/table(hr_train$Attrition)[1]) * 0.5,
                        (1/table(hr_train$Attrition)[2]) * 0.5)

#Model 1 - Lasso Regression

glmnet4<-train(Attrition~.,data=hr_train,method="glmnet",tuneGrid=myGridGlmnet4,trControl=myControlClass4,preProcess=c("zv","center","scale"),weights =model_weights)

#Model 2 - Decision Tree - Manually Tune minbucket 
set.seed(42)
dt10 <- train(Attrition~., data=hr_train,method ="rpart",tuneGrid = myGridDt4,trControl=myControlClass4, control = rpart.control(minbucket=c(1)),weights =model_weights)
set.seed(42)
dt11 <- train(Attrition~., data=hr_train,method ="rpart",tuneGrid = myGridDt4,trControl=myControlClass4, control = rpart.control(minbucket=c(5)),weights =model_weights)
set.seed(42)
dt12 <- train(Attrition~., data=hr_train,method ="rpart",tuneGrid = myGridDt4,trControl=myControlClass4, control = rpart.control(minbucket=c(10)),weights =model_weights)

#dt11 is the best

#Model 3 - Random Forest
set.seed(42)
forest4 <- train(Attrition ~., data=hr_train, method="ranger", tuneGrid= myGridForest4, trControl= myControlClass4,weights =model_weights)

#Model 4 - SGB
set.seed(42)
gbm4 <- train(Attrition ~., data=hr_train, method="gbm", tuneGrid= myGridGbm4, trControl= myControlClass4,weights =model_weights)



#Compare Models in train data

model_list4 <- list(Glmnet=glmnet4, DT=dt11, Forest = forest4, SGBM = gbm4)
resamp4 <- resamples(model_list4)
bwplot(resamp4,metric ="ROC")

#Compare Models in test data
prediction_hr_glm <- predict(glmnet4,hr_test,type="prob")
prediction_hr_dt <- predict(dt11,hr_test,type="prob")
prediction_hr_forest <- predict(forest4,hr_test,type="prob")
prediction_hr_gbm <- predict(gbm4,hr_test,type="prob")

Test_Performances4 <- data.table(
  Glm_Test_ROC = roc(hr_test$Attrition,prediction_hr_glm[,1])$auc,
  Dt_Test_ROC = roc(hr_test$Attrition,prediction_hr_dt[,1])$auc,
  Forest_Test_ROC =roc(hr_test$Attrition,prediction_hr_forest[,1])$auc,
  SGBM_Test_ROC = roc(hr_test$Attrition,prediction_hr_gbm[,1])$auc
)





