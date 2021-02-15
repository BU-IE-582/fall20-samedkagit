require(caret)
library(ggplot2)
library(dplyr)
library(yardstick)
library(broom)
library(data.table)
library(stringr)
library(tidyr)
library(purrr)
require(glmnet)
library(WRTDStidal)
library(varhandle)
library(cvAUC)
library(pROC)
library(parallel)
library(doParallel)
library(ISLR)
library(klaR)
library(e1071)
library(ada)

train <- fread("C:/Users/Abdulsamed/Desktop/IE582/IE582_Fall20_ProjectTrain.csv")
test <- fread("C:/Users/Abdulsamed/Desktop/IE582/IE582_Fall20_ProjectTest.csv")

set.seed(42)
myFolds <- createMultiFolds(train$y,k=10,times = 3)


myControl<-trainControl(method="cv",number=10,verboseIter=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE,savePredictions=TRUE,index=myFolds,allowParallel=TRUE)


myGridGlmnet <- expand.grid(alpha= 0:1, lambda = c(seq(0.0001,0.1,length=10),seq(0.15,5,length=10)))
myGridForest <- expand.grid(mtry = c(2,5,7,10,15,30,60),splitrule = c("gini","extratrees"), min.node.size=c(1,3,5,10))
myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:50)*50,shrinkage=c(0.01, 0.001), n.minobsinnode=10)


glm_model0 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("center","scale")) 
glm_model1 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale")) 
glm_model2 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale","pca")) 
glm_model3 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale","spatialSign"))


penalized_model0 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("center","scale"))
penalized_model1 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale"))
penalized_model2 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale","pca"))
penalized_model3 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale","spatialSign"))

plot(penalized_model0)

forest_model0 <- train(y~.,train, method="ranger",tuneGrid=myGridForest,trControl = myControl)
plot(forest_model0)

knn_model0 <- train(y~.,data=train, method = "knn", trControl = myControl,preProcess = c("nzv","center","scale"), tuneLength = 20)
plot(knn_model0)

table(train$y)

gbm_model0 = train(y~.,data=train, method = "gbm",tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv"))

#Imbalanced Class Weighting
model_weights <- ifelse(train$y == "a",
                        (1/table(train$y)[1]) * 0.5,
                        (1/table(train$y)[2]) * 0.5)


glm_model4 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("center","scale")) 
glm_model5 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale")) 
glm_model6 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale","pca")) 
glm_model7 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale","spatialSign"))


penalized_model4<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("center","scale"))
penalized_model5<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale"))
penalized_model6<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale","pca"))
penalized_model7<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale","spatialSign"))

forest_model1 <- train(y~.,train, method="ranger",tuneGrid=myGridForest,weights=model_weights,trControl = myControl)

gbm_model1 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv"))

gbm_model2 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv","pca"))

gbm_model3 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("nzv"))

gbm_model4 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl)



myGridGbm3 <- expand.grid(interaction.depth=c(3), n.trees = (1:70)*10,shrinkage=c(0.01,0.02,0.03,0.05), n.minobsinnode=c(3,9,10,11))
set.seed(55)
gbm_model6 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm3 ,trControl = myControl)


##MODELLER YUKARDA, BUNDAN SONRASI ANALÝZ

model_list <- list(#glm0=glm_model0,glm1=glm_model1,glm2=glm_model2,glm3=glm_model3,
                   glm4=glm_model4,glm5=glm_model5,glm6=glm_model6,glm7=glm_model7,
                   glmnet0=penalized_model0,glmnet1=penalized_model1,glmnet2=penalized_model2,glmnet3=penalized_model3,glmnet4=penalized_model4,
                   glmnet5=penalized_model5,glmnet6=penalized_model6,glmnet7=penalized_model7,
                   forest0=forest_model0,forest1=forest_model1,
                   #knn0 = knn_model0,
                   gbm0 = gbm_model0, gbm1 = gbm_model1, gbm2 = gbm_model2,gbm3=gbm_model3,gbm4=gbm_model4,gmb6 = gbm_model6)



resamp <- resamples(model_list)
summary(resamp)

bwplot(resamp,metric ="ROC")
bwplot(resamp,metric ="Sens")
bwplot(resamp,metric ="Spec")

dotplot(resamp,metric ="ROC")
dotplot(resamp,metric ="Sens")
dotplot(resamp,metric ="Spec")


#Comparison
results = data.table()
for(i in model_list)
{
  k <- which.max(i[["results"]]$ROC)
  temp<- max(i[["results"]]$ROC) + ( 0.5*i[["results"]]$Sens[k]+0.5*i[["results"]]$Spec[k])
  results = rbind(results,temp/2)
}
results$model <- names(model_list)
results %>% arrange(desc(x))


predictions4 <- predict(gbm_model4,test,type ="prob")
predictions6 <- predict(gbm_model6,test,type ="prob")


which(predictions4$b > 0.5  & predictions4$b < 0.51)
which(predictions4$b < 0.5  & predictions4$b > 0.49)



prediction_deneme <- predictions4$b
prediction_deneme[898] <- 0 
prediction_deneme[1114] <- 0 
prediction_deneme[1259] <- 1  
prediction_deneme[1531] <- 0 
prediction_deneme[2012] <- 0 
prediction_deneme[788] <- 0 
prediction_deneme[952] <- 0 
prediction_deneme[650] <- 0 
prediction_deneme[1005] <- 0
prediction_deneme[1852] <- 0
prediction_deneme[1861] <- 1 
prediction_deneme[1896] <- 0
prediction_deneme[135] <- 0
prediction_deneme[440] <- 1 
prediction_deneme[1342] <- 0
prediction_deneme[1957] <- 0



















#152,264,523,576,650,679,709,787,815,937,1201,1750,54,637,1201,1323,1380,1383,1592,1629,1760,1767,1866,1874 testte yok
