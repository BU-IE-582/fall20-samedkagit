#IE 582 / HW3
library(ggplot2)
library(dplyr)
library(yardstick)
library(broom)
library(purrr)
library(data.table)
library(stringr)
library(tidyr)
library(ggfortify)
library(tidyverse)
library(Metrics)
library(glmnet)
library(CVXR)
library(plotly)
library(viridis)
#Data Reading

consumption <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW3/RealTimeConsumption-01012016-01122020.csv")
consumption$Tarih <- paste(consumption$Tarih,consumption$Saat)
consumption <-consumption %>%select(-Saat)

consumption$Tarih <- as.POSIXct(consumption$Tarih, format = '%d.%m.%Y %H:%M', tz="Europe/Istanbul")
consumption <- consumption %>% rename(Tuketim=`Tüketim Miktarý (MWh)`)
consumption$Tuketim <- gsub("\\.", "",consumption$Tuketim)
consumption$Tuketim <- gsub("\\,", ".",consumption$Tuketim)
consumption$Tuketim <- as.numeric(consumption$Tuketim)
consumption <- consumption %>% mutate(lag_168 = lag(Tuketim,168), lag_48 = lag(Tuketim,48)) %>% drop_na()

duplicated = consumption[duplicated(consumption$Tarih)]$Tarih
consumption <- consumption %>% filter(!(as.IDate(Tarih) %in% c(as.IDate(duplicated),as.IDate(duplicated+3600*24*2),as.IDate(duplicated+3600*24*7))))

#TASK A

naive_test <- consumption %>% filter(Tarih >= as.POSIXct("01-11-2020", format = '%d-%m-%Y'))

mape_lag48_hourly <- naive_test %>% group_by(as.ITime(Tarih)) %>% summarize(mape= mape(Tuketim,lag_48)) %>% rename(Saat = `as.ITime(Tarih)`)
mape_lag168_hourly <- naive_test %>% group_by(as.ITime(Tarih)) %>% summarize(mape= mape(Tuketim,lag_168)) %>% rename(Saat = `as.ITime(Tarih)`)

#TASK B

lm_train <- consumption %>% filter(Tarih < as.POSIXct("01-11-2020", format = '%d-%m-%Y'))
lm_test <-  consumption %>% filter(Tarih >= as.POSIXct("01-11-2020", format = '%d-%m-%Y'))

lm_model <- lm(Tuketim ~ lag_48+lag_168, lm_train)

lm_test$prediction <- predict(lm_model,lm_test)
lm_mape <- lm_test %>% group_by(as.ITime(Tarih)) %>% summarize(mape= mape(Tuketim, prediction)) %>% rename(Saat = `as.ITime(Tarih)`)


#TASK C

lm_train_hourly <- lm_train %>% mutate(saat = as.ITime(Tarih))
lm_test_hourly  <- lm_test %>% mutate(saat = as.ITime(Tarih))  %>% select(-prediction)

lm_models_hourly<-lm_train_hourly%>%group_by(saat)%>%nest()%>% 
  mutate(fit=purrr::map(data,function(d)lm(Tuketim~lag_168+lag_48,data=d)))

lm_models_hourly_mape = data.frame( hour = 0:23 , mape = 0)
hour_0 = as.ITime(as.POSIXct("01-11-2020",format ='%d-%m-%Y'))

for( i in 1:24)
{
predict_data <- lm_test_hourly %>% filter( saat==hour_0+3600*(i-1) )
lm_models_hourly_mape$mape[i]<-mape(predict_data$Tuketim,predict(lm_models_hourly$fit[[i]],predict_data))
rm(predict_data)
}

lm_models_hourly_mape

sum((lm_models_hourly_mape$mape - lm_mape$mape)>0)

#TASK D

consumption2 <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW3/RealTimeConsumption-01012016-01122020.csv")

consumption2$Tarih <- as.POSIXct(consumption2$Tarih, format = '%d.%m.%Y', tz="Europe/Istanbul")
consumption2 <- consumption2 %>% rename(Tuketim=`Tüketim Miktarý (MWh)`)
consumption2$Tuketim <- gsub("\\.", "",consumption2$Tuketim)
consumption2$Tuketim <- gsub("\\,", ".",consumption2$Tuketim)
consumption2$Tuketim <- as.numeric(consumption2$Tuketim)
consumption2 <- consumption2 %>% filter(!(as.IDate(Tarih) %in% c(as.IDate(duplicated))))



consumption_lag_48 <- consumption2 %>% pivot_wider(names_from = Saat, values_from= Tuketim, names_prefix="lag_48_")
consumption_lag_168 <- consumption2 %>% pivot_wider(names_from = Saat, values_from= Tuketim, names_prefix="lag_168_")

consumption_lag_48$Tarih = consumption_lag_48$Tarih + 3600*48
consumption_lag_168$Tarih = consumption_lag_168$Tarih + 3600*168

lasso_data <-consumption2 %>% inner_join(consumption_lag_48, by = "Tarih")
lasso_data <- lasso_data %>% inner_join(consumption_lag_168, by="Tarih")


lasso_data_train <- lasso_data %>% filter(Tarih < as.POSIXct("01-11-2020", format = '%d-%m-%Y'))
lasso_data_test <-  lasso_data %>% filter(Tarih >= as.POSIXct("01-11-2020", format = '%d-%m-%Y'))


lasso_models_hourly<-lasso_data_train%>%group_by(Saat)%>%nest()%>% 
  mutate(fit=purrr::map(data,function(d)cv.glmnet(data.matrix(d%>%select(contains("lag"))),data.matrix(d$Tuketim),family="gaussian",standardize =TRUE)))
      

lasso_models_hourly_mape = data.frame( hour = 0:23 , mape = 0)
k=1
set.seed(1234)
for( i in c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",paste(10:23,":00",sep="")))
{
  predict_data <- lasso_data_test %>% filter(Saat == i)
  lasso_models_hourly_mape$mape[k]<-mape(predict_data$Tuketim,predict(lasso_models_hourly$fit[[k]],data.matrix(predict_data%>%select(contains("lag"))),s= "lambda.1se"))
  k = k+1
  rm(predict_data)
}
rm(k)
lasso_models_hourly_mape

for(i in 1:24)
{
  print(coef(lasso_models_hourly$fit[[i]],s="lambda.1se"))
  lasso_models_hourly$fit[[i]]
}


#TASK E
lasso_data_model <- lasso_data
fused_lasso_results = data.table(hour="00:00",lambda=0, objective = 0,MAPE = 0)
thresh <- 1e-7
for( j in c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",paste(10:23,":00",sep="")))
{
for(lambda in c(100000000,50000000,20000000,seq(200000,1100000,300000)))
{
for(i in 1:10)
{
hour_data<-lasso_data_model %>%filter(Saat == j)%>%filter(Tarih < as.POSIXct("01-11-2020",format ='%d-%m-%Y'))
test_data <- hour_data[(round(nrow(hour_data)*0.1)*(i-1)+1):(round(nrow(hour_data)*0.1)*i),]  

train_data <- anti_join(hour_data,test_data,by=c("Tarih","Saat")) %>% select(matches("lag_|Tuketim"))
test_data <- test_data %>% select(matches("lag_|Tuketim"))
betaHat <- Variable(49) # betaHat[49] is the intercept
matrix1 <- as.matrix(train_data%>%select(-Tuketim))
matrix2 <- as.matrix(test_data%>%select(-Tuketim))

objective<-(sum((train_data$Tuketim-((matrix1%*%betaHat[1:48])+betaHat[49]))^2)+lambda*sum(betaHat[1:48]^2)+lambda*sum(abs(betaHat[2:24]-betaHat[1:23]))+lambda*sum(abs(betaHat[26:48]-betaHat[25:47])))/nrow(train_data)

problem <- Problem(Minimize(objective))
result <- solve(problem, FEASTOL = thresh, RELTOL = thresh, ABSTOL = thresh, verbose = TRUE)
beta <- result$getValue(betaHat)

temp<-data.table(hour=j,lambda=lambda, objective= result$value , MAPE = mape(test_data$Tuketim,(matrix2%*%beta[1:48])+beta[49]))
fused_lasso_results <- rbind(fused_lasso_results,temp)
}
}
}

evaluation <- fused_lasso_results[-1,] %>%group_by(hour,lambda) %>% 
  summarise(Mean_MAPE = mean(MAPE), Se_MAPE = sqrt(var(MAPE)), Mean_Objective = mean(objective))%>%ungroup()%>% arrange(hour,lambda)

# Lambda = 20000000 is the best among them.

fused_lasso_results2 = data.table(hour="00:00",lambda=0, objective = 0,MAPE = 0)
betas = paste("b",1:49,sep = "")
fused_lasso_results2[,betas] <- 0

for( j in c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",paste(10:23,":00",sep="")))
{
  for(lambda in c(20000000))
  {
      hour_data<-lasso_data_model %>%filter(Saat == j)%>%filter(Tarih < as.POSIXct("01-11-2020",format ='%d-%m-%Y'))%>%select(matches("lag_|Tuketim"))
      test_data<-lasso_data_model %>%filter(Saat == j)%>%filter(Tarih >= as.POSIXct("01-11-2020",format ='%d-%m-%Y'))%>%select(matches("lag_|Tuketim"))
      
      betaHat <- Variable(49) # betaHat[49] is the intercept
      matrix1 <- as.matrix(hour_data%>%select(-Tuketim))
      matrix2 <- as.matrix(test_data%>%select(-Tuketim))
      
      objective<-sum((hour_data$Tuketim-((matrix1%*%betaHat[1:48])+betaHat[49]))^2)+lambda*sum(betaHat[1:48]^2)+lambda*sum(abs(betaHat[2:24]-betaHat[1:23]))+lambda*sum(abs(betaHat[26:48]-betaHat[25:47]))
      
      problem <- Problem(Minimize(objective))
      result <- solve(problem, FEASTOL = thresh, RELTOL = thresh, ABSTOL = thresh, verbose = TRUE)
      beta <- result$getValue(betaHat)
      
      temp<-data.table(hour=j,lambda=lambda,objective= result$value, MAPE = mape(test_data$Tuketim,(matrix2%*%beta[1:48])+beta[49]))
      for(s in 1:49)
      temp[,betas[s]] <- beta[s]
      fused_lasso_results2 <- rbind(fused_lasso_results2,temp)
  }
}
fused_lasso_results2 <- fused_lasso_results2[-1,]

#Task F

data1 <- fused_lasso_results2 %>% select(hour,MAPE)
data1[,"name"] <- "Fused_Lasso"

data2 <- lasso_models_hourly_mape %>% rename(MAPE = mape)
data2[,"name"] <- "Hourly_Lasso_Models"
data2$hour <- data1$hour

data3 <- lm_mape %>% rename(hour =Saat, MAPE = mape)
data3[,"name"] <- "Linear_Model"
data3$hour <- data1$hour

data4 <- mape_lag168_hourly %>% rename(hour =Saat, MAPE = mape)
data4[,"name"] <- "Lag_168"
data4$hour <- data1$hour

data5 <- lm_models_hourly_mape %>% rename(MAPE = mape)
data5$hour <- data1$hour
data5[,"name"] <-"Hourly_Linear_Models"

data6 <- mape_lag48_hourly %>% rename(hour =Saat, MAPE = mape)
data6[,"name"] <- "Lag_48"
data6$hour <- data1$hour

data <- rbind(data1,data2,data3,data4,data5,data6)

# Plot
data %>% ggplot( aes(x=name, y=MAPE, fill=name)) + geom_boxplot() +theme(legend.position="none",plot.title = element_text(size=11)) +
  ggtitle("MAPE Comparison") + xlab("Methods")


