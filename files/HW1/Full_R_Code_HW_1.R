#IE 582 HW1-1
library(ggplot2)
library(dplyr)
library(yardstick)
library(broom)
library(lubridate)
library(data.table)
library(stringr)
library(tidyr)
library(chron)

match_2018 <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW1/E0.csv")
match_2019 <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW1/E1.csv")
match_2020 <- fread("C:/Users/Abdulsamed/Documents/GitHub/fall20-samedkagit/files/HW1/E2.csv")

match_2019 <- match_2019 %>% select(-Time)
match_2020 <- match_2020 %>% select(-Time)

#########################################################################################TASK 1#############################################################
#!!!!!!!!!!!!!Season 2018/2019

#Home Score

expected_goal <- 0:6
lambda <- mean(match_2018$FTHG)
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2018)*dpois(expected_goal,lambda))


ggplot(match_2018) + geom_histogram(aes(x=FTHG),binwidth = 1,col="black") + labs(title="Season 2018/2019", x ="Home Goals", y = "Number of Games")+ theme_bw()+ 
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)
  
#Away Score

lambda <- mean(match_2018$FTAG)
expected_goal <- 0:6
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2018)*dpois(expected_goal,lambda))

ggplot(match_2018,aes(x=FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2018/2019", x ="Away Goals", y = "Number of Games")+ theme_bw()+
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)

#Home-Away Score

ggplot(match_2018,aes(x=FTHG-FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2018/2019", x ="Home Goals - Away Goals", y = "Number of Games")+ theme_bw() 

#!!!!!!!!!!!!!Season 2019/2020

#Home Score

lambda <- mean(match_2019$FTHG)
expected_goal <- 0:8
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2019)*dpois(expected_goal,lambda))

ggplot(match_2019,aes(x=FTHG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2019/2020", x ="Home Goals", y = "Number of Games")+ theme_bw()+
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)

#Away Score

lambda <- mean(match_2019$FTAG)
expected_goal <- 0:9
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2019)*dpois(expected_goal,lambda))

ggplot(match_2019,aes(x=FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2019/2020", x ="Away Goals", y = "Number of Games")+ theme_bw()+ 
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)

#Home-Away Score

ggplot(match_2019,aes(x=FTHG-FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2019/2020", x ="Home Goals - Away Goals", y = "Number of Games")+ theme_bw() 

#!!!!!!!!!!!!!Season 2020/2021
#Home Score

lambda <- mean(match_2020$FTHG)
expected_goal <- 0:7
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2020)*dpois(expected_goal,lambda))

ggplot(match_2020,aes(x=FTHG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2020/2021", x ="Home Goals", y = "Number of Games")+ theme_bw()+
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)
#Away Score

lambda <- mean(match_2020$FTHG)
expected_goal <- 0:7
expected_goal <- data.frame(x=expected_goal, y=nrow(match_2020)*dpois(expected_goal,lambda))

ggplot(match_2020,aes(x=FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2020/2021", x ="Away Goals", y = "Number of Games")+ theme_bw()+
geom_line(data = expected_goal, aes(x=x,y=y), color = "red", lwd = 1)

#Home-Away Score

ggplot(match_2020,aes(x=FTHG-FTAG)) + geom_histogram(binwidth = 1,col="black") + labs(title="Season 2020/2021", x ="Home Goals - Away Goals", y = "Number of Games")+ theme_bw() 

#########################################################################################TASK 2#############################################################
#Manipulations

match_2018_filtered <- match_2018 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR)
match_2019_filtered <- match_2019 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR)
match_2020_filtered <- match_2020 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR)
match_combined <- bind_rows(match_2018_filtered,match_2019_filtered, match_2020_filtered)

match_combined<-cbind( match_combined[,1] ,apply(match_combined[,2:13],c(1,2), function(x) { 1/x}), match_combined[,c(14,15)] )
match_combined <- match_combined %>% mutate(B365_sum = apply(match_combined[,2:4],1, FUN = sum ))
match_combined <- match_combined %>% mutate(BW_sum = apply(match_combined[,5:7],1, FUN = sum ))
match_combined <- match_combined %>% mutate(IW_sum = apply(match_combined[,8:10],1, FUN = sum ))
match_combined <- match_combined %>% mutate(PS_sum = apply(match_combined[,11:13],1, FUN = sum ))

match_combined <- match_combined %>% mutate(B365H_Normal =B365H/B365_sum , B365D_Normal =B365D/B365_sum , B365A_Normal= B365A/B365_sum )
match_combined <- match_combined %>% mutate(BWH_Normal =BWH/BW_sum , BWD_Normal =BWD /BW_sum , BWA_Normal= BWA/BW_sum )
match_combined <- match_combined %>% mutate(IWH_Normal =IWH/IW_sum , IWD_Normal =IWD /IW_sum , IWA_Normal= IWA/IW_sum )
match_combined <- match_combined %>% mutate(PSH_Normal =PSH/PS_sum , PSD_Normal =PSD /PS_sum , PSA_Normal= PSA/PS_sum )
match_combined <-match_combined %>% select(- BW_sum, -IW_sum, - PS_sum,-B365_sum)

###For B365
match_B365 <- match_combined %>% select(FTR, B365H_Normal, B365D_Normal,B365A_Normal, HR,AR )
match_B365 <- match_B365 %>% mutate(H_A_interval = cut( (match_B365$B365H_Normal-match_B365$B365A_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_B365, aes(x= (B365H_Normal-B365A_Normal), y= B365D_Normal, col=H_A_interval))+
  labs(title="B365 Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_B365_draw_count <- match_B365 %>% group_by(H_A_interval) %>% count(FTR)
match_B365_draw_count <- match_B365_draw_count %>%rename(draw_count = n)
match_B365_draw_count <- match_B365_draw_count %>% filter(FTR =="D")
match_B365_draw_count<- match_B365_draw_count %>% select(-FTR)

match_B365_count <-match_B365 %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_B365_count <- full_join(match_B365_count,match_B365_draw_count, by="H_A_interval")  
match_B365_count <- match_B365_count %>% replace_na(list(draw_count=0))

match_B365_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_B365, aes(x= (B365H_Normal-B365A_Normal), y= B365D_Normal, col=H_A_interval))+
  labs(title="B365 Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+ 
  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_B365_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)


###For BW
match_BW <- match_combined %>% select(FTR, BWH_Normal, BWD_Normal,BWA_Normal, HR,AR )
match_BW <- match_BW %>% mutate(H_A_interval = cut( (match_BW$BWH_Normal-match_BW$BWA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_BW, aes(x= (BWH_Normal-BWA_Normal), y= BWD_Normal, col=H_A_interval))+
  labs(title="BW Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_BW_draw_count <- match_BW %>% group_by(H_A_interval) %>% count(FTR)
match_BW_draw_count <- match_BW_draw_count %>%rename(draw_count = n)
match_BW_draw_count <- match_BW_draw_count %>% filter(FTR =="D")
match_BW_draw_count<- match_BW_draw_count %>% select(-FTR)

match_BW_count <-match_BW %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_BW_count <- full_join(match_BW_count,match_BW_draw_count, by="H_A_interval")  
match_BW_count <- match_BW_count %>% replace_na(list(draw_count=0))

match_BW_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_BW, aes(x= (BWH_Normal-BWA_Normal), y= BWD_Normal, col=H_A_interval))+
  labs(title="BW Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_BW_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

###For IW
match_IW <- match_combined %>% select(FTR, IWH_Normal, IWD_Normal,IWA_Normal, HR,AR )
match_IW <- match_IW %>% mutate(H_A_interval = cut( (match_IW$IWH_Normal-match_IW$IWA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_IW, aes(x= (IWH_Normal-IWA_Normal), y= IWD_Normal, col=H_A_interval))+
  labs(title="IW Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_IW_draw_count <- match_IW %>% group_by(H_A_interval) %>% count(FTR)
match_IW_draw_count <- match_IW_draw_count %>%rename(draw_count = n)
match_IW_draw_count <- match_IW_draw_count %>% filter(FTR =="D")
match_IW_draw_count<- match_IW_draw_count %>% select(-FTR)

match_IW_count <-match_BW %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_IW_count <- full_join(match_IW_count,match_IW_draw_count, by="H_A_interval")  
match_IW_count <- match_IW_count %>% replace_na(list(draw_count=0))

match_IW_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_IW, aes(x= (IWH_Normal-IWA_Normal), y= IWD_Normal, col=H_A_interval))+
  labs(title="IW Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_IW_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

###For PS
match_PS <- match_combined %>% select(FTR, PSH_Normal, PSD_Normal,PSA_Normal, HR,AR )
match_PS <- match_PS %>% mutate(H_A_interval = cut( (match_PS$PSH_Normal-match_PS$PSA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_PS, aes(x= (PSH_Normal-PSA_Normal), y= PSD_Normal, col=H_A_interval))+
  labs(title="PS Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_PS_draw_count <- match_PS %>% group_by(H_A_interval) %>% count(FTR)
match_PS_draw_count <- match_PS_draw_count %>%rename(draw_count = n)
match_PS_draw_count <- match_PS_draw_count %>% filter(FTR =="D")
match_PS_draw_count<- match_PS_draw_count %>% select(-FTR)

match_PS_count <-match_PS %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_PS_count <- full_join(match_PS_count,match_PS_draw_count, by="H_A_interval")  
match_PS_count <- match_PS_count %>% replace_na(list(draw_count=0))

match_PS_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_PS, aes(x= (PSH_Normal-PSA_Normal), y= PSD_Normal, col=H_A_interval))+
  labs(title="PS Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_PS_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

#########################################################################################TASK 3#############################################################
#Manipulations

match_2018_filtered <- match_2018 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR) %>% filter( HR ==0, AR == 0)
match_2019_filtered <- match_2019 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR)%>% filter( HR ==0, AR == 0)
match_2020_filtered <- match_2020 %>% select(FTR, B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,HR,AR)%>% filter( HR ==0, AR == 0)
match_combined <- bind_rows(match_2018_filtered,match_2019_filtered, match_2020_filtered)

match_combined<-cbind( match_combined[,1] ,apply(match_combined[,2:13],c(1,2), function(x) { 1/x}), match_combined[,c(14,15)] )
match_combined <- match_combined %>% mutate(B365_sum = apply(match_combined[,2:4],1, FUN = sum ))
match_combined <- match_combined %>% mutate(BW_sum = apply(match_combined[,5:7],1, FUN = sum ))
match_combined <- match_combined %>% mutate(IW_sum = apply(match_combined[,8:10],1, FUN = sum ))
match_combined <- match_combined %>% mutate(PS_sum = apply(match_combined[,11:13],1, FUN = sum ))

match_combined <- match_combined %>% mutate(B365H_Normal =B365H/B365_sum , B365D_Normal =B365D/B365_sum , B365A_Normal= B365A/B365_sum )
match_combined <- match_combined %>% mutate(BWH_Normal =BWH/BW_sum , BWD_Normal =BWD /BW_sum , BWA_Normal= BWA/BW_sum )
match_combined <- match_combined %>% mutate(IWH_Normal =IWH/IW_sum , IWD_Normal =IWD /IW_sum , IWA_Normal= IWA/IW_sum )
match_combined <- match_combined %>% mutate(PSH_Normal =PSH/PS_sum , PSD_Normal =PSD /PS_sum , PSA_Normal= PSA/PS_sum )
match_combined <-match_combined %>% select(- BW_sum, -IW_sum, - PS_sum,-B365_sum)

###For B365
match_B365 <- match_combined %>% select(FTR, B365H_Normal, B365D_Normal,B365A_Normal, HR,AR )
match_B365 <- match_B365 %>% mutate(H_A_interval = cut( (match_B365$B365H_Normal-match_B365$B365A_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_B365, aes(x= (B365H_Normal-B365A_Normal), y= B365D_Normal, col=H_A_interval))+
  labs(title="B365 Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_B365_draw_count <- match_B365 %>% group_by(H_A_interval) %>% count(FTR)
match_B365_draw_count <- match_B365_draw_count %>%rename(draw_count = n)
match_B365_draw_count <- match_B365_draw_count %>% filter(FTR =="D")
match_B365_draw_count<- match_B365_draw_count %>% select(-FTR)

match_B365_count <-match_B365 %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_B365_count <- full_join(match_B365_count,match_B365_draw_count, by="H_A_interval")  
match_B365_count <- match_B365_count %>% replace_na(list(draw_count=0))

match_B365_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_B365, aes(x= (B365H_Normal-B365A_Normal), y= B365D_Normal, col=H_A_interval))+
  labs(title="B365 Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_B365_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)


###For BW
match_BW <- match_combined %>% select(FTR, BWH_Normal, BWD_Normal,BWA_Normal, HR,AR )
match_BW <- match_BW %>% mutate(H_A_interval = cut( (match_BW$BWH_Normal-match_BW$BWA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_BW, aes(x= (BWH_Normal-BWA_Normal), y= BWD_Normal, col=H_A_interval))+
  labs(title="BW Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_BW_draw_count <- match_BW %>% group_by(H_A_interval) %>% count(FTR)
match_BW_draw_count <- match_BW_draw_count %>%rename(draw_count = n)
match_BW_draw_count <- match_BW_draw_count %>% filter(FTR =="D")
match_BW_draw_count<- match_BW_draw_count %>% select(-FTR)

match_BW_count <-match_BW %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_BW_count <- full_join(match_BW_count,match_BW_draw_count, by="H_A_interval")  
match_BW_count <- match_BW_count %>% replace_na(list(draw_count=0))

match_BW_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_BW, aes(x= (BWH_Normal-BWA_Normal), y= BWD_Normal, col=H_A_interval))+
  labs(title="BW Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_BW_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

###For IW
match_IW <- match_combined %>% select(FTR, IWH_Normal, IWD_Normal,IWA_Normal, HR,AR )
match_IW <- match_IW %>% mutate(H_A_interval = cut( (match_IW$IWH_Normal-match_IW$IWA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_IW, aes(x= (IWH_Normal-IWA_Normal), y= IWD_Normal, col=H_A_interval))+
  labs(title="IW Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_IW_draw_count <- match_IW %>% group_by(H_A_interval) %>% count(FTR)
match_IW_draw_count <- match_IW_draw_count %>%rename(draw_count = n)
match_IW_draw_count <- match_IW_draw_count %>% filter(FTR =="D")
match_IW_draw_count<- match_IW_draw_count %>% select(-FTR)

match_IW_count <-match_BW %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_IW_count <- full_join(match_IW_count,match_IW_draw_count, by="H_A_interval")  
match_IW_count <- match_IW_count %>% replace_na(list(draw_count=0))

match_IW_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_IW, aes(x= (IWH_Normal-IWA_Normal), y= IWD_Normal, col=H_A_interval))+
  labs(title="IW Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_IW_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

###For PS
match_PS <- match_combined %>% select(FTR, PSH_Normal, PSD_Normal,PSA_Normal, HR,AR )
match_PS <- match_PS %>% mutate(H_A_interval = cut( (match_PS$PSH_Normal-match_PS$PSA_Normal),breaks = seq(-0.821,0.899,length.out=21)))


ggplot(match_PS, aes(x= (PSH_Normal-PSA_Normal), y= PSD_Normal, col=H_A_interval))+
  labs(title="PS Probability Calculations", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") 

match_PS_draw_count <- match_PS %>% group_by(H_A_interval) %>% count(FTR)
match_PS_draw_count <- match_PS_draw_count %>%rename(draw_count = n)
match_PS_draw_count <- match_PS_draw_count %>% filter(FTR =="D")
match_PS_draw_count<- match_PS_draw_count %>% select(-FTR)

match_PS_count <-match_PS %>% group_by(H_A_interval) %>% summarise( H_A_interval = H_A_interval[1], total_count = n() ) 

match_PS_count <- full_join(match_PS_count,match_PS_draw_count, by="H_A_interval")  
match_PS_count <- match_PS_count %>% replace_na(list(draw_count=0))

match_PS_count$H_A_interval<-seq(-0.7757368,0.8537368,length.out=20) #assign mid points for plotting

ggplot(match_PS, aes(x= (PSH_Normal-PSA_Normal), y= PSD_Normal, col=H_A_interval))+
  labs(title="PS Probability Calculations with Estimated Probability of Draws", x =" P(home win) – P(away win)", y = " P (tie)")+  scale_colour_discrete("P(Home)-P(Away)")+ theme_bw() +
  geom_point() + geom_smooth(color="black",se=FALSE,size=2,linetype = "dashed") + 
  geom_col(data=match_PS_count, aes(x=H_A_interval, y= (draw_count/total_count)),col="red",alpha=0.3)

