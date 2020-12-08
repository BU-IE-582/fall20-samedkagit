#IE 582 / HW2
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
library(plot3D)
#Data Reading

train_x <- fread("C:/Users/Abdulsamed/Desktop/IE582/uWaveGestureLibrary_X_TRAIN")
train_y <- fread("C:/Users/Abdulsamed/Desktop/IE582/uWaveGestureLibrary_Y_TRAIN")
train_z <- fread("C:/Users/Abdulsamed/Desktop/IE582/uWaveGestureLibrary_Z_TRAIN")

#Column Names

colnames(train_x)[1] = "class"
newnames <- paste("t",1: (ncol(train_x)-1), sep = "")
colnames(train_x)[2:ncol(train_x)] = newnames

colnames(train_y)[1] = "class"
newnames <- paste("t",1: (ncol(train_y)-1), sep = "")
colnames(train_y)[2:ncol(train_y)] = newnames

colnames(train_z)[1] = "class"
newnames <- paste("t",1: (ncol(train_z)-1), sep = "")
colnames(train_z)[2:ncol(train_z)] = newnames

#Velocity DF

mySum = t(apply(train_x[,-1], 1, cumsum))
train_x_velo <- cbind(train_x[,1],mySum)

mySum = t(apply(train_y[,-1], 1, cumsum))
train_y_velo <- cbind(train_y[,1],mySum)

mySum = t(apply(train_z[,-1], 1, cumsum))
train_z_velo <- cbind(train_z[,1],mySum)

#Position DF

mySum = t(apply(train_x_velo[,-1], 1, cumsum))
train_x_pos <- cbind(train_x_velo[,1],mySum)

mySum = t(apply(train_y_velo[,-1], 1, cumsum))
train_y_pos <- cbind(train_y_velo[,1],mySum)

mySum = t(apply(train_z_velo[,-1], 1, cumsum))
train_z_pos <- cbind(train_z_velo[,1],mySum)

rm(mySum)

#Data Visualization

instances <- list()
for(i in 1:8)
{
instances=append(instances,list(cbind(t((train_x_pos%>%filter(class==i))[1,-1]),t((train_y_pos%>%filter(class==i))[1,-1]),t((train_z_pos%>%filter(class==i))[1,-1]))))
}


for(i in 1:8)
{
  a <- as.data.frame(unlist(instances[[i]]))
  scatter3D(x=a$V1,y=a$V2,z=a$V3, main=paste("An Instance From Class ", i))
  rm(a)
  }

#Longer Format

train_x_adj <- cbind(time_series_id=1:nrow(train_x), train_x)
train_y_adj <- cbind(time_series_id=1:nrow(train_y), train_y)
train_z_adj <- cbind(time_series_id=1:nrow(train_z), train_z)

train_x_adj_long<-pivot_longer(train_x_adj,-contains("s"),names_to="time_index",values_to="x",names_prefix ="t",names_transform=list("time_index"=as.integer)) 
train_y_adj_long<-pivot_longer(train_y_adj,-contains("s"),names_to="time_index",values_to="y",names_prefix ="t",names_transform=list("time_index"=as.integer))
train_z_adj_long<-pivot_longer(train_z_adj,-contains("s"),names_to="time_index",values_to="z",names_prefix ="t",names_transform=list("time_index"=as.integer))

train_xyz_long <- inner_join(train_x_adj_long,train_y_adj_long,by=c("time_series_id","class","time_index"))
train_xyz_long <- inner_join(train_xyz_long,train_z_adj_long,by=c("time_series_id","class","time_index"))


#PCA

pca_xyz <- prcomp(train_xyz_long%>%select(x,y,z), scale=TRUE)
tidy(pca_xyz,"pcs") #0.49 of the variance kept
pca_xyz$rotation

train_xyz_long_pca <- cbind(train_xyz_long,score=(pca_xyz$x)[,1]) %>% select(-x,-y,-z)
set.seed(130)
for(i in 1:8)
{
  chosen <- train_xyz_long_pca %>% filter(class == i)
  chosen <- unique(chosen$time_series_id) %>% sample(size=2)
  chosen1 <- train_xyz_long_pca %>% filter(time_series_id == chosen[1])
  chosen2 <- train_xyz_long_pca %>% filter(time_series_id == chosen[2])
  chosen <- bind_rows(chosen1,chosen2)
  chosen$time_series_id = as.character(chosen$time_series_id)
  print(ggplot(chosen,aes(x=time_index,y=score,color=time_series_id))+geom_line()+labs(title=paste("2 Randomly Picked Sample From Class ",i),x="Time Index",y="PCA Score"))
  rm(chosen,chosen1,chosen2)
}

#PCA for each class

class_pca <- train_xyz_long %>% group_by(class) %>% arrange(class) %>% nest() %>% 
  mutate(pca=purrr::map(data,function(d) prcomp(d%>%select(x,y,z))), rotation_pca= purrr::map(pca,function(d) tidy(d,matrix="rotation")%>%filter(PC==1)), 
         eigenvalues_pca = purrr::map(pca, function(d) tidy(d,matrix="eigenvalues")%>%filter(PC==1) %>% select(-PC))) %>% 
  unnest(c(rotation_pca,eigenvalues_pca)) %>% select(class,column, value, cumulative,PC) %>% pivot_wider(names_from = column, values_from=value)

class_pca

#MDS

mds_matris<- inner_join(train_x_adj,train_y_adj,by=c("class","time_series_id"),suffix=c("_x","_y"))
mds_matris<- inner_join(mds_matris,train_z_adj,by=c("class","time_series_id"))

distance_matrix <- dist(mds_matris%>%select(-class,-time_series_id))

mds_fit <- cmdscale(distance_matrix,eig=TRUE, k=2)

x <- mds_fit$points[,1]
y <- mds_fit$points[,2]

mds_applied_ts <- cbind(x,y,mds_matris[,1:2])
mds_applied_ts$class = as.character(mds_applied_ts$class)
mycolor <- c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")


ggplot(mds_applied_ts,aes(x=x,y=y,color=class))+geom_point() + scale_color_manual(values=mycolor) + 
  labs(title = "MDS with 2 dimensions",x="Coordinate 1", y="Coordinate 2")

