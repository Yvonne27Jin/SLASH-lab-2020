library(tidyLPA)
library(tidyverse)
library(dplyr)
library(mclust)
library(reshape2)
library(readr)
setwd("C:/Users/jinya/OneDrive - connect.hku.hk/SLASH Lab/COVID and Sleep data/5.20")
COVID <- read_csv("COVID_merged_time3_0527.csv")
View(COVID)
names(COVID)


########################################## 3 time points ################

#Getting data prepared (Item level)
SeepWakeEnmeshment<-COVID[,c(35,36,55,31,33,53,70:74)] #35,36,55 -> exercise; 31,33,53 -> PSQI; 70:74 -> COVID9
  SeepWakeEnmeshment_rm<-na.omit(SeepWakeEnmeshment) # only 179 obs if taking 3 time points into account (336 at time 3)
  SeepWakeEnmeshment_rm_scale<-scale(SeepWakeEnmeshment_rm)
  
  #write.csv(SeepWakeEnmeshment, "SleepwakeEnmeshment_rm_0528.csv")

  
  sink("LPA result.txt",append = T, split = T)
  
  
  #m3<-estimate_profiles(SeepWakeEnmeshment_rm_scale,1:3,variances = "equal", covariances = "zero")

#Best fit model - BIC
  BIC<-mclustBIC(SeepWakeEnmeshment_rm_scale)
  plot(BIC)
  summary(BIC)
  
  
  model_EEI6<-Mclust(SeepWakeEnmeshment_rm_scale,modelNames="EEI",G=6,x=BIC)
  summary(model_EEI6)  
  model_EEI9<-Mclust(SeepWakeEnmeshment_rm_scale,modelNames="EEI",G=9,x=BIC)
  summary(model_EEI9)
  model_EEI8<-Mclust(SeepWakeEnmeshment_rm_scale,modelNames="EEI",G=8,x=BIC)
  summary(model_EEI8)
 

#Best fit model - ICL
  ICL<-mclustICL(SeepWakeEnmeshment_rm_scale)
  plot(ICL)
  summary(ICL)
  

#Best fit model - BLRT 
  #mclustBootstrapLRT(SeepWakeEnmeshment_rm_scale,modelName = "EEI")
  ## error?

#Visualize LPA
  
  
  mean_EEI6<-data.frame(model_EEI6$parameters$mean,stringsAsFactors = FALSE)
  mean_EEI6<-rownames_to_column(mean_EEI6)
  mean_EEI6_melt<-reshape2::melt(mean_EEI6,id="rowname")
  names(mean_EEI6_melt)<-c("item","cluster","mean")
  
  ggplot(mean_EEI6_melt, aes(x=item,y=mean,group=cluster,color=cluster))+
    geom_point(size=1.5)+
    geom_line(size=0.75)+
    theme(axis.text.x=element_text(angle=45))
  
  
  sink()
  
  
  ###################
  
  mean_EEI9<-data.frame(model_EEI9$parameters$mean,stringsAsFactors = FALSE)
  mean_EEI9<-rownames_to_column(mean_EEI9)
  mean_EEI9_melt<-reshape2::melt(mean_EEI9,id="rowname")
  names(mean_EEI9_melt)<-c("item","cluster","mean")
  
  ggplot(mean_EEI9_melt, aes(x=item,y=mean,group=cluster,color=cluster))+
    geom_point(size=1.5)+
    geom_line(size=0.75)+
    theme(axis.text.x=element_text(angle=45))
  
  mean_EEI8<-data.frame(model_EEI8$parameters$mean,stringsAsFactors = FALSE)
  mean_EEI8<-rownames_to_column(mean_EEI8)
  mean_EEI8_melt<-reshape2::melt(mean_EEI8,id="rowname")
  names(mean_EEI8_melt)<-c("item","cluster","mean")
  
  ggplot(mean_EEI8_melt, aes(x=item,y=mean,group=cluster,color=cluster))+
    geom_point(size=1.5)+
    geom_line(size=0.75)+
    theme(axis.text.x=element_text(angle=45))
  



  
  ##################################################### 2 time points ####################

  
  #Getting data prepared (Item level)
  SeepWakeEnmeshment2<-COVID[,c(35,36,31,33,70:74)] #35,36 -> exercise; 31,33-> PSQI; 70:74 -> COVID9
  SeepWakeEnmeshment2_rm<-na.omit(SeepWakeEnmeshment2) # 254 obs if taking 2 time points into account
  SeepWakeEnmeshment2_rm_scale<-scale(SeepWakeEnmeshment2_rm)
  
  sink("LPA result.txt",append = T, split = T)
  
  
  #Best fit model - BIC
  BIC<-mclustBIC(SeepWakeEnmeshment2_rm_scale)
  plot(BIC)
  summary(BIC)
  
  model_VEI5<-Mclust(SeepWakeEnmeshment2_rm_scale,modelNames="VEI",G=5,x=BIC)
  summary(model_VEI5)  
  model_VEI9<-Mclust(SeepWakeEnmeshment2_rm_scale,modelNames="VEI",G=9,x=BIC)
  summary(model_VEI9)
  model_VEI4<-Mclust(SeepWakeEnmeshment2_rm_scale,modelNames="VEI",G=4,x=BIC)
  summary(model_VEI4)
  
  #Best fit model - ICL
  ICL<-mclustICL(SeepWakeEnmeshment2_rm_scale)
  plot(ICL)
  summary(ICL)
  
  sink()
  
  #Best fit model - BLRT 
  mclustBootstrapLRT(SeepWakeEnmeshment2_rm_scale,modelName = "VEI")
  ## error?

  
  model_VEI5<-data.frame(model_VEI5$parameters$mean,stringsAsFactors = FALSE)
  model_VEI5<-rownames_to_column(model_VEI5)
  model_VEI5_melt<-reshape2::melt(model_VEI5,id="rowname")
  names(model_VEI5_melt)<-c("item","cluster","mean")
  
  ggplot(model_VEI5_melt, aes(x=item,y=mean,group=cluster,color=cluster))+
    geom_point(size=1.5)+
    geom_line(size=0.75)+
    theme(axis.text.x=element_text(angle=45))

