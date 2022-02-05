

## 2021.3.11 re-calculaing BMI 

library(foreign)
library(haven)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lavaan)
library(apaTables)

df_labelled <- read.spss("merged_Survey on Sleep Health+followedup.sav",use.value.labels = T,to.data.frame = T)

parti_lablled <- df_labelled %>% filter(Eli1 == "Yes" & Eli2 == "Yes" & agree == "Yes")

BMI_cleaned <- parti_lablled[c("SCOV_ID")]

#note down the metrics and number
BMI_cleaned <- BMI_cleaned %>% cbind(h_metric = parti_lablled$height_2_1, 
                                     height_raw = parti_lablled$height_1_1_1, 
                                     w1_metric = parti_lablled$weight_2_1,
                                     weight_1_raw = parti_lablled$weight_1_1_1,
                                     w2_metric = parti_lablled$weight_2_1_F,
                                     weight_2_raw = parti_lablled$weight_1_1_1_F)



#--------------------------
  
# read in SPSS data with original (numerical) values
df <- read_spss("merged_Survey on Sleep Health+followedup.sav")

df_1 <- df %>% filter(Eli1 == 2 & Eli2 == 1 & agree == 1)
# 1633 -> 1387

# change unit to kg and m
  
# height: convert feet to m
# 1 feet = 30.48 cm; cm -> 80.01 -> m
df_1 <- df_1 %>% mutate(Height = ifelse(height_2_1 == 1, height_1_1_1*0.01, height_1_1_1 * 30.48*0.01)) 

# weight: convert pound to kg
# 1 pound = 0.45359237 kg
df_1 <- df_1 %>% mutate(Weight = ifelse(weight_2_1 == 2, weight_1_1_1, weight_1_1_1 * 0.45359237)) 


# Weight_F:time point 2
df_1 <- df_1 %>% mutate(Weight_F = ifelse(weight_2_1_F == 2, weight_1_1_1_F, weight_1_1_1_F * 0.45359237)) 

# add to BMI data frame 
# the one with capitcal in the first letter is after unit-change.
BMI_cleaned <- BMI_cleaned %>% cbind(Height = df_1$Height, 
                                     Weight_1 = df_1$Weight,
                                     Weight_2 = df_1$Weight_F)

# write out the calculated dataframe
write.csv(BMI_cleaned, "R_recalculated_BMI_need_check_0311.csv",row.names = F, fileEncoding = "UTF-8")





#-----------------------------
# check manually the abnormal height and weight data
# import checked database
library(readxl)
data_checked <- read_excel("R_recalculated_BMI_need_check_0312.xlsx")





# calculate BMI
  # BMI = kg/m^2 
  
data_checked$BMI_1 = BMI_cleaned$Weight_1/(BMI_cleaned$Height*BMI_cleaned$Height)
data_checked$BMI_2 = BMI_cleaned$Weight_2/(BMI_cleaned$Height*BMI_cleaned$Height)





#BMI <- read.csv("condition_BMI.csv")
#COVID_cleaned$BMI <- BMI$BMI
# 986 VS 1386
