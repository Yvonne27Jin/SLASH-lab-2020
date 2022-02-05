

## recalculate drinking data with manually cleaned raw data
## 3.25





library(readxl)
library(tidyverse)
library(tidyr)
drinking <- read_excel("alcohol_cleaned.xlsx")



drinking <- drinking %>% filter(Eli1 == 2 & Eli2 == 1 & agree == 1)
glimpse(drinking)

## drinking 


# 1- weekly, 2 - per month, 3 - per year
drinking$alcohol1_2_1 <- as.numeric(drinking$alcohol1_2_1)
drinking$alcohol1_1_1_1 <- as.numeric(drinking$alcohol1_1_1_1)

#Drinking <- parti_lablled %>% select(starts_with("alcohol"), "last7days_alcohol_F", "MarApr_alcohol5_F")

# calculate frequency 
drinking <- drinking %>% mutate(
  freq_fac =  case_when(alcohol1_2_1 == "3" ~ 1/52, 
                        alcohol1_2_1 == "2" ~ 1/4, 
                        alcohol1_2_1 == "1" ~ 1),
  alcohol_freq = (alcohol1_1_1_1)*freq_fac
)

# time 1 
summary(drinking$alcohol_freq)
var(drinking$alcohol_freq,na.rm = T)

# March & April
summary(as.integer(drinking$alcohol1_F))
var(drinking$alcohol1_F,na.rm = T)

# last week
summary(as.integer(drinking$last7days_alcohol_F))
var(drinking$last7days_alcohol_F,na.rm = T)



drinking <- drinking %>% cbind(drinking_freq_time1 = drinking$alcohol_freq, 
                                         drinking_freq_MarApr = drinking$alcohol1_F, 
                                         drinking_freq_lastweek =drinking$last7days_alcohol_F)



##binge drinking (per week)


Binge_drinking <- drinking %>% mutate(binge_lastyear = as.integer(alcohol2)/52,
                                      binge_mar_apr = as.integer(MarApr_alcohol5_F)/8,
                                      binge_7d = as.integer(alcohol2_F)) 

# time 1 (last year?)

summary(Binge_drinking$binge_lastyear)
var(Binge_drinking$binge_lastyear,na.rm = T)


# Mar & Apr


summary(Binge_drinking$binge_mar_apr)
var(Binge_drinking$binge_mar_apr,na.rm = T)

# last week

summary(Binge_drinking$binge_7d)
var(Binge_drinking$binge_7d,na.rm = T)


drinking <- drinking %>% cbind(binge_drinking_time1 = Binge_drinking$binge_lastyear, 
                                         binge_drinking_MarApr = Binge_drinking$binge_mar_apr, 
                                         binge_drinking_lastweek =Binge_drinking$binge_7d)



write.csv(drinking,"03.25_alcohol_recalculated.csv")
