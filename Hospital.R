#Predictive Analytics

setwd("D:/GIM/TERM 2/PPA/Hospital")


library(car)
library(corrplot)
#training data set
library(caret)
library(caTools)
#pair.panel chart
library(psych)
#clean column name
library(janitor)
library(dplyr)
#to standerdize data
library(standardize)
library(BBmisc)
#pair.panel chart
library(psych)


data=read.csv("Mission_Hospital.csv")
data=clean_names(data)

#converting to factor
data=strings2factors(data,which = c("gender","marital_status",
                                    "key_complaints_code","mode_of_arrival",
                                    "type_of_admsn","implant_used_y_n"))


#null value in key_compaints_code will be filled by more frequent one
na_count=sapply(data,function(data)   data.frame(sum(is.na(data))))

#replacing crtinine null value with median

data$creatinine=lapply(data$creatinine,function(x)
  {is.na(data$creatinine)=median(data$creatinine,na.rm = TRUE)})

#replacing urea null value by median

data$urea=lapply(data$urea,function(x)
{is.na(data$urea)=median(data$urea,na.rm = TRUE)})

#replacing hb null value by median
data$hb=lapply(data$urea,function(x)
{is.na(data$hb)=median(data$hb,na.rm = TRUE)})

data$high_bp=ifelse(data$bp_high>=140,1,0)
data$low_bp=ifelse(data$bp_low<=70,1,0)

#Required data frame

hospital_data=data %>% select(-c(gender,marital_status,key_complaints_code,
                                 past_medical_history_code,mode_of_arrival,
                                 state_at_the_time_of_arrival,alert,type_of_admsn,
                                 implant_used_y_n,concession,total_length_of_stay,
                                 ln_total_cost))

#Cost_to _hospital outliers



hospital_data_nor=normalize(hospital_data, method = "standardize", range = c(0, 1), 
          margin = 1L, on.constant = "quiet")

lm1=lm(total_cost_to_hospital~body_weight,data=hospital_data_nor)
summary(lm1)
plot(lm1)


lm2=lm(total_cost_to_hospital~body_weight+length_of_stay_icu,
       data=hospital_data_nor)
summary(lm2)
plot(lm2)



lm3=lm(total_cost_to_hospital~body_weight+length_of_stay_icu+length_of_stay_ward+
         cost_of_implant,data=hospital_data_nor)
summary(lm3)
plot(lm3)

#body_weight is not making significant contribution in model

lm4=lm(total_cost_to_hospital~length_of_stay_icu+length_of_stay_ward+
         cost_of_implant+diabetes2+body_height+low_bp,
         data=hospital_data_nor)
summary(lm4)
plot(lm4)


cr<- cor(data[c("age","body_weight","body_height","total_length_of_stay","length_of_stay_icu","cost_of_implant","total_cost_to_hospital")])
cr
corrplot(cr,method = "number")

#corr=cor(hospital_data(c("age","unmarried","body_weight","body_height",
#                             "urea","creatinine","ambulance","transferred",
#                             "elective","length_of_stay_icu","length_of_stay_ward",
#                            "implant","cost_of_implant","high_bp","low_bp",
#                            "total_cost_to_hospital")))






#lm1=lm(log(data$TOTAL_COST_TO_HOSPITAL)~data$BODY_WEIGHT)
#summary(lm1)
#plot(lm1)

#TOTAL_COST_TO_HOSPITAL=11.745190 + weight * 0.008442





