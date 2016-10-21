library(data.table)
library(ggplot2)

train <- read.csv("../RAW/Train.csv")
first_camp = read.csv("../RAW/First_Health_Camp_Attended.csv")
second_camp = read.csv("../RAw/Second_Health_Camp_Attended.csv")
third_camp = read.csv("../RAW/Third_Health_Camp_Attended.csv")

train = data.table(train)
first_camp = data.table(first_camp)
second_camp = data.table(second_camp)
third_camp = data.table(third_camp)

first_Camp = first_camp[,c("Patient_ID","Health_Camp_ID","Health_Score"), with=F]
second_camp = second_camp[,c("Patient_ID","Health_Camp_ID","Health.Score"), with=F]
third_camp = third_camp[,c("Patient_ID","Health_Camp_ID","Number_of_stall_visited"), with=F]
third_camp = subset(third_camp, Number_of_stall_visited>0)

colnames(first_Camp)[3]="Outcome"
colnames(second_camp)[3]="Outcome"
colnames(third_camp)[3]="Outcome"

all_camps = rbind(first_Camp,second_camp,third_camp)
all_camps$Outcome=1
summary(all_camps)

dim(train)

train = merge(train,all_camps, by = c("Patient_ID","Health_Camp_ID"), all.x=T)
sum(train$Outcome, na.rm=T)
