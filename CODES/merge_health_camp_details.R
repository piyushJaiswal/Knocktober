library(data.table)

train = read.csv("../DERIVED/train_with_Outcome.csv")
train = data.table(train)
any(is.na(train))
summary(train)
sapply(train,class)
train[,Registration_Date:=as.Date(Registration_Date)]

#.............................
test = read.csv("../RAW/Test_D7W1juQ.csv")
test = data.table(test)
any(is.na(test))

#.............................
health_camp <- read.csv("../RAW/Health_Camp_Detail.csv")
health_camp <- data.table(health_camp)

summary(health_camp)
sapply(health_camp, class)

health_camp[,":="(Camp_Start_Date = as.Date(Camp_Start_Date),
                  Camp_End_Date = as.Date(Camp_End_Date))]
health_camp$Category3 = as.factor(health_camp$Category3)

#.............................
train = merge(train, health_camp, by = c("Health_Camp_ID"), all.x=T)
any(is.na(train))

test = merge(test, health_camp, by = c("Health_Camp_ID"), all.x=T)
any(is.na(test))
#.............................

write.csv(train, file="../DERIVED/train_outcome_camp_details.csv", row.names = F)
write.csv(test, file="../DERIVED/test_camp_details.csv", row.names = F)
