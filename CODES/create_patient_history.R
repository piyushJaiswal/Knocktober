library(data.table)
source("lib.R")

train = read.csv("../DERIVED/train_all_merged.csv")
train = data.table(train)
train <- train[!(is.na(Registration_Date)),]

test = read.csv("../DERIVED/test_all_merged.csv")
test = data.table(test)

sapply(train,class)
sapply(test,class)

#................................
train <- registration.around.campStart(df=train)
train <- num.repeated.registration(df = train)
train <- time.of.association(df=train)
train <- total.registrations.camp(df=train)
train <- patient.outcome.sum(df=train)

train$con_rate <- train$patient_outcome_sum/ train$registration_num_overall 
train$con_rate[is.infinite(train$con_rate)] <- -1
train[,patient_outcome_sum:=NULL]

train <- consecutive.camps.interval(df = train)

head(train)

summary(train)
sapply(train,class)

write.csv(train,file="../DERIVED/train_hist_feat_merged.csv", row.names=F)

#................................
test <- registration.around.campStart(df=test)
test <- num.repeated.registration(df = test)
test <- time.of.association(df=test)
test <- total.registrations.camp(df=test)

dat_common<- subset(train, Patient_ID %in% unique(test$Patient_ID))
setorder(dat_common, "Patient_ID",-"Registration_Date")
dat_common[,tmp_ind:=1:.N,by = "Patient_ID"]
s = dat_common[tmp_ind==1,c("Patient_ID","con_rate"),with=F]

test <- merge(test, s, by = "Patient_ID", all.x=T)
test[registration_num_overall==0,con_rate:=-1]

sapply(test,class)

write.csv(test,file="../DERIVED/test_hist_feat_merged.csv", row.names=F)

#....................................
train[,tag:="train"]
test[,tag:="test"]
test[,Outcome:=0]
dat_common <- rbind(train,test)

dat_common <- consecutive.camps.interval(df = dat_common)
train = dat_common[tag=="train",]
test <- dat_common[tag=="test",]
test[,Outcome:=NULL]
train[,tag:=NULL]
test[,tag:=NULL]

write.csv(train,file="../DERIVED/train_hist_feat_merged.csv", row.names=F)
write.csv(test,file="../DERIVED/test_hist_feat_merged.csv", row.names=F)

#.....................................
#.....................................
train.orig = copy(train)
dates = sort(unique(train$Camp_Start_Date))
val <- train[Camp_Start_Date %in% dates[31:40],]
train <- train[Camp_Start_Date %in% dates[1:30],]

drop.cols = c("Patient_ID", "Health_Camp_ID", "Registration_Date",
              "Camp_Start_Date", "Camp_End_Date", "First_Interaction", 
              "Employer_Category")
train <- subset(train,select = !(colnames(train)%in% drop.cols))
val <- subset(val,select = !(colnames(val)%in% drop.cols))

#.....................................
library(xgboost)
y_train <- train$Outcome
x_train = copy(train)

cols.fac <- names(which(sapply(train,class)=="factor"))
x_train <- x_train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_train[,Outcome:=NULL]

y_val <- val$Outcome
x_val = copy(val)

x_val <- x_val[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_val[,Outcome:=NULL]

x_train[is.na(x_train)] = 111222333
x_val[is.na(x_val)] = 111222333

param <- list(max_depth = 5, 
              eta = 0.002, 
              silent = 1,
              objective="binary:logistic",
              eval_metric="auc",
              # subsample = 0.75,
              min_child_weight = 15,
              colsample_bytree = 0.75)
train.xg <- xgb.DMatrix(as.matrix((x_train)), label=y_train, missing=111222333)
test.xg <- xgb.DMatrix(as.matrix((x_val)), label=y_val, missing=111222333)
watchlist <- list(test = test.xg,train = train.xg)

model_xgb <- xgb.train(data=train.xg, nrounds = 1500,
                     params = param, verbose = 2, missing = 111222333, 
                     #early.stop.round = 500, 
                     watchlist = watchlist, 
                     maximize = T)
imp_xgb = xgb.importance(model = model_xgb, feature_names = colnames(x_train))
write.csv(imp_xgb, file = "../MODEL/imp_xgb.csv", row.names=F)

#......................................
library(pROC)

preds_xgb = predict(model_xgb,newdata = test.xg, ntreelimit=model_xgb$bestInd,
                    missing=111222333)
auc_xgb = auc(y_val,preds_xgb)
print(auc_xgb)

preds_xgb[preds_xgb>0.3]=1
preds_xgb[preds_xgb<0.3]=0

#......................................
library(caret)
c = confusionMatrix(preds_xgb,y_val,positive = "1")

#......................................
train = copy(train.orig)
y_train <- train$Outcome
x_train = copy(train)

cols.fac <- names(which(sapply(train,class)=="factor"))
x_train <- x_train[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_train[,Outcome:=NULL]
x_train[is.na(x_train)] = 111222333

train.xg <- xgb.DMatrix(as.matrix((x_train)), label=y_train, missing=111222333)
model_xgb <- xgb.train(data=train.xg, nrounds = 1500,
                       params = param, verbose = 2, missing = 111222333, 
                       #early.stop.round = 500, 
                       watchlist = watchlist, 
                       maximize = T)
