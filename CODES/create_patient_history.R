library(data.table)
source("lib.R")

train = read.csv("../DERIVED/train_all_merged.csv")
train = data.table(train)

test = read.csv("../DERIVED/test_all_merged.csv")
test = data.table(test)

sapply(train,class)
sapply(test,class)

#................................
train <- registration.around.campStart(df=train)
train <- num.repeated.registration(df = train)
train <- time.of.association(df=train)
train <- total.registrations.camp(df=train)

summary(train)
sapply(train,class)

write.csv(train,file="../DERIVED/train_hist_feat_merged.csv", row.names=F)

#................................
test <- registration.around.campStart(df=test)
test <- num.repeated.registration(df = test)
test <- time.of.association(df=test)
test <- total.registrations.camp(df=test)

sapply(test,class)

write.csv(test,file="../DERIVED/test_hist_feat_merged.csv", row.names=F)

#.....................................
#.....................................
train.orig = copy(train)
dates = sort(unique(train$Camp_Start_Date))
val <- train[Camp_Start_Date %in% dates[31:40],]
train <- train[Camp_Start_Date %in% dates[1:30],]

drop.cols = c("Patient_ID", "Health_Camp_ID", "Registration_Date",
              "Camp_Start_Date", "Camp_End_Date", "First_Interaction", 
              "Employer_Category", "time_association")
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

