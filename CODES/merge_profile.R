library(data.table)

train = read.csv("../DERIVED/train_outcome_camp_details.csv")
train = data.table(train)

train[,":="(Camp_Start_Month = as.integer(strftime(Camp_Start_Date,"%m")),
            Camp_Start_wkDay = as.integer(strftime(Camp_Start_Date,"%u")),
            Registration_Month = as.integer(strftime(Registration_Date,"%m")),
            Registration_wkDay = as.integer(strftime(Registration_Date,"%u")))]
summary(train)
#....................................
profile = read.csv("../RAW/Patient_Profile.CSV")
profile = data.table(profile)

summary(profile)
sapply(profile, class)

cols_withNA = c("Income","Education_Score","Age")
profile[,(cols_withNA):=lapply(.SD,function(x){x[x=="None"]=NA
                                return (x)}), .SDcols = cols_withNA]

cols_withBlank = c("City_Type","Employer_Category")
profile[,(cols_withBlank):=lapply(.SD,function(x){x[x==""]=NA
                                      return (x)}), .SDcols = cols_withBlank]

profile[,":="(Education_Score = as.numeric(as.character(Education_Score)),
               Age = as.integer(as.character(Education_Score)))]
summary(profile)
#.....................................
train = merge(train, profile, by = c("Patient_ID"))

write.csv(train,file="../DERIVED/train_all_merged.csv", row.names = F)
#.....................................
train.orig = copy(train)
dates = sort(unique(train$Camp_Start_Date))
val <- train[Camp_Start_Date %in% dates[31:40],]
train <- train[Camp_Start_Date %in% dates[1:30],]

drop.cols = c("Patient_ID", "Health_Camp_ID", "Registration_Date",
              "Camp_Start_Date", "Camp_End_Date", "First_Interaction")
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

param <- list(max_depth = 6, 
              eta = 0.01, 
              silent = 1,
              objective="binary:logistic",
              eval_metric="auc",
              # subsample = 0.75,
              min_child_weight = 20,
              colsample_bytree = 0.5)
model_xgb <- xgboost(data=as.matrix(x_train),label = y_train, nrounds = 500,
                     params = param, verbose = 2, missing = 111222333)
imp_xgb = xgb.importance(model = model_xgb, feature_names = colnames(x_train))
write.csv(imp_xgb, file = "../MODEL/imp_xgb.csv", row.names=F)

#......................................
library(pROC)

preds_xgb = predict(model_xgb,newdata = as.matrix(x_val))
auc_xgb = auc(y_val,preds_xgb)
print(auc_xgb)

preds_xgb[preds_xgb>0.2727756]=1
preds_xgb[preds_xgb<0.2727756]=0

#......................................
library(caret)
c = confusionMatrix(preds_xgb,y_val,positive = "1")


