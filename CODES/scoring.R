library(data.table)

test = read.csv("../DERIVED/test_hist_feat_merged.csv")
test = data.table(test)

#...............................
test.orig = copy(test)
drop.cols = c("Patient_ID", "Health_Camp_ID", "Registration_Date",
              "Camp_Start_Date", "Camp_End_Date", "First_Interaction", 
              "Employer_Category", "time_association")
test <- subset(test,select = !(colnames(test)%in% drop.cols))

#...............................
x_test = copy(test)
cols.fac <- names(which(sapply(test,class)=="factor"))
x_test <- x_test[,(cols.fac):=lapply(.SD,function(x) as.numeric(x)), .SDcols=cols.fac]
x_test[is.na(x_test)] = 111222333

test.xg <- xgb.DMatrix(as.matrix((x_test)), missing=111222333)

#...............................
preds_xgb_test = predict(model_xgb,newdata = test.xg, ntreelimit=model_xgb$bestInd,
                         missing=111222333)
test.orig[,Outcome:=preds_xgb_test]

#...............................
scored <- subset(test.orig, select = c(Patient_ID,Health_Camp_ID,Outcome))

write.csv(scored, file="../DERIVED/Submission_V3.csv", row.names=F)

