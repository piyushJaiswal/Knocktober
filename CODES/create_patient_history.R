library(data.table)

train = read.csv("../DERIVED/train_all_merged.csv")
train = data.table(train)

sapply(train,class)

#................................
train[,First_Interaction := as.Date(train$First_Interaction, format = "%d-%b-%y")]
train[,Registration_Date := as.Date(Registration_Date)]

setorder(train,"Patient_ID","Registration_Date")
train[,tmp_ind:=1:.N,by="Patient_ID"]
train[,time_association:=0]
train[tmp_ind>1 & !(is.na(First_Interaction)),time_association:= Registration_Date - First_Interaction]

tmp = train[tmp_ind==1,c("Patient_ID","Registration_Date"),with=F]
setnames(tmp,"Registration_Date","tmp_first_interaction")

dim(train)
train = merge(train,tmp,by=c("Patient_ID"),all.x=T)
dim(train)
train[tmp_ind>1 & (is.na(First_Interaction)),time_association:= Registration_Date - tmp_first_interaction]
train[,":="(tmp_ind=NULL,tmp_first_interaction=NULL)]

#................................