registration.around.campStart <- function(df){
  #1. Does he register much in advance, or during when the camp is going on
  df$Registration_Date <- as.Date(df$Registration_Date)
  df$Camp_Start_Date <- as.Date(df$Camp_Start_Date)
  df$registers_around_campStart <- as.numeric(df$Registration_Date - df$Camp_Start_Date)
  return (df)
}

num.repeated.registration <- function(df){
  setorder(df,"Patient_ID","Registration_Date")
  df[,registration_num_overall:=1:.N,by="Patient_ID"]
  df[,registration_num_overall:=registration_num_overall-1]
  
  setorder(df,"Patient_ID","Category1","Registration_Date")
  df[,registration_num_camp_cat:=1:.N,by=c("Patient_ID","Category1")]
  df[,registration_num_camp_cat:=registration_num_camp_cat-1]
  
  return (df)
}

time.of.association <- function(df){
  df[,First_Interaction := as.Date(df$First_Interaction, format = "%d-%b-%y")]
  df[,Registration_Date := as.Date(Registration_Date)]
  
  setorder(df,"Patient_ID","Registration_Date")
  df[,tmp_ind:=1:.N,by="Patient_ID"]
  df[,time_association:=0]
  df[tmp_ind>1 & !(is.na(First_Interaction)),time_association:= Registration_Date - First_Interaction]
  
  tmp = df[tmp_ind==1,c("Patient_ID","Registration_Date"),with=F]
  setnames(tmp,"Registration_Date","tmp_first_interaction")
  
  dim(df)
  df = merge(df,tmp,by=c("Patient_ID"),all.x=T)
  dim(df)
  df[tmp_ind>1 & (is.na(First_Interaction)),time_association:= Registration_Date - tmp_first_interaction]
  df[,":="(tmp_ind=NULL,tmp_first_interaction=NULL)]
  
  return(df)
}

total.registrations.camp <- function(df){

  setorder(df,"Health_Camp_ID","Registration_Date")
  df[,registration_camp_tillDate:=1:.N,by="Health_Camp_ID"]
  df[,registration_camp_tillDate:=registration_camp_tillDate-1]
  
  reg_total <- df[,list(registration_camp_total = .N),by = c("Health_Camp_ID")]
  df <- merge(df,reg_total,by="Health_Camp_ID",all.x=T)
  
  return(df)
}