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

patient.outcome.sum <- function(df){
  df <- data.table(df)
  setorder(df,"Patient_ID","Registration_Date")
  #df$patient_outcome_sum <- as.integer(-123)
  df[,patient_outcome_sum := (cumsum(Outcome)-Outcome), by=list(Patient_ID)]
  df$patient_outcome_sum[df$patient_outcome_sum == -123] <- NA
  df[registration_num_overall ==0,patient_outcome_sum:=NA]
  return (df)
}

consecutive.camps.interval <- function(df){
  
  df <- data.table(df)
  
  #get last date of ongoing camp in city
  city_camp <- unique(df[!(is.na(City_Type)),c("Health_Camp_ID","Camp_Start_Date","Category1","City_Type"),with=F])
  setorder(city_camp,"City_Type","Category1","Camp_Start_Date")
  city_camp[,tmp_ind := 1:.N,by = c("City_Type","Category1")]
  city_camp[,prev_Camp_Start_Date := c(NA,as.Date(Camp_Start_Date[-.N])),by = c("City_Type","Category1")]
  city_camp[,prev_Camp_Start_Date := as.Date(prev_Camp_Start_Date, origin = "1970-01-01")]
  city_camp[,last_camp_time_diff:= as.numeric(Camp_Start_Date - prev_Camp_Start_Date)]
  
  city_camp[is.na(last_camp_time_diff), last_camp_time_diff:=-1]
  #setorder(df,"city_type","Category1","Camp_Start_Date")
  
  df <- merge(df, city_camp[,c("City_Type","Health_Camp_ID","last_camp_time_diff"),with=F], 
              by = c("City_Type","Health_Camp_ID"), all.x=T )
  
  return(df)
}

