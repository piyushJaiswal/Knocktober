consecutive.camps.interval <- function(df){
  
  df <- data.table(df)
  #get last date of ongoing camp in city
  city_camp <- unique(df[!(is.na(city_type)),c("Health_Camp_ID","Camp_Start_Date","Category1","city_type"),with=F])
  setorder(city_camp,"city_type","Category1","Camp_Start_Date")
  city_camp[,tmp_ind = 1:.N,by = c("city_type","Category1")]
  city_camp[,prev_Camp_Start_Date := c(-1,Camp_Start_Date[-.N]),by = c("city_type","Category1")]
  
  city_camp[,last_camp_time_diff:= as.numeric(Camp_Start_Date - prev_Camp_Start_Date)]
  #setorder(df,"city_type","Category1","Camp_Start_Date")
  
  df <- merge(df, city_camp[,c("city_type","Health_Camp_ID","last_camp_time_diff"),with=F], 
              by = c("city_type","Health_Camp_ID"), all.x=T )
  
  return(df)
}

df[, previous_camp_end_date := c(as.Date("1900-01-01"), 
                                 as.Date(Camp_End_Date[-.N])), by = c(Category2)]
df$previous_camp_end_date[
  df$previous_camp_end_date == as.Date("1900-01-01")]<-NA

df$is_another_camp_going_on <- 
  ifelse(df$Camp_Start_Date < df$previous_camp_end_date, 1,  0)
#get last category of ongoing camp
df[, previous_camp_category := c(NA,Category1[-.N]), by = Category2]
df$current_camp_category <- as.integer(df$Category1)
#set flags
df$is_ongoing_camp_same_as_previous <- ifelse(
  df$previous_camp_category == df$current_camp_category, 1,0
)

camps.running.simultaneuously <- function(df){
  
  df <- data.table(df)
  #get last date of ongoing camp in city
  city_camp <- unique(df[!(is.na(city_type)),c("Health_Camp_ID","city_type",
                                               "Camp_Start_Date", "Camp_End_Date", 
                                               "Category1"),with=F])
  setorder(city_camp,"city_type","Category1","Camp_Start_Date","Camp_End_Date")
  city_camp[,tmp_ind = 1:.N,by = c("city_type","Category1")]
  city_camp[,prev_Camp_Start_Date := c(-1,Camp_Start_Date[-.N]),by = c("city_type","Category1")]
  
  city_camp[,last_camp_time_diff:= as.numeric(Camp_Start_Date - prev_Camp_Start_Date)]
  #setorder(df,"city_type","Category1","Camp_Start_Date")
  
  df <- merge(df, city_camp[,c("city_type","Health_Camp_ID","last_camp_time_diff"),with=F], 
              by = c("city_type","Health_Camp_ID"), all.x=T )
  
  return(df)
}