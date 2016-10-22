#load library
library(sqldf)

#read file
df <- read.csv("D:\\AnalyticsVidya\\Knoctober\\Codes\\Knocktober\\DERIVED\\train_outcome_camp_details.csv")
head(df)

#1. Does he register much in advance, or during when the camp is going on
df$Registration_Date <- as.Date(df$Registration_Date)
df$Camp_Start_Date <- as.Date(df$Camp_Start_Date)
df$registers_before_camp <- ifelse((df$Registration_Date < df$Camp_Start_Date),1,0)
#will be -ve too
df$number_of_days_to_camp_after_registration <- difftime(df$Registration_Date ,
                                                         df$Camp_Start_Date , 
                                                         units = c("days"))

#2. done bu piyush

#3. No. of time person has registered for this specific camp genre or for any camp?
#get count of times he has registered 
ref_1 <- sqldf("select Patient_ID , count(*) as gross_times_registered from df
               group by Patient_ID")
ref_1$has_registered_multiple_times <- ifelse((ref_1$gross_times_registered > 1), 1,0)
#left join 
df <- merge(x = df, y = ref_1, by = "Patient_ID", x.all = T)
head(df)

#3.2 get counts for each genre
g1 <- sqldf("select Patient_ID , count(*) as number_of_times_visited_camp_1 from df where Category1 = 'First' group by Patient_ID")
g2 <- sqldf("select Patient_ID , count(*) as number_of_times_visited_camp_2 from df where Category1 = 'Second' group by Patient_ID")
g3 <- sqldf("select Patient_ID , count(*) as number_of_times_visited_camp_3 from df where Category1 = 'Third' group by Patient_ID")
#merge each 
g12 <- merge(x = g1, y= g2 , by = "Patient_ID", all = T)
g123 <- merge(x = g12, y= g3 , by = "Patient_ID", all = T)
g123[is.na(g123)] <- 0

#merge with main df
df <- merge(x = df, y = g123, by = "Patient_ID", all.x=T)
head(df)
