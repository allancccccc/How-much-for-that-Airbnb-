kaggledata <- read.csv("analysisData.csv")
library(shiny)
library(DBI)
library(dplyr)

kaggledata %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit()

#Removing $
kaggledata$weekly_price <- as.numeric(gsub('[$,]', '', kaggledata$weekly_price))
kaggledata$security_deposit <- as.numeric(gsub('[$,]', '', kaggledata$security_deposit))
kaggledata$cleaning_fee <- as.numeric(gsub('[$,]', '', kaggledata$cleaning_fee))
kaggledata$extra_people <- as.numeric(gsub('[$,]', '', kaggledata$extra_people))
kaggledata$monthly_price <- as.numeric(gsub('[$,]', '', kaggledata$monthly_price))

#Convert amenities into binary
tv<-ifelse(grepl("TV",kaggledata$amenities),1,0)
wireless<-ifelse(grepl("wireless|Wireless|WIRELESS",kaggledata$amenities),1,0)
parking<-ifelse(grepl("parking|Parking|PARKING",kaggledata$amenities),1,0)
ac<-ifelse(grepl("air conditioning|Air conditioning",kaggledata$amenities),1,0)
wifi<-ifelse(grepl("Wifi",kaggledata$amenities),1,0)
pool<-ifelse(grepl("pool|Pool|POOL",kaggledata$amenities),1,0)
pets <- ifelse(grepl("Pets allowed", kaggledata$amenities), 1, 0)
washer <- ifelse(grepl("Washer|washer", kaggledata$amenities), 1, 0)
doorman <- ifelse(grepl("Doorman|doorman", kaggledata$amenities), 1, 0)
gym <- ifelse(grepl("Gym|gym", kaggledata$amenities), 1, 0)
fireplace <- ifelse(grepl("Fireplace|fireplace", kaggledata$amenities), 1, 0)
kitchen <- ifelse(grepl("Kitchen|kitchen", kaggledata$amenities), 1, 0)
new <- ifelse(grepl("New|new", kaggledata$summary), 1, 0)
beautiful <- ifelse(grepl("Beautiful|beautiful", kaggledata$summary), 1, 0)
quiet <- ifelse(grepl("Quiet|quiet", kaggledata$summary), 1, 0)
spacious <- ifelse(grepl("Spacious|spacious", kaggledata$summary), 1, 0)
view <- ifelse(grepl("View|view", kaggledata$summary), 1, 0)


sum(tv)
sum(pool)
sum(ac)


kaggledata$pool=pool
kaggledata$tv=tv
kaggledata$wireless=wireless
kaggledata$parking=parking
kaggledata$ac=ac
kaggledata$wifi=wifi
kaggledata$pets=pets
kaggledata$washer=washer
kaggledata$doorman=doorman
kaggledata$gym=gym
kaggledata$fireplace=fireplace
kaggledata$kitchen=kitchen
kaggledata$new=new
kaggledata$beautiful=beautiful
kaggledata$quiet=quiet
kaggledata$spacious=spacious
kaggledata$view=view

#Replacing N/A
kaggledata$host_listings_count[which(is.na(kaggledata$host_listings_count))]=mean(kaggledata$host_listings_count,na.rm = TRUE)
kaggledata$host_total_listings_count[which(is.na(kaggledata$host_total_listings_count))]=mean(kaggledata$host_total_listings_count,na.rm = TRUE)
kaggledata$security_deposit[which(is.na(kaggledata$security_deposit))]=mean(kaggledata$security_deposit,na.rm = TRUE)
kaggledata$cleaning_fee[which(is.na(kaggledata$cleaning_fee))]=mean(kaggledata$cleaning_fee,na.rm = TRUE)
kaggledata$reviews_per_month[which(is.na(kaggledata$reviews_per_month))]=mean(kaggledata$reviews_per_month,na.rm = TRUE)
kaggledata$beds[which(is.na(kaggledata$beds))]=mean(kaggledata$beds,na.rm = TRUE)
kaggledata$extra_people[which(is.na(kaggledata$extra_people))]=mean(kaggledata$extra_people,na.rm = TRUE)
kaggledata$accommodates[which(is.na(kaggledata$accommodates))]=mean(kaggledata$accommodates,na.rm = TRUE)
kaggledata$minimum_nights[which(is.na(kaggledata$minimum_nights))]=mean(kaggledata$minimum_nights,na.rm = TRUE)
kaggledata$review_scores_rating[which(is.na(kaggledata$review_scores_rating))]=mean(kaggledata$review_scores_rating,na.rm = TRUE)
kaggledata$review_scores_cleanliness[which(is.na(kaggledata$review_scores_cleanliness))]=mean(kaggledata$review_scores_cleanliness,na.rm = TRUE)
kaggledata$review_scores_value[which(is.na(kaggledata$review_scores_value))]=mean(kaggledata$review_scores_value,na.rm = TRUE)
kaggledata$review_scores_location[which(is.na(kaggledata$review_scores_location))]=mean(kaggledata$review_scores_location,na.rm = TRUE)

kaggledata=select(kaggledata,host_listings_count,neighbourhood_group_cleansed,property_type,room_type,accommodates,
                  bathrooms,bedrooms,beds,price,security_deposit,cleaning_fee,
                  extra_people,minimum_nights,review_scores_rating,
                  review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                  availability_30,review_scores_communication,review_scores_location,review_scores_value,
                  reviews_per_month,zipcode,pool,tv,wifi,ac,parking,wireless,pets,washer,zipcode,doorman,gym,fireplace,kitchen,new,beautiful,quiet,spacious,view,monthly_price)
summary(kaggledata)

scoringData = read.csv('scoringData.csv')

scoringData %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% 
  na.omit()

scoringData$weekly_price <- as.numeric(gsub('[$,]', '', scoringData$weekly_price))
scoringData$security_deposit <- as.numeric(gsub('[$,]', '', scoringData$security_deposit))
scoringData$cleaning_fee <- as.numeric(gsub('[$,]', '', scoringData$cleaning_fee))
scoringData$extra_people <- as.numeric(gsub('[$,]', '', scoringData$extra_people))
scoringData$monthly_price <- as.numeric(gsub('[$,]', '', scoringData$monthly_price))

tv<-ifelse(grepl("TV",scoringData$amenities),1,0)
wireless<-ifelse(grepl("wireless|Wireless|WIRELESS",scoringData$amenities),1,0)
parking<-ifelse(grepl("parking|Parking|PARKING",scoringData$amenities),1,0)
ac<-ifelse(grepl("air conditioning|Air conditioning",scoringData$amenities),1,0)
wifi<-ifelse(grepl("Wifi",scoringData$amenities),1,0)
pool<-ifelse(grepl("pool|Pool|POOL",scoringData$amenities),1,0)
pets <- ifelse(grepl("Pets allowed", scoringData$amenities), 1, 0)
washer <- ifelse(grepl("Washer|washer", scoringData$amenities), 1, 0)
doorman <- ifelse(grepl("Doorman|doorman", scoringData$amenities), 1, 0)
gym <- ifelse(grepl("Gym|gym", scoringData$amenities), 1, 0)
fireplace <- ifelse(grepl("Fireplace|fireplace", scoringData$amenities), 1, 0)
kitchen <- ifelse(grepl("Kitchen|kitchen", scoringData$amenities), 1, 0)
new <- ifelse(grepl("New|new", scoringData$summary), 1, 0)
beautiful <- ifelse(grepl("Beautiful|beautiful", scoringData$summary), 1, 0)
quiet <- ifelse(grepl("Quiet|quiet", scoringData$summary), 1, 0)
spacious <- ifelse(grepl("Spacious|spacious", scoringData$summary), 1, 0)
view <- ifelse(grepl("View|view", scoringData$summary), 1, 0)


scoringData$pool=pool
scoringData$tv=tv
scoringData$wireless=wireless
scoringData$parking=parking
scoringData$ac=ac
scoringData$wifi=wifi
scoringData$pets=pets
scoringData$washer=washer
scoringData$doorman=doorman
scoringData$gym=gym
scoringData$fireplace=fireplace
scoringData$kitchen=kitchen
scoringData$new=new
scoringData$beautiful=beautiful
scoringData$quiet=quiet
scoringData$spacious=spacious
scoringData$view=view


scoringData$host_listings_count[which(is.na(kaggledata$host_listings_count))]=mean(kaggledata$host_listings_count,na.rm = TRUE)
scoringData$host_total_listings_count[which(is.na(kaggledata$host_total_listings_count))]=mean(kaggledata$host_total_listings_count,na.rm = TRUE)
scoringData$security_deposit[which(is.na(kaggledata$security_deposit))]=mean(kaggledata$security_deposit,na.rm = TRUE)
scoringData$cleaning_fee[which(is.na(kaggledata$cleaning_fee))]=mean(kaggledata$cleaning_fee,na.rm = TRUE)
scoringData$reviews_per_month[which(is.na(kaggledata$reviews_per_month))]=mean(kaggledata$reviews_per_month,na.rm = TRUE)
scoringData$beds[which(is.na(kaggledata$beds))]=mean(kaggledata$beds,na.rm = TRUE)
scoringData$extra_people[which(is.na(kaggledata$extra_people))]=mean(kaggledata$extra_people,na.rm = TRUE)
scoringData$accommodates[which(is.na(kaggledata$accommodates))]=mean(kaggledata$accommodates,na.rm = TRUE)
scoringData$minimum_nights[which(is.na(kaggledata$minimum_nights))]=mean(kaggledata$minimum_nights,na.rm = TRUE)
scoringData$review_scores_rating[which(is.na(kaggledata$review_scores_rating))]=mean(kaggledata$review_scores_rating,na.rm = TRUE)
scoringData$review_scores_cleanliness[which(is.na(kaggledata$review_scores_cleanliness))]=mean(kaggledata$review_scores_cleanliness,na.rm = TRUE)
scoringData$review_scores_value[which(is.na(kaggledata$review_scores_value))]=mean(kaggledata$review_scores_value,na.rm = TRUE)
scoringData$review_scores_location[which(is.na(kaggledata$review_scores_location))]=mean(kaggledata$review_scores_location,na.rm = TRUE)


scoringData=select(scoringData,id,host_listings_count,neighbourhood_group_cleansed,property_type,room_type,accommodates,
                   bathrooms,bedrooms,beds,security_deposit,cleaning_fee,
                   extra_people,minimum_nights,review_scores_rating,
                   review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                   availability_30,review_scores_communication,review_scores_location,review_scores_value,
                   reviews_per_month,zipcode,pool,tv,wifi,ac,parking,wireless,pets,washer,zipcode,doorman,gym,fireplace,kitchen,new,beautiful,quiet,spacious,view,monthly_price)

#gbm
library(gbm)
set.seed(617)
boost=gbm(price~.,data=kaggledata,distribution = 'gaussian',
          n.trees = 2000,interaction.depth = 10,shrinkage = 0.2)
predBoost=predict(boost,data=kaggledata, n.tree=2000)
rmseBoost=sqrt(mean((predBoost-kaggledata$price)^2))
rmseBoost

pred=predict(boost,newdata=scoringData,n.trees = 2000)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'aolunkaggle.csv',row.names = F)




