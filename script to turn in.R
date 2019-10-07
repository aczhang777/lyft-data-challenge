#Import Datasets--------------------------------------------------------------------------------------
##Download driver_ids data frame
driver_ids <- read.csv("driver_ids.csv", stringsAsFactors=FALSE)

##Download ride_ids data frame
ride_ids <- read.csv("ride_ids.csv", stringsAsFactors=FALSE)

##Download ride_timestamps data frame
ride_timestamps <- read.csv("ride_timestamps.csv", stringsAsFactors=FALSE)

#Initial Data Cleaning--------------------------------------------------------------------------------------
##Merge ride_ids and ride_timestamps by ride ID
rides_merged <- merge(ride_ids,ride_timestamps,all=TRUE)
rides_merged <- rides_merged[order(rides_merged$driver_id),]

##Find the date of each ride
rides_merged[,8] <- substr(rides_merged$timestamp,1,10)
names(rides_merged)[8] <- "Date of Ride"


##Remove rows with unique ride IDs that are not attached to a unique driver iD
##Remove rows with unique ride IDs that don't have any timestamp information
non_rides_merged <- rides_merged[c(which(is.na(rides_merged$driver_id)), which(is.na(rides_merged$event))),] ##Contains removed values
rides_merged <- rides_merged[-c(which(is.na(rides_merged$driver_id)), which(is.na(rides_merged$event))),] ##New merged data frame

#Driver Information--------------------------------------------------------------------------------------
##Find number of rides each driver has done
driver_ids_new <- data.frame(driver_ids, "Number of Rides"=numeric(nrow(driver_ids)), stringsAsFactors=FALSE)
for(i in 1:nrow(driver_ids_new)){
  driver_ids_new[i,3] <- length(which(ride_ids$driver_id == driver_ids[i,1]))
}

##Find how long each driver has been with Lyft
###First need to find the most recent ride on file for the driver
driver_ids_new <- data.frame(driver_ids_new,"Most_Recent_Ride"=character(nrow(driver_ids_new)),stringsAsFactors=FALSE)
for(i in 1:nrow(driver_ids_new)){
  vec <- which(rides_merged$driver_id == driver_ids_new$driver_id[i])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of one driver
  date_int <- numeric(length(vec))
  for(j in 1:length(vec)){
    date_int[j] <- as.integer(gsub("-","",substr(one_driver[j,"timestamp"],1,10))) ##Convert dates into integer values
  }
  max <- as.character(max(date_int)) ##Find max value and convert back to character, corresponds to most recent date
  first <- substr(max,1,4) ##Year
  second <- substr(max,5,6) ##Month
  third <- substr(max,7,8) ##Day
  driver_ids_new[i,4] <- paste(first,second,third,sep="-") ##Contains character value with most recent date for one driver
}

###Now subtract each date from onboarding date
library(lubridate)
for(i in 1:nrow(driver_ids_new)){
  first <- as.Date(driver_ids_new$Most_Recent_Ride[i],"%Y-%m-%d") ##Converts date of most recent ride into days
  second <- as.Date(substr(driver_ids_new$driver_onboard_date[i],1,10),"%Y-%m-%d") ##Converts onboarding date into days
  driver_ids_new[i,5] <- (yday(first) - yday(second)) / 30.417 ##Calculate difference and divide by average number of days in a month
  names(driver_ids_new)[5] <- "Length of Work (Months)"
}

##Remove drivers who never gave any rides
###Data frame holding information about all drivers with 0 rides
non_drivers <- driver_ids_new[c(which(driver_ids_new$Number.of.Rides == 0), which(is.na(driver_ids_new$`Length of Work (Months)`))),]
###Data frame holding information from drivers only with >0 rides
driver_ids_new <- driver_ids_new[-c(which(driver_ids_new$Number.of.Rides == 0), which(is.na(driver_ids_new$`Length of Work (Months)`))),]

#Churn Rate Calculation--------------------------------------------------------------------------------------
##March -> April
march_driver_ids <- driver_ids_new[which(substr(driver_ids_new$driver_onboard_date,6,7) == "03"),1] #All drivers onboarded in March
march_drivers <- data.frame("Driver ID"=march_driver_ids,"Stopped Working"=character(length(march_driver_ids)),stringsAsFactors=FALSE)
march_sum <- 0
for(i in 1:nrow(march_drivers)){
  vec <- which(rides_merged$driver_id == march_drivers[i,1])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of each driver onboarded in March
  if(length(which(substr(one_driver[,8],6,7) == "04")) > 0){
      march_sum <- march_sum + 1
      march_drivers[i,2] <- "N"
  } else{
    march_drivers[i,2] <- "Y"
  }
}
march_churn <- 1 - march_sum / nrow(march_drivers) #6.1% of active drivers onboarded stopped working in April

##April -> May
april_driver_ids <- driver_ids_new[which(substr(driver_ids_new$driver_onboard_date,6,7) == "04"),1]
april_drivers <- data.frame("Driver ID"=april_driver_ids,"Stopped Working"=character(length(april_driver_ids)),stringsAsFactors=FALSE)
april_drivers <- rbind(april_drivers,march_drivers[which(march_drivers$Stopped.Working == "N"),]) #All drivers onboarded in April or have been working since March
april_sum <- 0
for(i in 1:nrow(april_drivers)){
  vec <- which(rides_merged$driver_id == april_drivers[i,1])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of each driver onboarded in March or April
  if(length(which(substr(one_driver[,8],6,7) == "05")) > 0){
    april_sum <- april_sum + 1
    april_drivers[i,2] <- "N"
  } else{
    april_drivers[i,2] <- "Y"
  }
}
april_churn <- 1 - april_sum / nrow(april_drivers) #8.8% of active drivers stopped working in May

##May -> June
may_driver_ids <- driver_ids_new[which(substr(driver_ids_new$driver_onboard_date,6,7) == "05"),1]
may_drivers <- data.frame("Driver ID"=may_driver_ids,"Stopped Working"=character(length(may_driver_ids)),stringsAsFactors=FALSE)
may_drivers <- rbind(may_drivers,april_drivers[which(april_drivers$Stopped.Working == "N"),]) #All drivers onboarded in May or have been working since March/April
may_sum <- 0
for(i in 1:nrow(may_drivers)){
  vec <- which(rides_merged$driver_id == may_drivers[i,1])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of each driver onboarded in March, April, or May
  if(length(which(substr(one_driver[,8],6,7) == "06")) > 0){
    may_sum <- may_sum + 1
    may_drivers[i,2] <- "N"
  } else{
    may_drivers[i,2] <- "Y"
  }
}
may_churn <- 1 - may_sum / nrow(may_drivers) #15.1% of active drivers stopped working in June

avg_churn <- (march_churn + april_churn + may_churn)/3 #avg churn rate is 10.0%
avg_lifetime <- 1/avg_churn #avg projected lifetime of a driver is ~10 months

#Driver Information (Cont.)--------------------------------------------------------------------------------------
##Only include drivers who have been working for at least 2 weeks
###Data frame holding information about all drivers who quit after less than 2 weeks
non_drivers <- driver_ids_new[which(driver_ids_new$`Length of Work (Months)` < 0.5),]
###Data frame holding information from drivers who have worked for Lyft for at least 2 weeks
driver_ids_new <- driver_ids_new[-which(driver_ids_new$`Length of Work (Months)` < 0.5),]

##Find how many prime time rides each driver has given
for(i in 1:nrow(driver_ids_new)){
  vec <- which(rides_merged$driver_id == driver_ids_new$driver_id[i])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of one driver
  driver_ids_new[i,6] <- length(which(one_driver$ride_prime_time != 0)) / 5 ##Total number of rides with prime time
  names(driver_ids_new)[6] <- "Number of Prime Time Rides"
}

##Find average monthly revenue for each driver
###Estimated price of each ride without prime time
ride_ids_new <- ride_ids
for(i in 1:nrow(ride_ids_new)){
  ##Base price + service fee + price per mile + price per minute
  calc <- 2 + 1.75 + 1.15*(ride_ids_new$ride_distance[i]/1609.344) + 0.22*(ride_ids_new$ride_duration[i]/60)
  if(calc < 5){ ##Minimum fare
    price <- 5
  } else if(calc > 400){ ##Maximum fare
    price <- 400
  } else {
    price <- calc
  }
  ride_ids_new[i,6] <- price
  names(ride_ids_new)[6] <- "Est. Price of Ride"
}

###Estimated price of ride with prime time
ride_ids_new[,7] <- ride_ids_new$`Est. Price of Ride` + ride_ids_new$`Est. Price of Ride`*(ride_ids_new$ride_prime_time/100)
names(ride_ids_new)[7] <- "Price w/ Prime Time"

###Estimated average monthly revenue for each driver
for(i in 1:nrow(driver_ids_new)){
  vec <- which(ride_ids_new$driver_id == driver_ids_new$driver_id[i])
  one_driver <- ride_ids_new[vec,] ##Data frame with all the unique rides of one driver
  driver_ids_new[i,7] <- sum(one_driver$`Price w/ Prime Time`) / driver_ids_new$`Length of Work`[i] 
  names(driver_ids_new)[7] <- "Average Monthly Revenue"
}

hist(driver_ids_new$`Average Monthly Revenue`, freq = TRUE, main = "Histogram of Proportion of Days Worked", xlab = "Proportion of Days Worked (%)", ylab = "Frequency") 
summary(driver_ids_new$`Proportion of Days Worked`) ## median is 58%, mean is 55%, with a max of 100%

##Proportion of days worked
for(i in 1:nrow(driver_ids_new)){
  vec <- which(rides_merged$driver_id == driver_ids_new$driver_id[i])
  one_driver <- rides_merged[vec,] ##Data frame with all ride information and timestamps of one driver
  driver_ids_new[i,8] <- length(unique(one_driver[,8])) / (driver_ids_new$`Length of Work (Months)`[i] * 30.417) #Finds number of unique dates worked and divides by length of work in days
  if(driver_ids_new[i,8] > 1){ ##Some discrepancies appear with calculation of proportions due to only using the average number of days in a month, restrict max to 100%
    driver_ids_new[i,8] <- 1
  }
  names(driver_ids_new)[8] <- "Proportion of Days Worked"
}

##Total Revenue
for (i in 1:nrow(driver_ids_new)) {
  vec <- which(ride_ids_new$driver_id == driver_ids_new$driver_id[i])
  one_driver <- ride_ids_new[vec,] ##all the rides the ith driver has done
  driver_ids_new[i, 9] <- sum(one_driver$`Price w/ Prime Time`)
  names(driver_ids_new)[9] <- "Total Revenue"
}

##Average Revenue per Ride
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 10] <- (driver_ids_new[i, 9] / driver_ids_new[i, 3]) ## 8th column is revenue, 3rd is number of rides
  names(driver_ids_new)[10] <- "Revenue Per Ride"
}

hist(driver_ids_new$`Revenue Per Ride`, freq = TRUE, main = "Histogram of Revenue Per Ride", xlab = "Revenue Per Ride in Dollars", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 15)
summary(driver_ids_new$`Revenue Per Ride`) 
boxplot(driver_ids_new$`Revenue Per Ride`) ## half of the riders in this dataset make fewer than $12 in revenue per ride

##Length of work at Lyft in days
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 11] <- (driver_ids_new[i, 5] * 30.417) ## 5th column is how long they've worked for Lyft in months
  names(driver_ids_new)[11] <- "Length of Work (Days)"
}

##Rides per day
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 12] <- driver_ids_new[i, 3] / driver_ids_new[i, 11] ## 3rd col is number of rides given, 10th is how long they've worked for lyft in days
  names(driver_ids_new)[12] <- "Rides Per Day"
}

hist(driver_ids_new$`Rides Per Day`, freq = TRUE, main = "Histogram of Rides Per Day", xlab = "Number of Rides Per Day", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 15)
summary(driver_ids_new$`Rides Per Day`)
boxplot(driver_ids_new$`Rides Per Day`) ## most give fewer than 5 rides/day.

##Average revenue per day
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 13] <- driver_ids_new[i, 9] * driver_ids_new[i, 11] ## 9th col is revenue per ride, 11th is rides per day
  names(driver_ids_new)[13] <- "Revenue Per Day"
}

hist(driver_ids_new$`Revenue Per Day`, freq = TRUE, main = "Histogram of Revenue Per Day", xlab = "Revenue Per Day in Dollars", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 20)
summary(driver_ids_new$`Revenue Per Day`) 
boxplot(driver_ids_new$`Revenue Per Day`)

#Prime Time rides per day
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 14] <- driver_ids_new[i, 6] / driver_ids_new[i, 11] # 6th col is number of prime time rides, 10th is length of work (days)
  names(driver_ids_new)[14] <- "Prime Time Rides Per Day"
}

hist(driver_ids_new$`Prime Time Rides Per Day`, freq = TRUE, main = "Histogram of Prime Time Rides Per Day", xlab = "Number of Prime Time Rides", ylab = "Frequency", col = c("lightpink", "lightskyblue")) ## very right skewed, not surprising
summary(driver_ids_new$`Prime Time Rides Per Day`) ## median is almost 6, while the mean is almost 7.
boxplot(driver_ids_new$`Prime Time Rides Per Day`) ## lot of potential outliers...

##Distribution for productivity (in terms of distance travelled in meters per day)
hist(driver_ids_new$`Meters Per Day`, freq = TRUE, main = "Histogram of Meters Per Day", xlab = "Meters Per Day in Thousands", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 20, xaxt = "n")
axis(side = 1, at = c(0, 20000, 40000, 60000, 80000, 100000), labels = c(0, 20, 40, 60, 80, 100), tick = TRUE)
summary(driver_ids_new$`Meters Per Day`) ## mean meters per day is 26838.3, with a median of 23414.2.
boxplot(driver_ids_new$`Meters Per Day`)

##Distribution for consistency (in terms of proportion of days worked)
hist(driver_ids_new$`Proportion of Days Worked`, freq = TRUE, main = "Histogram of Proportion of Days Worked", xlab = "Proportion of Days Worked", ylab = "Frequency", col = c("lightpink", "lightskyblue"))
## it's more common to see people who have actually spent a lot of their available days driving than it is to see people who have driven a lot (thus increasing meters per day).
summary(driver_ids_new$`Proportion of Days Worked`) ## most people devote slightly more than half their available days to driving for Lyft.
boxplot(driver_ids_new$`Proportion of Days Worked`)

#Top 25% of Drivers (Revenue)--------------------------------------------------------------------------------------
##Look at the distribution of different variables within the cohort of drivers that make the top 25% of average revenue per day
top25Percent <- which(driver_ids_new$`Revenue Per Day` >= 77.1950)
MVPDrivers <- driver_ids_new[top25Percent, ] #Contains top 25% of drivers in terms of revenue per day

##Distribution of revenue per day
hist(MVPDrivers$`Revenue Per Day`, freq = TRUE, main = "Histogram of Revenue Per Day for Top 25%", xlab = "Revenue Per Day in Dollars", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 15)
summary(MVPDrivers$`Revenue Per Day`) ## average revenue per day $107.00, with half making less than $98.79 per day and half making more.

##Distribution of revenue per ride
hist(MVPDrivers$`Revenue Per Ride`, freq = TRUE, main = "Histogram of Revenue Per Ride for Top 25%", xlab = "Revenue Per Ride", ylab = "Frequency", col = c("lightpink", "lightskyblue"))
summary(MVPDrivers$`Revenue Per Ride`) ## mean is $14.16, with the minimum being $11.61.

##Distribution of rides per day:
hist(MVPDrivers$`Rides Per Day`, freq = TRUE, main = "Histogram of Rides Per Day for Top 25%", xlab = "Rides Per Day", ylab = "Frequency", col = c("lightpink", "lightskyblue"))
summary(MVPDrivers$`Rides Per Day`) ## mean is 7.614, with a median of 7.028. Gotta get up to about 7 rides per day.

##Distribution of the total number of rides:
hist(MVPDrivers$Number.of.Rides, freq = TRUE, main = "Histogram of Number of Rides for Top 25%", xlab = "Number of Rides", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 15) 
summary(MVPDrivers$Number.of.Rides) ## average number of rides given is 441.7, median is 425, with the max being at 919. 

##Distribution of the number of prime time rides per day:
hist(MVPDrivers$`Prime Time Rides Per Day`, freq = TRUE, main = "Histogram of Number of Prime Time Rides Per Day for Top 25%", xlab = "Number of Prime Time Rides Per Day", ylab = "Frequency", col = c("lightpink", "lightskyblue"))
summary(MVPDrivers$`Prime Time Rides Per Day`) ## median is 2.686, mean is 2.882, with a max of 7.937

##Distribution of the number of prime time rides they've given:
hist(MVPDrivers$`Number of Prime Time Rides`, freq = TRUE, main = "Histogram of Number of Prime Time Rides for Top 25%", xlab = "Number of Prime Time Rides", ylab = "Frequency", col = c("lightpink", "lightskyblue"), breaks = 15) 
summary(MVPDrivers$`Number of Prime Time Rides`) ## the mean is 165, with a median of 160

##Distribution of length of time they've worked there in days:
hist(MVPDrivers$`Length of Time Worked in Days`, freq = TRUE, main = "Histogram of Length of Time Worked in Days", xlab = "Length of Time in Days", ylab = "Frequency", col = c("lightpink", "lightskyblue")) 
summary(MVPDrivers$`Length of Time Worked in Days`) ## the minimum is only a month--with the mean being 2 months. Okay, so you usually have to work for Lyft for about 2 months to get to this level of revenue. Median is only slightly shorter, at 56.5 (rounding up to 57).

##How much of total revenue comes from this cohort?
attach(MVPDrivers)
attach(driver_ids_new)
totalRev <- sum(`Total Revenue`)
revFromTop25Percent <- sum(MVPDrivers$`Total Revenue`) / totalRev
revFromTop25Percent

##What proportion of all rides do they give?
totalRides <- sum(driver_ids_new$Number.of.Rides)
ridesFromTop25Percent <- sum(MVPDrivers$Number.of.Rides) / totalRides
ridesFromTop25Percent

##What proportion of all prime time rides do they give?
totalPrimeTimeRides <- sum(driver_ids_new$`Number of Prime Time Rides`)
primeTimeFromTop25Percent <- sum(MVPDrivers$`Number of Prime Time Rides`) / totalPrimeTimeRides
primeTimeFromTop25Percent

##What would make a driver want to stay with Lyft?
avgLifetimeInDays <- mean(c(`Length of Time Worked in Days`))
sdLifetimeInDays <- sd(c(`Length of Time Worked in Days`))
avgLifetimeInDays;sdLifetimeInDays ## avg is 58 and sd is 18.7
summary(`Length of Time Worked in Days`) ## median is 60
hist(`Length of Time Worked in Days`, freq = TRUE, main = "Histogram of Length of Time Worked in Days", xlab = "Length of Time In Days", ylab = "Frequency", col = c("lightpink", "lightskyblue"))

#Productivity vs. Consistency--------------------------------------------------------------------------------------
## Is rides per day related to the proportion of days they've worked out of all days they've been a Lyft driver? 

plot(x = driver_ids_new$`Rides Per Day`, y = driver_ids_new$`Proportion of Days Worked`)

###Linear Model
linear <- lm(`Proportion of Days Worked` ~ `Rides Per Day`, data = driver_ids_new)
summary(linear)
plot(linear, 1)
plot(linear, 2)
mmp(linear) 

###Quadratic Model
quadratic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2), data = driver_ids_new) ## so this is a good model
summary(quadratic)
plot(quadratic, 1)
plot(quadratic, 2)
mmp(quadratic) 

###Cubic Model
cubic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2) + I(`Rides Per Day`^3), data = driver_ids_new)
summary(cubic)
plot(cubic, 1)
plot(cubic, 2)
mmp(cubic) 

###Polynomial Terms
polyTerms <- lm(`Proportion of Days Worked` ~ poly(`Rides Per Day`, 5))
summary(polyTerms)

###ANOVA test for finding best model
quadVsCub <- anova(quadratic, cubic)
quadVsCub

###Compare error rates for the quadratic and cubic models
###Use k fold cross validation with 10 folds to get an estimate of the mean square error rate for both models.

####Quadratic model
set.seed(626)
driverData <- driver_ids_new[sample(nrow(driver_ids_new)), ] ## randomly rearrange the rows in driverData
folds <- cut(seq(1, nrow(driver_ids_new)), breaks = 10, labels = FALSE) ## number the rows in driverData 1 through 10
emptyList <- vector("list", 12)
quadModels <- rep(emptyList, times = 10) ## store our quadratic models here
quadMSE <- numeric(10)
for (i in 1:10) {
  testIndices <- which(folds == i, arr.ind = TRUE) ## the ith fold is the data we use for testing purposes
  testingData <- driverData[testIndices, ]
  trainingData <- driverData[-testIndices, ] ## train on the remaining 9 folds
  quad <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2), data = trainingData)
  quadModels[[i]] <- quad ## saving the model here
  MSE <- mean((testingData$`Proportion of Days Worked` - predict(quad, newdata = testingData))^2) ## calculating mean square error here
  quadMSE[i] <- MSE ## saving it here
}

mean(quadMSE) ##Estimate of mean squared error for quadratic model is 0.01338.

####Cubic Model
set.seed(515)
driverData1 <- driver_ids_new[sample(nrow(driver_ids_new)), ]
cubicModels <- rep(emptyList, times = 10)
cubicMSE <- numeric(10)
for (i in 1:10) {
testIndices <- which(folds == i, arr.ind = TRUE)
testingData <- driverData1[testIndices, ]
trainingData <- driverData1[-testIndices, ]
cubic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2) + I(`Rides Per Day`^3))
cubicModels[[i]] <- cubic
MSE <- mean((testingData$`Proportion of Days Worked` - predict(cubic, newdata = testingData))^2)
cubicMSE[i] <- MSE
}

mean(cubicMSE) ##Our estimate of error here is 0.01248.
