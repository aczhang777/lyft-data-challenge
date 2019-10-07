Procedure:
1. Figure out how many rides a driver has given. Kienna did this.
2. Figure out how much money they've made Lyft in total. Done.
3. Divide by how many rides they've given to get revenue per ride. Done.
4. Figure out how many rides, on average, a driver gives per day.
5. Multiply avg amount made per ride * avg amount of rides given per day = avg amount of money a driver makes for lyft per day, if they continue driving at their current rate.

# figuring out how many rides a driver has given was done above
# figuring out how much money they've made Lyft total
for (i in 1:nrow(driver_ids_new)) {
  vec <- which(ride_ids_new$driver_id == driver_ids_new$driver_id[i])
  n3 <- length(vec)
  one_driver <- ride_ids_new[vec,] ##all the rides the ith driver has done
  driver_ids_new[i, 8] <- sum(one_driver$`Price w/ Prime Time`)
  names(driver_ids_new)[8] <- "Total Revenue"
}


Revenue per ride:

# figuring out revenue per ride
for (i in 1:nrow(driver_ids_new)) {
  driver_ids_new[i, 9] <- (driver_ids_new[i, 8] / driver_ids_new[i, 3]) ## 8th column is revenue, 3rd is number of rides
  names(driver_ids_new)[9] <- "Revenue Per Ride"
  
  # how many rides does a driver give per day?
  ## first figure out how long they've worked there in days
  for (i in 1:nrow(driver_ids_new)) {
    driver_ids_new[i, 10] <- (driver_ids_new[i, 5] * 30.417) ## 5th column is how long they've worked for Lyft in months
    names(driver_ids_new)[10] <- "Length of Time Worked in Days" ##
  }
  ## then figure out how many rides they've given per day
  for (i in 1:nrow(driver_ids_new)) {
    driver_ids_new[i, 11] <- driver_ids_new[i, 3] / driver_ids_new[i, 10] ## 3rd col is number of rides given, 10th is how long they've worked for lyft in days
    names(driver_ids_new)[11] <- "Rides Per Day"
  }
  
# so how much would a driver make lyft per day, if they continued at their current rate?
  for (i in 1:nrow(driver_ids_new)) {
    driver_ids_new[i, 12] <- driver_ids_new[i, 9] * driver_ids_new[i, 11] ## 3rd col is number of rides given, 10th is how long they've worked for lyft in days
    names(driver_ids_new)[12] <- "Revenue Per Day"
  }
  
  # another interesting thing: how many prime time rides do they give per day?
  for (i in 1:nrow(driver_ids_new)) {
    driver_ids_new[i, 13] <- driver_ids_new[i, 6] / driver_ids_new[i, 10] # 6th col is number of prime time rides, 10th is number of days worked
    names(driver_ids_new)[13] <- "Prime Time Rides Per Day"
  }
  
  
MVP DRIVERS: and the ones right below them!
  top25Percent <- which(driver_ids_new$`Revenue Per Day` >= 77.1950)
MVPDrivers <- driver_ids_new[top25Percent, ]
next25Percent <- which((driver_ids_new$`Revenue Per Day` >= 48.7932 & driver_ids_new$`Revenue Per Day` < 77.1950))
OneBelowMVP <- driver_ids_new[next25Percent, ]

## summary statistics!
summary(MVPDrivers$`Revenue Per Day`) ## average revenue per day $107.00, with half making less than $98.79 per day and half making more.
summary(OneBelowMVP$`Revenue Per Day`)

summary(MVPDrivers$`Revenue Per Ride`) ## mean is $14.16, with the minimum being $11.61.
summary(OneBelowMVP$`Revenue Per Ride`)

summary(MVPDrivers$`Rides Per Day`) ## mean is 7.614, with a median of 7.028. Gotta get up to about 7 rides per day.
summary(OneBelowMVP$`Rides Per Day`)

summary(MVPDrivers$`Prime Time Rides Per Day`) ## median is , mean is 2.882, with a max of 7.937
summary(OneBelowMVP$`Prime Time Rides Per Day`)

summary(MVPDrivers$`Length of Time Worked in Days`) ## the minimum is only a month--with the mean being 2 months. Okay, so you usually have to work for Lyft for about 2 months to get to this level of revenue. Median is only slightly shorter, at 56.5 (rounding up to 57).
summary(OneBelowMVP$`Length of Time Worked in Days`)

## Next: how much of total revenue comes from this cohort?
totalRev <- sum(`Total Revenue`)
revFromTop25Percent <- sum(MVPDrivers$`Total Revenue`) / totalRev
revFromTop25Percent
## The top 25% are responsible for 48.25% of the total revenue.

What proportion of all rides do they give?
totalRides <- sum(driver_ids_new$Number.of.Rides)
ridesFromTop25Percent <- sum(MVPDrivers$Number.of.Rides) / totalRides
ridesFromTop25Percent
## They've also given 47.52% of all rides. How about prime time rides?

totalPrimeTimeRides <- sum(driver_ids_new$`Number of Prime Time Rides`)
primeTimeFromTop25Percent <- sum(MVPDrivers$`Number of Prime Time Rides`) / totalPrimeTimeRides
primeTimeFromTop25Percent
## They've also given 50.27% of all prime time rides.
  

# RELATIONSHIP BETWEEN PRODUCTIVITY AND CONSISTENCY
  # calculating fraction of days they actually drove (as in, days they actually drove / days they've been employed)
  rides_merged[, 8] <- substr(rides_merged$timestamp,1,10)
names(rides_merged)[8] <- "Date of Ride"

for(i in 1:nrow(driver_ids_new)){
  vec <- which(rides_merged$driver_id == driver_ids_new$driver_id[i])
  one_driver <- rides_merged[vec, ] ##Data frame with all ride information and timestamps of one driver
  proportionWorked <- length(unique(one_driver[, 8])) / (driver_ids_new$`Length of Time Worked in Days`[i]) #Finds number of unique dates worked and divides by length of work in days
  if(proportionWorked > 1){ ##Some discrepancies appear with calculation of proportions due to only using the average number of days in a month, restrict max to 100%
    driver_ids_new[i, 15] <- 1
  } else {
    driver_ids_new[i, 15] <- proportionWorked
  }
  names(driver_ids_new)[15] <- "Proportion of Days Worked"
}

plot(x = driver_ids_new$`Rides Per Day`, y = driver_ids_new$`Proportion of Days Worked`)
## so people who drive more per day also tend to spend more days driving. interesting. also, definitely nonlinear--try a quadratic term?

linear <- lm(`Proportion of Days Worked` ~ `Rides Per Day`, data = driver_ids_new)
summary(linear)
plot(linear, 1)
plot(linear, 2)
mmp(linear) ## linear is definitely not a good fit

quadratic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2), data = driver_ids_new) ## so this is a good model
summary(quadratic)
plot(quadratic, 1)
plot(quadratic, 2)
mmp(quadratic) ## pretty good fit--but can we do better?

cubic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2) + I(`Rides Per Day`^3), data = driver_ids_new)
summary(cubic)
plot(cubic, 1)
plot(cubic, 2)
mmp(cubic) ## so this one might be an even better fit to our data, let's try other polynomial terms:

polyTerms <- lm(`Proportion of Days Worked` ~ poly(`Meters Per Day`, 5))
summary(polyTerms)

## so, not much more helpful beyond cubic. got it. now, which model actually is best?

We'll start by running an ANOVA test:
quadVsCub <- anova(quadratic, cubic)
quadVsCub

So the model that has a cubic term significantly reduces the residual sum of squares than the model that just has a quadratic term. That being said, it's worth noting that both models have similar R^2 values: 0.7777 for the quadratic model and 0.7903 for the cubic one.
Now let's compare error rates--we'll use k fold cross validation with 10 folds to get an estimate of the mean square error rate for both models.

First, for the quadratic model:
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

mean(quadMSE) ## this is our estimate of mean squared error

Our estimate of mean squared error for the quadratic model is 0.01336.

Now let's compute it for the cubic model:
set.seed(515)
driverData1 <- driver_ids_new[sample(nrow(driver_ids_new)), ]
cubicModels <- rep(emptyList, times = 10)
cubicMSE <- numeric(10)
for (i in 1:10) {
  testIndices <- which(folds == i, arr.ind = TRUE)
  testingData <- driverData1[testIndices, ]
  trainingData <- driverData1[-testIndices, ]
  cubic <- lm(`Proportion of Days Worked` ~ `Rides Per Day` + I(`Rides Per Day`^2) + I(`Rides Per Day`^3), data = trainingData)
  cubicModels[[i]] <- cubic
  MSE <- mean((testingData$`Proportion of Days Worked` - predict(cubic, newdata = testingData))^2)
  cubicMSE[i] <- MSE
}

mean(cubicMSE)

Our estimate of error here is 0.01283.
So practically, these models aren't significantly different. Either model would work well in trying to predict the proportion of days worked (consistency) using rides per day (productivity).

