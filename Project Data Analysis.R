# IE 500 - Predicting the Impact of Weather on Ride Sharing in Chicago
# ----------------------------------------------------------------------
# Final Code Script
# All our code that we use to generate graphs and models in our presentation is available in this script
# We have provided anotations at each steps for explanation and reproducability purposes
# ----------------------------------------------------------------------
# API for darksky package: af52319c964c1b3abacafee65179f1ed
# ----------------------------------------------------------------------
# Load necessary libraries and packages
#devtools::install.packages("darksky")
#install.packages(c("tidyverse", "curl", "caret", "ISLR", "MASS", "glmnet", "leaps", "dplyr", "magrittr", "earth", "car", "tree",
                   #"ada", "randomForest", "gbm", "Metrics", "sqldf", "lubridate", "mgcv", "elasticnet", "ModelMetrics"))  
library(darksky)
library(tidyverse)
library(curl)
library(caret)
library(ISLR)
library(MASS)
library(glmnet)
library(leaps)
library(dplyr)
library(magrittr)
library(earth)
library(car)
library(tree)
library(ada)
library(randomForest)
library(gbm)
library(Metrics)
library(sqldf)
library(lubridate)
library(mgcv)
library(elasticnet)
library(ModelMetrics)

# Coordinate of the City of Chciago: 41.8781, -87.6298
# API for darksky package: af52319c964c1b3abacafee65179f1ed
# When asked about API, please copy-paste the key above
now <- get_current_forecast(41.8781, -87.6298)
print(now)
# ---------------------------------------------------------------------
# The code below are for generating the historical sample weather graphs
# Daily Historical Temperature
seq(Sys.Date()-93, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(41.8781, -87.6298, .x)) %>% 
  map_df("hourly") %>% 
  ggplot(aes(x=time, y=temperature)) +
  geom_line() + ggtitle("Past Daily Temperature in Chicago") +
  xlab("Date") + ylab("Temperature (°F)") +
  theme(plot.title = element_text(hjust = 0.5))

# Daily Humidity History
seq(Sys.Date()-93, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(41.8781, -87.6298, .x)) %>% 
  map_df("daily") %>% 
  ggplot(aes(x=time, y=humidity)) +
  geom_line() + ggtitle("Past Daily Humidity in Chicago") +xlab("Date") +ylab("Humidity") +
  theme(plot.title = element_text(hjust = 0.5))

# Daily Wind Speed History
seq(Sys.Date()-93, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(41.8781, -87.6298, .x)) %>% 
  map_df("daily") %>% 
  ggplot(aes(x=time, y=windSpeed)) +
  geom_line() + ggtitle("Past Daily Wind Speed in Chicago") +xlab("Date") +ylab("Wind Speed") +
  theme(plot.title = element_text(hjust = 0.5))

#Daily Visibility History
seq(Sys.Date()-93, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(41.8781, -87.6298, .x)) %>% 
  map_df("daily") %>% 
  ggplot(aes(x=time, y=visibility)) +
  geom_line() + ggtitle("Past Daily Visibility in Chicago") +xlab("Date") +ylab("Visibility") +
  theme(plot.title = element_text(hjust = 0.5))

# Temperature -> Take average of daily temperature through high and low
# Humidity
# Wind Speed
# Visibility
# Icon/Summary

# Extracts dailiy/hourly data of the weather (do more dates just to be safe)
dw <- seq(Sys.Date()-160, as.Date("2019/11/03"), "1 day") %>% 
  map(~get_forecast_for(41.8781, -87.6298, .x)) %>% 
  map_df("hourly")

# Replace night and day cloudy with clear-day/night

dw$icon[dw$icon == "partly-cloudy-day"] <- "clear-day"
dw$icon[dw$icon == "partly-cloudy-night"] <- "clear-night"
dw$icon[dw$icon == "cloudy"] <- "clear-day"

# Gets the data table for only variables that we want
weatherdata <- c("time", "icon", "temperature", "humidity", "windSpeed", "visibility")
wd <- dw[weatherdata]

# Clean and make variables easier to read
wd$icon[wd$icon == "clear-day"] <- "Clear Day"
wd$icon[wd$icon == "clear-night"] <- "Clear Night"
wd$icon[wd$icon == "rain"] <- "Rain"
wd$icon[wd$icon == "snow"] <- "Snow"
wd$icon[wd$icon == "sleet"] <- "Sleet"
wd$icon[wd$icon == "fog"] <- "Fog"

#--------------------------------------------------------------------------------------------------------------------------------------------

# Ride Sharing Data File
# Part 2 - Data Exploration and Visualization
# This will take a while to load since the data set is very big
df <- read.csv("Ridesharing_File.csv")
df <- df[, -1] # -> Removes the trip id column since we won't need it
df <- df[, -17] # Removes the centroil location columns since we already have individual latitude and longitude
df <- df[, -19] # Removes the centroil location columns since we already have individual latitude and longitude
df <- df[, -13] # Remove shared trip
df <- df[, -13] # Remove trips pooled

df <- na.omit(df) # Removes all rows containing NA values (data cleanup)

# Convert seconds to numerical 
df$Trip.Seconds <- as.character(df$Trip.Seconds)
df$Trip.Seconds <- lapply(df$Trip.Seconds, readr::parse_number)
df$Trip.Seconds <- as.numeric(as.character(unlist(df$Trip.Seconds)))

df <- na.omit(df)
# Check the data types
str(df)

# Convert date values into date
df$Trip.Start.Timestamp <- round(as.POSIXct(df$Trip.Start.Timestamp, format = "%m/%d/%Y %I:%M:%S %p"), units = "hours")

df <- df[, -2]
df <- df[,-4]
df <- df[,-4]
df <- df[,-5]
df <- df[, -9:-12]

# --------------------------------------------------------------------------------
# Create a new data table that consists of unique values of time
# The table created will be used to to combine both data source into one
colnames(df)[1] <- "time"
df$time <- as.POSIXct(df$time)

newdb <- data.frame(unique(df$time)) # Gives you all the available times in the ride sharing dataset
newdb <- newdb[(order(as.Date(newdb$unique.df.time.))),] # Order it from oldest to newest
newdb <- data.frame(newdb) # Makes it into a dataframe object

# Combine and subset tables using sqlite package
maindf <- sqldf("SELECT * FROM df JOIN wd using(time)") # This is our main table
colnames(df)[4] <- "area"

newdb <- sqldf("SELECT time, COUNT(*), area FROM df GROUP BY time, area")
maindf <- sqldf("SELECT * FROM newdb JOIN wd using(time)") # This is our main table
colnames(maindf)[2] <- "rides"
colnames(maindf)[4] <- "condition"

maindf$condition[maindf$condition == "Clear Day"] <- 1
maindf$condition[maindf$condition == "Clear Night"] <- 2
maindf$condition[maindf$condition == "Fog"] <- 3
maindf$condition[maindf$condition == "Rain"] <- 4

day <- as.Date(maindf$time, format = "%Y-%m-%d %I:%M:%S %p")
maindf$WeekDay <- weekdays(day)
maindf$WeekDay <- as.factor(maindf$WeekDay)

# This is the new final dataframe that we use to run our model
# This dataframe consists of counts instead of predicting total.trip.cost
finaldf <- maindf[-1] 

#----------------------------------------------------------------------------
# Data Exploration and Analysis
# Everything in data exploration was done using entire data set so it will take a long time
#----------------------------------------------------------------------------
# Plot some basic measures

table(df$Trip.Start.Timestamp)

# Sample database for testing

train_idx <- sample(x = 1:nrow(df), size = floor(0.01*nrow(df))) # Gets a random 80% index from
sample_data <- df[train_idx,]

# We should exclude all rides that were free due to promotions

sample_data <- sample_data[!(sample_data$Fare == 0),]

sample_data[sample_data[, "Fare"] == 2.5, ]

# Creates individual dataframe for avg, min, and max
as <- aggregate(Trip.Total ~ Trip.Start.Timestamp, sample_data, mean)
colnames(as)[colnames(as) == "Trip.Total"] <- "Average"
mins <- aggregate(Trip.Total ~ Trip.Start.Timestamp, sample_data, min)
colnames(mins)[colnames(mins) == "Trip.Total"] <- "Min"
maxs <- aggregate(Trip.Total ~ Trip.Start.Timestamp, sample_data, max)
colnames(maxs)[colnames(maxs) == "Trip.Total"] <- "Max"

# Dataframe for daily avg, min, and max
sdata2 <- cbind(as, mins, maxs)
sdata2 <- sdata2[,-c(3,5)]

# Plots the average, min and max of Daily Total Trip Cost
ggplot(sdata2, aes(x = Trip.Start.Timestamp, y = value, color = Legend)) +
  geom_line(aes(y = Average, col = "Avg")) +
  geom_line(aes(y = Min, col = "Min")) +
  geom_line(aes(y = Max, col = "Max")) +
  ggtitle("Daily Total Trip Cost of Ride Sharing in Chicago") +
  xlab("Date (2019)") + 
  ylab("Total Cost ($USD)") +
  theme(plot.title = element_text(hjust = 0.5))

# This is very interesting observations: majority of the peaks falls on Friday with Thursdays being the 2nd highest
# Initial hypothesis suggests Fri-Sat with the highest daily max

count_area <- sample_data %>%
  group_by(Trip.Start.Timestamp, Pickup.Community.Area) %>%
  summarise(Rides = length(Pickup.Community.Area))

# Creates a Scatterplot for Daily Pickup areas count
ggplot(count_area, aes(x = Trip.Start.Timestamp, y = Pickup.Community.Area, size = Rides, fill = Rides)) +
  geom_point(shape = 21) + scale_y_continuous(breaks = seq(0, 80, 5)) +
  scale_fill_continuous(low = "yellow", high = "red") +
  ggtitle("Daily Pick Up Locations in Chicago") +
  xlab("Date (2019)") + 
  ylab("Community Area") +
  theme(plot.title = element_text(hjust = 0.5))

count_dropoff <- sample_data %>%
  group_by(Trip.End.Timestamp, Dropoff.Community.Area) %>%
  summarise(Rides = length(Dropoff.Community.Area))

# Creates a scatterplot for Daily Dropoff areas count
ggplot(count_dropoff, aes(x = Trip.End.Timestamp, y = Dropoff.Community.Area, size = Rides, fill = Rides)) +
  geom_point(shape = 21) + scale_y_continuous(breaks = seq(0,80,5)) +
  scale_fill_continuous(low = "yellow", high = "red") +
  ggtitle("Daily Drop Off Locations in Chicago") +
  xlab("Date (2019)") +
  ylab("Community Area") +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------------------------------------------
# BELOW IS OUR INITIAL PROJECTT MODEL APPROACH
# Response = Price
# This is our old initial approach which we did not use for our final model in presentation
# Everything below is for reference purpose only!!
#--------------------------------------------------------------------------------------------

# Combine both weather (wd) and ride (df) table into a master table for analysis
# Fix column names and date/time format to the same type as weather table
colnames(df)[1] <- "time"
df$time <- as.POSIXct(df$time)

# Merge the two tables together using sqldf package
#install.packages("sqldf")

maindf <- sqldf("SELECT * FROM df JOIN wd using(time)") # This is our main table

# ---------------------------------------------------------------------
# Model for Predicting Total Trip Cost using Weather and Trip Variables 
# Y = Trip.Total

# In order to get just the variables for running SL/ML models, we subset the cols
var <- c("Trip.Seconds", "Trip.Miles", "Pickup.Community.Area",
         "Trip.Total", "icon", "temperature", "humidity", "windSpeed", "visibility")

faredf <- maindf[var] # This is now the dataset that only consist of the variables


# Since the data is so big and we cannot run everything, we decided to use 1% for now

fdf_index <- sample(x = 1:nrow(faredf), size = floor(0.01*nrow(faredf)))
finaldf <- faredf[fdf_index,]

set.seed(10)

# Train data for regressions
train_idx <- sample(x = 1:nrow(finaldf), size = floor(0.80*nrow(finaldf))) # Gets a random 80% index from

train_data <- finaldf[train_idx,]
test_data <- finaldf[-train_idx,] # Gives you all the rows that is not in the train_idx index

# Linear Regression

lm.fit <- lm(Trip.Total ~ ., data=train_data)
lm.pred <- predict(lm.fit, newdata = test_data)
lm.mse <- mean((lm.pred - test_data[,4])^2)

linear <- plot(test_data$Trip.Total, lm.pred, xlab = "Actual Trip Total", ylab="Predicted Trip Total", main = "Linear Regression Fitted Plot", pch = "o", col="black", lty = 5)
abline(0,1)

# MARS

set.seed(10)

MARS_model <- earth(finaldf$Trip.Total~., data = finaldf, degree = 2, penalty = 3, pmethod ="backward", nfold=10, ncross=5)

summary(MARS_model) # Gives you the model coefficients and terms
MARS_model$rss 
MARS_model$rsq 

residuals.mars <- c()
MARS.predict <- predict(MARS_model,finaldf,type="response") # Applies the entire data set into the built model
residuals.mars <- (finaldf$Trip.Total-MARS.predict) # Calculates the residual of the model

MARS.mse <- mean((finaldf$Trip.Total-MARS.predict)^2)
MARS.mape <- mean(abs((finaldf$Trip.Total-MARS.predict)/finaldf$Trip.Total) * 100)
MARS.mape

par(mfrow=c(1,1))
qqPlot(residuals.mars, main = "MARS: Residual Plot")


plot(finaldf$Trip.Total, MARS.predict, pch = "o", col="black", lty = 5, xlab = "Actual Trip Total", ylab = "Predicted Trip Total", main = "MARS Fitted Plot")
abline(0,1)

MARS.varimp <- evimp(MARS_model, trim=FALSE)
print(MARS.varimp)
plot(MARS.varimp)

plot.earth.models(MARS_model)

# GAM

GAM_model <- gam(Trip.Total ~ s(Trip.Miles, bs="cr") 
                  + s(Trip.Seconds, bs="cr") + s(temperature, bs="cr") 
                  + s(humidity, bs="cr") + s(windSpeed, bs="cr")
                  + s(visibility, bs="cr") + Pickup.Community.Area + icon, data = finaldf)

par(mfrow=c(1,2))
gam.check(GAM_model)

summary(GAM_model)

par(mfrow=c(2,4))
plot(GAM_model, se=TRUE)

layout(matrix(c(1:1),1,1,byrow=TRUE))
residuals.gam <- c()
gam.predict <- predict(GAM_model, newdata = finaldf, type="response")
residuals.gam <- (finaldf$Trip.Total-gam.predict)

qqPlot(residuals.gam,main = "GAM:Residual Plot") 


plot(finaldf$Trip.Total, gam.predict, pch="o", col='black',lty=5,  main="GAM Fitted Model",
     xlab = "Actual Trip Total", ylab="Predicted Trip Total")
abline(0,1)

GAM.mse <- mean((finaldf$Trip.Total-gam.predict)^2)
GAM.mape <- mean(abs((finaldf$Trip.Total-gam.predict)/finaldf$Trip.Total) * 100)

# Tree Based Models

set.seed(10)

# Fitting Regression Trees

# Split Test and Train

train = sample(1:nrow(treedf), nrow(treedf)/2)
treedf <- finaldf
treedf$icon <- as.factor(treedf$icon)

# Fit a regression tree to predict median home value (medv)

tree.ride = tree(treedf$Trip.Total~., data=treedf, subset=train)

# Check summary of model. Important variables lstat, rm and dis.

summary(tree.ride) 

# Plot regression tree. Don't forget to add text

plot(tree.ride)
text(tree.ride,pretty=0)

# Plot a cross validation tree

cv.ride = cv.tree(tree.ride)
#Check the total deviance with respect to size (number of terminals)

plot(cv.ride$size,cv.ride$dev,type='b', xlab = "Size of Tree", ylab = "Total Deviance", main = "Total Deviance vs Size (terminals)")

# Prune the tree on the best 4 terminal nodes

prune.ride = prune.tree(tree.ride,best=4)
# Plot tree
plot(prune.ride)
text(prune.ride,pretty=0)

# Prediction 

yhat = predict(tree.ride,newdata=treedf[-train,])
ride.test = treedf[-train,"Trip.Total"]

#Results

plot(yhat, ride.test, xlab = "Predicted Trip Total", ylab = "Actual Trip Total", main = "Regression Tree Fitted Model")
abline(0,1)
regtree.mse <- mean((ride.test-yhat)^2)
regtree.mape <- mean(abs((ride.test-yhat)/ride.test) * 100)

# Correlation plot to verify relationship

library(corrplot)
c <- cor(finaldf[,-5])

corrplot(c, method = "circle")


# Model Selection and Validation

par(mfrow=c(2,2))
linear <- plot(test_data$Trip.Total, lm.pred, xlab = "Actual Trip Total", ylab="Predicted Trip Total", main = "Linear Regression Fitted Plot", pch = "o", col="black", lty = 5)
abline(0,1)
MARS <- plot(finaldf$Trip.Total, MARS.predict, pch = "o", col="black", lty = 5, xlab = "Actual Trip Total", ylab = "Predicted Trip Total", main = "MARS Fitted Plot")
abline(0,1)
GAM <- plot(finaldf$Trip.Total, gam.predict, pch="o", col='black',lty=5,  main="GAM Fitted Model",
     xlab = "Actual Trip Total", ylab="Predicted Trip Total")
abline(0,1)
linearTree <- plot(yhat, ride.test, xlab = "Predicted Trip Total", ylab = "Actual Trip Total", main = "Regression Tree Fitted Model")
abline(0,1)

par(mfrow=c(1,1))
mseplot <- barplot(c(lm.mse, MARS.mse, GAM.mse, regtree.mse), names.arg = c("Linear Regression","MARS", "GAM", "Regression Tree"),
        xlab = "Model", ylab = "MSE", main = "MSE of Models")

mseval <- as.numeric(as.character(c(lm.mse, MARS.mse, GAM.mse, regtree.mse)))
text(x = mseplot, y = mseval, label = mseval, pos = 1)

save(sales_MARS, file="./finalmodel.RData")
subset.mse

#----------------------------------------------------------------------------------------------
# -------------- Final Model Selection and Assessment -----------------------------------------
#--------------The code below is the final updated version of our model------------------------
#----------------------------------------------------------------------------------------------
#--------------------Response = Number of Rides------------------------------------------------
#----------------------------------------------------------------------------------------------
# Below will be code for models attempted using the new finaldf
# We are predicting number of rides with the following variables/predictors:
# Temperature, WindSpeed, Humidity, Visibility, Area, Weekday

# Linear Regression

set.seed(10)

# Due to computing constraint, we're going to try using a small sample
# The sample size needs to be small due to running random forest
# It has no problems using the entire set for regression but RF will crash the computer
ridesharing_index <- sample(1:nrow(finaldf), size = 0.05*nrow(finaldf))
ridesharing <- finaldf[ridesharing_index,]

# Split data into test and train

set.seed(10)
train_idx <- sample(1:nrow(ridesharing), size =0.7*nrow(ridesharing))
train.data <- ridesharing[train_idx,]
test.data <- ridesharing[-train_idx,]

# Helper function for calculating RMSE

rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}

# Summarize relationship between rides and each variable using simple scatter plot

par(mfrow=c(2,3))
plot(ridesharing$condition, ridesharing$rides, xlab = "Weather Condition", ylab = "Rides")
plot(ridesharing$temperature, ridesharing$rides, xlab = "Temperature", ylab = "Rides")
plot(ridesharing$windSpeed, ridesharing$rides, xlab = "Wind Speed", ylab = "Rides")
plot(ridesharing$humidity, ridesharing$rides, xlab = "Humidity", ylab = "Rides")
plot(ridesharing$visibility, ridesharing$rides, xlab = "Visibility", ylab = "Rides")
plot(ridesharing$WeekDay, ridesharing$rides, xlab = "Week Day", ylab = "Rides")

#--------------Training Data on Different Models----------------------
####################################################################
# This is how we initialize to see which approach is best for our problem
# We used the same validation approach to control it and make it less bias
train.control <- trainControl(method = "cv", number = 5)

set.seed(10)
glm.fit <- train(rides~., data = ridesharing, method = "glm", trControl = train.control)
set.seed(10)
boost.fit <- train(rides~., data = ridesharing, method = "gbm", trControl = train.control)
set.seed(10)
cart.fit <- train(rides~., data = ridesharing, method = "rpart", trControl = train.control)
set.seed(10)
svm.fit <- train(rides~., data = ridesharing, method = "svmRadial", trControl = train.control)
set.seed(10)
bag.fit <- train(rides~., data = ridesharing, method = "treebag", trControl = train.control)
set.seed(10)
svml.fit <- train(rides~., data = ridesharing, method = "svmLinear", trControl = train.control)
set.seed(10)
mars.fit <- train(rides~., data = ridesharing, method = "earth", trControl = train.control)
set.seed(10)
rf.fit <- train(rides ~., data = ridesharing, method = "ranger", trControl = train.control)


lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length = 100))
set.seed(10)
ridge.fit <- train(rides~., data = ridesharing, method = "ridge", trControl = train.control, tuneGrid = lambdaGrid, preProcess=c('center', 'scale'))
set.seed(10)
lasso.fit <- train(rides~., data = ridesharing, method = "lasso", preProc = c('scale', 'center'), trControl = train.control)

# collect resamples
results <- resamples(list(GLM=glm.fit, BOOST=boost.fit, CART=cart.fit, SVMR=svm.fit, BAG=bag.fit, SVML=svml.fit,MARS=mars.fit, RF = rf.fit, RIDGE=ridge.fit,LASSO=lasso.fit))

# dot plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)

#------------------CART Model (Regression Trees)--------------------------
######################################################################
# Fitting Regression Trees

tree.ride = tree(rides~., data=ridesharing, subset=train_idx)
# Check summary of model. Important variables lstat, rm and dis.
summary(tree.ride) 
# Plot regression tree. Don't forget to add text
plot(tree.ride)
text(tree.ride,pretty=0)

# Plot a cross validation tree
cv.ride=cv.tree(tree.ride, K = 10)
#Check the total deviance with respect to size (number of terminals)
plot(cv.ride$size,cv.ride$dev,type='b', xlab = "Number of Terminals", ylab = "Deviance", main = "Total Deviance vs Size in Regression Tree")

# Prune the tree on the best 15 terminal nodes
prune.ride = prune.tree(tree.ride,best=15)
# Plot tree
plot(prune.ride)
text(prune.ride,pretty=0)

# Prediction 
yhat=predict(prune.ride,newdata=test.data)
yactual = test.data$rides

#Results
plot(yhat,test.data$rides, xlab = "Predicted", ylab = "Actual", main = "Fitted Regression Tree Model")
abline(0,1)
mean((yhat-test.data$rides)^2)
tree_rmse = rmse_reg(prune.ride, test.data, "rides")


#---------------------------Bagging Model------------------------------
#----------------------------------------------------------------------

##############################
# Bagging
##############################

# Check how many trees to fit:

bag.rides <- randomForest(rides ~ ., data=train.data, mtry = ncol(train.data) - 1, importance = TRUE, ntree=400)
plot(bag.rides, type='l', main='MSE by ntree for Bagging')
# Select ~100 trees
bag.rides <- randomForest(rides ~ ., data=train.data, mtry = ncol(train.data) - 1, importance = TRUE, ntree=100)
bag_rmse <- rmse_reg(bag.rides, test.data, "rides")
# Variable importance according to bagging
importance(bag.rides)
varImpPlot(bag.rides)

bag_pred = predict(bag.rides, newdata = test.data)
plot(bag_pred, yactual, xlab = "Predicted", ylab = "Actual", main = "Fitted Bagging Model")
abline(0,1)


##############################
# Random Forest
##############################

# We will try different values of m
rf.mse <- c()
for(i in 1:(ncol(train.data)-1)){
  rf.rides <- randomForest(rides~., data=train.data, mtry=i, importance=TRUE, ntree=80)
  rf.mse[i] <- rf.rides$mse[80]
}
plot(rf.mse, type = 'b,c', main='Training Error by m', xlab='Number of Predictors', ylab='MSE')
# 5 preditors yields the lowest MSE
# Select final model, 5 predictors per tree.
rf.rides <- randomForest(rides~., data=train.data, mtry=5, importance=TRUE, ntree=100)
plot(rf.rides, type = 'l', main = 'MSE by ntree for Random Forest')

rf_rmse <- rmse_reg(rf.rides, test.data, "rides")
# Variable importance according to random forest
importance(rf.rides)
varImpPlot(rf.rides)

rf_pred <- predict(rf.rides, newdata = test.data)
plot(rf_pred, yactual, xlab = "Predicted", ylab = "Actual", main = "Fitted Random Forest Model")
abline(0,1)

# Before, RMSE of RF using base model was 89.6, R-sq 0.76, MAE of 34.63
rsq <- function (x, y) cor(x, y) ^ 2
rf_rsq <- rsq(rf_pred, yactual) # 0.744
rf_mae <- mae(rf_pred, yactual) # 30.812

# Partial Dependence Plot Comparing Random Forest vs Bagging
par(mfrow=c(1,2))
partialPlot(rf.rides, test.data, x.var = "condition", main = "PDP (Condition) in Random Forest")
partialPlot(bag.rides, test.data, x.var = "condition", main = "PDP (Condition) in Bagging")

partialPlot(rf.rides, test.data, x.var = "temperature", main = "PDP (Temperature) in Random Forest")
partialPlot(bag.rides, test.data, x.var = "temperature", main = "PDP (Temperature) in Bagging")

partialPlot(rf.rides, test.data, x.var = "humidity", main = "PDP (Humidity) in Random Forest")
partialPlot(bag.rides, test.data, x.var = "humidity", main = "PDP (Humidity) in Bagging")

partialPlot(rf.rides, test.data, x.var = "windSpeed", main = "PDP (Wind Speed) in Random Forest")
partialPlot(bag.rides, test.data, x.var = "windSpeed", main = "PDP (Wind Speed) in Bagging")

partialPlot(rf.rides, test.data, x.var = "visibility", main = "PDP (Visibility) in Random Forest")
partialPlot(bag.rides, test.data, x.var = "visibility", main = "PDP (Visibility) in Bagging")

# From the PDP plot, we are able to identify the relationship that each weather factor has on the number of rides
# There are some interesting observations
# This is our final model for this project
# Further improvements can be potentially made by exploring other possible weather factors
# or even hypertuning the parameters to achieve a more accurate model for either random forest or bagging model

#--------------------------------------------------------------------------------------------------------------
#----------------------END OF CODE-----------------------------------------------------------------------------


