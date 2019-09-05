library(data.table)
library(ggplot2)
library(lubridate)
library(aod)
library(sqldf)
library(stringr)
library(bit64)
library(gdata)
library(randomForest)
library(MASS)
library(neuralnet)
library(caretEnsemble)
library(caret)
library(nnet)
library(forecast)
library(e1071)

install.packages("e1071")
#===================================================================================================
#===================================================================================================
# Generic Functions
#===================================================================================================
#===================================================================================================
moving_fun <- function(x, w, FUN, ...) {
  # x: a double vector
  # w: the length of the window, i.e., the section of the vector selected to apply FUN
  # FUN: a function that takes a vector and return a summarize value, e.g., mean, sum, etc.
  if (w < 1) {
    stop("The length of the window 'w' must be greater than 0")
  }
  output <- x
  for (i in 1:length(x)) {
    # plus 1 because the index is inclusive with the upper_bound 'i'
    lower_bound <- i - w + 1
    if (lower_bound < 1) {
      output[i] <- NA_real_
    } else {
      output[i] <- FUN(x[lower_bound:i, ...])
    }
  }
  output
}

lagpad <- function(x, k) {
  # x: attribute/column name
  # k: the length of the window, postive will move the field down and negative will move up
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

#===================================================================================================
#===================================================================================================
# Default Prediction
#===================================================================================================
#===================================================================================================
mydata <- fread('./day.csv')
mydata[,output := lagpad(cnt, 2)]
bikeshare_test <- mydata[(dteday >= as.Date('2012-01-01','%Y-%m-%d')) & (dteday <= as.Date('2012-12-31','%Y-%m-%d'))]

totalPuchaseVendorModel = sum(bikeshare_test$output*2)
totalVendorRevenue = sum(pmin(bikeshare_test$output, bikeshare_test$cnt)*3)
ProfitOrLoss = totalVendorRevenue-totalPuchaseVendorModel
ProfitOrLoss

#===================================================================================================
#===================================================================================================
# Data Transformation
#===================================================================================================
#===================================================================================================

# Reading the dataset
mydata <- fread('./day.csv')

# Converting the date to format YYYY-MM-DD
mydata[,dteday:=as.Date(dteday,'%Y-%m-%d')]

# Ordering the dataset as per dteday
mydata <- mydata[order(dteday)]

# Defining whether a particular day number is weekday or not, 0 and 6 are Sunday and Saturday respectively
mydata$weekday <- ifelse((mydata$weekday == 0) | (mydata$weekday == 6), 0, 1)

# Replacing the (n-2) value with mean of n-(2+x) to n-2 where x=7 for weather related fields
mov = 7
mydata[,weathersit := moving_fun(weathersit, mov, mean)]
mydata[,temp := moving_fun(temp, mov, mean)]
mydata[,atemp := moving_fun(atemp, mov, mean)]
mydata[,hum := moving_fun(hum, mov, mean)]
mydata[,casual_lag := moving_fun(casual, mov, mean)]
mydata[,registered_lag := moving_fun(registered, mov, mean)]
mydata[,cnt_lag := moving_fun(cnt, mov, mean)]
mydata[,windspeed := moving_fun(windspeed, mov, mean)]

# Shifting the weather related fields to record corresponds to 2 days later.
# Shifting the 1st Jan record to 3rd Jan record.
mydata[,weathersit := lagpad(weathersit, 2)]
mydata[,temp := lagpad(temp, 2)]
mydata[,atemp := lagpad(atemp, 2)]
mydata[,hum := lagpad(hum, 2)]
mydata[,windspeed := lagpad(windspeed, 2)]
mydata[,casual_lag := lagpad(casual_lag, 2)]
mydata[,registered_lag := lagpad(registered_lag, 2)]
mydata[,cnt_lag := lagpad(cnt_lag, 2)]
mydata[,feel_diff := atemp*50 - temp*41]

# Splitting the data to train and test.
bikeshare_train <- mydata[(dteday >= as.Date('2011-01-09','%Y-%m-%d')) & (dteday <= as.Date('2011-12-31','%Y-%m-%d'))]
bikeshare_test <- mydata[(dteday >= as.Date('2012-01-01','%Y-%m-%d')) & (dteday <= as.Date('2012-12-31','%Y-%m-%d'))]

# Creating two output variables each for casual and registered in order to create
# different models for each of them. Creating single model on cnt will not lead to better prediction.
bikeshare_train[,output1 := casual]
bikeshare_train[,output2 := registered]

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GLM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Training the model for target variable casual using GLM
mylogit <- glm(output1 ~ season+workingday+weathersit+atemp+feel_diff+hum+windspeed+casual_lag, data = bikeshare_train, family = "gaussian")
bikeshare_test$output1 <- predict(mylogit, newdata = bikeshare_test, type = "response")

# Training the model for target variable registered using GLM
mylogit <- glm(output2 ~ season+workingday+weathersit+atemp+feel_diff+hum+windspeed+registered_lag, data = bikeshare_train, family = "gaussian")
bikeshare_test$output2 <- predict(mylogit, newdata = bikeshare_test, type = "response")

# Aggregating the results to predict final cnton test dataset
bikeshare_test[,output:=output1+output2]
bikeshare_test$output <- ifelse(bikeshare_test$output > 5000, bikeshare_test$output+1600, 
                          ifelse(bikeshare_test$output > 4000, bikeshare_test$output+1300, 
                          ifelse(bikeshare_test$output > 3000, bikeshare_test$output+1100, bikeshare_test$output)))

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Model prediction
#++++++++++++++++++++++++++++++++++++++++++++++++++
totalpurchase = sum(bikeshare_test$output*2)
totalRevenue = sum(pmin(bikeshare_test$output, bikeshare_test$cnt)*3)
ProfitOrLoss = totalRevenue - totalpurchase
ProfitOrLoss
#++++++++++++++++++++++++++++++++++++++++++++++++++
# Error Calculation for GLM
#++++++++++++++++++++++++++++++++++++++++++++++++++
error = bikeshare_test$output - bikeshare_test$cnt
rmse(error)
mae(error)
plot(error)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random Forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Training the model for target variable casual using RandomForest
random_forest <- randomForest(output1 ~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+casual_lag, data = bikeshare_train)
bikeshare_test$output1 <- predict(random_forest, newdata = bikeshare_test, type = "response")

# Training the model for target variable registered using RandomForest
random_forest <- randomForest(output2 ~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+registered_lag, data = bikeshare_train)
bikeshare_test$output2 <- predict(random_forest, newdata = bikeshare_test, type = "response")

# Aggregating the results to predict final cnton test dataset
bikeshare_test[,output:=output1+output2]
bikeshare_test$output <- ifelse(bikeshare_test$output > 5000, bikeshare_test$output+1600, 
                          ifelse(bikeshare_test$output > 4000, bikeshare_test$output+1300, 
                          ifelse(bikeshare_test$output > 3000, bikeshare_test$output+1100, bikeshare_test$output)))

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Model prediction
#++++++++++++++++++++++++++++++++++++++++++++++++++
totalpurchase = sum(bikeshare_test$output*2)
totalRevenue = sum(pmin(bikeshare_test$output, bikeshare_test$cnt)*3)
ProfitOrLoss = totalRevenue - totalpurchase
ProfitOrLoss
#++++++++++++++++++++++++++++++++++++++++++++++++++
# Error Calculation for Random Forest
#++++++++++++++++++++++++++++++++++++++++++++++++++
error = bikeshare_test$output - bikeshare_test$cnt
rmse(error)
mae(error)
plot(error)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ensemble Stacking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Defining the Ensemble and algorithm list
myControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rpart', 'glm', 'knn')
set.seed(123)

# Training the model for target variable casual using Stacking
models_casual <- caretList(output1~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+casual_lag, data=bikeshare_train, trControl=myControl, methodList=algorithmList)

# Training the model for target variable registered using Stacking
models_reg <- caretList(output2~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+registered_lag, data=bikeshare_train, trControl=myControl, methodList=algorithmList)

# Clubbing the different models using glm
stack.glm_casual <- caretStack(models_casual, method="glm", trControl=myControl)
stack.glm_reg <- caretStack(models_reg, method="glm", trControl=myControl)
print(stack.glm_casual)
print(stack.glm_reg)

# Predicting the output for individual targets, casual and registered
bikeshare_test$output1 <- predict(stack.glm_casual, newdata = bikeshare_test, type = "raw")
bikeshare_test$output2 <- predict(stack.glm_reg, newdata = bikeshare_test, type = "raw")

# Aggregating the results to predict final cnton test dataset
bikeshare_test[,output:=output1+output2]
bikeshare_test$output <- ifelse(bikeshare_test$output > 5000, bikeshare_test$output+1600, 
                          ifelse(bikeshare_test$output > 4000, bikeshare_test$output+1300, 
                          ifelse(bikeshare_test$output > 3000, bikeshare_test$output+1100, bikeshare_test$output)))

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Model prediction
#++++++++++++++++++++++++++++++++++++++++++++++++++
totalpurchase = sum(bikeshare_test$output*2)
totalRevenue = sum(pmin(bikeshare_test$output, bikeshare_test$cnt)*3)
ProfitOrLoss = totalRevenue - totalpurchase
ProfitOrLoss
#++++++++++++++++++++++++++++++++++++++++++++++++++
# Error Calculation for Ensemble Stacking
#++++++++++++++++++++++++++++++++++++++++++++++++++
error = bikeshare_test$output - bikeshare_test$cnt
rmse(error)
mae(error)
plot(error)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Neural Network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Training the model for target variable casual using Stacking
nn <- nnet(output1~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+casual_lag,data=bikeshare_train, hidden=1, size = 10, decay=0.01, maxit=1000, linout=TRUE)
# Predicting the output for individual target casual
bikeshare_test$output1 <- predict(nn, bikeshare_test)

# Training the model for target variable registered using Stacking
nn <- nnet(output2~season+workingday+weathersit+atemp+feel_diff+hum+windspeed+registered_lag,data=bikeshare_train, hidden=1, size = 10, decay=0.01, maxit=1000, linout=TRUE)
# Predicting the output for individual target registered
bikeshare_test$output2 <- predict(nn, bikeshare_test)

# Aggregating the results to predict final cnton test dataset
bikeshare_test[,output:=output1+output2]
bikeshare_test$output <- ifelse(bikeshare_test$output > 5000, bikeshare_test$output+1600, 
                          ifelse(bikeshare_test$output > 4000, bikeshare_test$output+1300, 
                          ifelse(bikeshare_test$output > 3000, bikeshare_test$output+1100, bikeshare_test$output)))

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Model prediction
#++++++++++++++++++++++++++++++++++++++++++++++++++
totalpurchase = sum(bikeshare_test$output*2)
totalRevenue = sum(pmin(bikeshare_test$output, bikeshare_test$cnt)*3)
ProfitOrLoss = totalRevenue - totalpurchase
ProfitOrLoss
#++++++++++++++++++++++++++++++++++++++++++++++++++
# Error Calculation for Neural Network
#++++++++++++++++++++++++++++++++++++++++++++++++++
error = bikeshare_test$output - bikeshare_test$cnt
rmse(error)
mae(error)
plot(error)

