library(data.table)
library(ggplot2)
library(lubridate)
library(aod)
library(sqldf)
library(stringr)
library(bit64)
library(gdata)
library(e1071)
library(mice)
library(VIM)
library(caret)
library(klaR)
library(psych)
library(pROC)

#install.packages("psych")
#install.packages("klaR")
#install.packages("VIM")
#install.packages("pROC")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Naive Bayes Classifier
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reading the input file
mydata <- fread('./vehicle_safety_NASS2010_2000_2012.csv')

#md.pattern(mydata1)
# Filling up missing Values in Numerical variables with kNN imputation
#BEWARE: It takes approx. 30 minutes to run kNN to impute the data. Uncomment to try
#mydata_imputed <- kNN(mydata)

# Saving the imputed dataframe for future use
#save(mydata1, file='./mydata1.Rda')
load('./mydata1.Rda')

# Finding correlation between different numerical inputs
x <- mydata1[c(1:8,10,12:14,16,18:21)]
y <- mydata1[c(1:8,10,12:14,16,18:21)]
cor(x, y)

mydata <- mydata1[c(1:20)]

# Separate groups are created for each category of 'Passenger Car', 'Truck (<=6000 lbs.)' and 'Truck (<=10000 lbs.)'
mydata$GV_WGTCDTR <- ifelse((mydata$GV_WGTCDTR == 'Passenger Car'), 1, 
                            ifelse((mydata$GV_WGTCDTR == 'Truck (<=6000 lbs.)'), 2, 3))
mydata$GV_WGTCDTR <- as.factor(mydata$GV_WGTCDTR)

# # Separate groups are created for 'Deployed' Vs 'Not Deployed'
mydata$OA_BAGDEPLY <- ifelse((mydata$OA_BAGDEPLY == 'Not Deployed'), 0, 1)
mydata$OA_BAGDEPLY <- as.factor(mydata$OA_BAGDEPLY)

# Separate groups are created for 'Male' Vs 'Female'
mydata$OA_SEX <- ifelse((mydata$OA_SEX == 'Male'), 1, 2)
mydata$OA_SEX <- as.factor(mydata$OA_SEX)

# Separate groups are created for each category of 'Left', 'Right', 'Rear' and 'Front'
mydata$VE_GAD1 <- ifelse((mydata$VE_GAD1 == 'Left'), 1, 
                         ifelse((mydata$VE_GAD1 == 'Right'), 2, 
                                ifelse((mydata$VE_GAD1 == 'Front'), 3, 4)))
mydata$VE_GAD1 <- as.factor(mydata$VE_GAD1)

# Four different bins are created intuitively for the given variable
mydata$OA_MAIS <- ifelse((mydata$OA_MAIS == 0) | (mydata$OA_MAIS == 1), 1, 
                         ifelse((mydata$OA_MAIS == 2), 2, 
                                ifelse((mydata$OA_MAIS == 3) | (mydata$OA_MAIS == 4), 3, 4)))
mydata$OA_MAIS <- as.factor(mydata$OA_MAIS)

# Separate bins are created for cars manufactured before 2002 and after 2008
mydata$GV_MODELYR <- ifelse((mydata$GV_MODELYR < 2002), 1, 
                         ifelse((mydata$GV_MODELYR >= 2002) & (mydata$GV_MODELYR < 2004), 2, 
                                ifelse((mydata$GV_MODELYR >= 2004) & (mydata$GV_MODELYR < 2006), 3, 
                                       ifelse((mydata$GV_MODELYR >= 2006) & (mydata$GV_MODELYR < 2008), 4, 5))))
mydata$GV_MODELYR <- as.factor(mydata$GV_MODELYR)

# Separate bins are created for vehicle weighing below 1000 Kg and above 2500 kg
mydata$GV_CURBWGT <- ifelse((mydata$GV_CURBWGT < 800), 1, 
                            ifelse((mydata$GV_CURBWGT >= 800) & (mydata$GV_CURBWGT < 1400), 2, 
                                   ifelse((mydata$GV_CURBWGT >= 1400) & (mydata$GV_CURBWGT < 2000), 3, 
                                          ifelse((mydata$GV_CURBWGT >= 2000) & (mydata$GV_CURBWGT < 2600), 4, 5))))
mydata$GV_CURBWGT <- as.factor(mydata$GV_CURBWGT)

# Four bins are created for values ranging from -30 Kmph to 30 kmph which are uniformly distributed at intervals of 20Kmph
mydata$GV_DVLAT <- ifelse((mydata$GV_DVLAT < -20), 1, 
                          ifelse((mydata$GV_DVLAT >= -20) & (mydata$GV_DVLAT < 0), 2, 
                                 ifelse((mydata$GV_DVLAT >= 0) & (mydata$GV_DVLAT < 20), 3, 4)))
mydata$GV_DVLAT <- as.factor(mydata$GV_DVLAT)

# Four bins are created for the values ranging from -25 Kmph to 25 kmph which are uniformly distributed at an interval of 25Kmph
mydata$GV_DVLONG <- ifelse((mydata$GV_DVLONG < -25), 1, 
                          ifelse((mydata$GV_DVLONG >= -25) & (mydata$GV_DVLONG < 0), 2, 
                                 ifelse((mydata$GV_DVLONG >= 0) & (mydata$GV_DVLONG < 25), 3, 4)))
mydata$GV_DVLONG <- as.factor(mydata$GV_DVLONG)

# We followed the original distribution of dataset and applied equal width binning algorithm
mydata$GV_ENERGY <- ifelse((mydata$GV_ENERGY < 600), 1, 
                           ifelse((mydata$GV_ENERGY >= 600) & (mydata$GV_ENERGY < 1200), 2, 
                                  ifelse((mydata$GV_ENERGY >= 1200) & (mydata$GV_ENERGY < 1800), 3, 4)))
mydata$GV_ENERGY <- as.factor(mydata$GV_ENERGY)

# Binning was done and record was distributed into three categories
mydata$GV_LANES <- ifelse((mydata$GV_LANES < 3), 1, 
                           ifelse((mydata$GV_LANES >= 3) & (mydata$GV_LANES < 5), 2, 3))
mydata$GV_LANES <- as.factor(mydata$GV_LANES)

# We followed the original distribution of dataset and applied equal width binning algorithm
mydata$GV_OTVEHWGT <- ifelse((mydata$GV_OTVEHWGT < 1000), 1, 
                             ifelse((mydata$GV_OTVEHWGT >= 1000) & (mydata$GV_OTVEHWGT < 1500), 2, 
                                    ifelse((mydata$GV_OTVEHWGT >= 1500) & (mydata$GV_OTVEHWGT < 2000), 3, 4)))
mydata$GV_OTVEHWGT <- as.factor(mydata$GV_OTVEHWGT)

# We followed the original distribution of dataset and applied equal width binning algorithm
mydata$GV_SPLIMIT <- ifelse((mydata$GV_SPLIMIT < 30), 1, 
                            ifelse((mydata$GV_SPLIMIT >= 30) & (mydata$GV_SPLIMIT < 45), 2, 
                                   ifelse((mydata$GV_SPLIMIT >= 45) & (mydata$GV_SPLIMIT < 60), 3, 4)))
mydata$GV_SPLIMIT <- as.factor(mydata$GV_SPLIMIT)

# As with other continuous variables, age was also binned into 5 groups using unequal width binning
mydata$OA_AGE <- ifelse((mydata$OA_AGE < 25), 1, 
                        ifelse((mydata$OA_AGE >= 25) & (mydata$OA_AGE < 35), 2, 
                               ifelse((mydata$OA_AGE >= 35) & (mydata$OA_AGE < 50), 3, 
                                      ifelse((mydata$OA_AGE >= 50) & (mydata$OA_AGE < 60), 4, 5))))
mydata$OA_AGE <- as.factor(mydata$OA_AGE)

# Separate bins were created
mydata$VE_PDOF_TR <- ifelse((mydata$VE_PDOF_TR < 20), 1, 
                        ifelse((mydata$VE_PDOF_TR >= 20) & (mydata$VE_PDOF_TR < 30), 2, 
                               ifelse((mydata$VE_PDOF_TR >= 30) & (mydata$VE_PDOF_TR < 40), 3, 
                                      ifelse((mydata$VE_PDOF_TR >= 40) & (mydata$VE_PDOF_TR < 50), 4, 
                                             ifelse((mydata$VE_PDOF_TR >= 50) & (mydata$VE_PDOF_TR < 60), 5, 6)))))
mydata$VE_PDOF_TR <- as.factor(mydata$VE_PDOF_TR)

write.csv(mydata, './DATA.csv', row.names = FALSE)

# Creating training and validation set in the ration 70:30
mydata.size <- length(mydata$OA_MAIS)
mydata.train.size <- round(mydata.size * 0.7)
mydata.validation.size <- mydata.size - mydata.train.size
mydata.train.idx <- sample(seq(1:mydata.size), mydata.train.size)
mydata.train.sample <- mydata[mydata.train.idx,]
mydata.validation.sample <- mydata[-mydata.train.idx,]

# 
nb_model <- naiveBayes(
                      subset(mydata.train.sample, select = -OA_MAIS),
                      mydata.train.sample$OA_MAIS, laplace=1
                      )

nb_model

preds <- predict(nb_model,
                 subset(mydata.validation.sample, select = -OA_MAIS))

table(preds, mydata.validation.sample$OA_MAIS)

round(sum(preds == mydata.validation.sample$OA_MAIS, na.rm = TRUE) /
        length(mydata.validation.sample$OA_MAIS), digits = 2)

confusionMatrix(table(preds, mydata.validation.sample$OA_MAIS))

plot.roc(as.numeric(mydata.validation.sample$OA_MAIS), as.numeric(preds))
  
  