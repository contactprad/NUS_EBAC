library(data.table)
library(ggplot2)
library(lubridate)
library(aod)
library(sqldf)
library(stringr)
library(bit64)
library(gdata)
library(dummies)
library(rpart)
library(e1071)
library(VIM)
library(caret)
library(klaR)
library(psych)
library(pROC)

# install.packages("dummies")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load FIFA Player profile dataset
FIFA <- read.csv('./FIFA_CompleteDataset.csv')

# Taking off unnecessary input variables from dataframe
FIFA_NUM <- FIFA[,!names(FIFA) %in% c("Name","Age","Photo","Nationality","Flag","Potential",
                                     "Club","Club.Logo","Preferred.Positions")]

# Converting characters to numeric
FIFA_NUM <- data.frame(lapply(FIFA_NUM, function(x) as.numeric(as.character(x))))

# md.pattern(FIFA_NUM)
# Filling up missing Values in Numerical variables with kNN imputation
FIFA_NUM_imputed <- kNN(FIFA_NUM)

options(max.print=1000000)
# Saving the imputed dataframe for future use
save(FIFA_NUM_imputed, file='./FIFA_NUM_imputed.Rda')
load('./FIFA_NUM_imputed.Rda')
FIFA_NUM_imputed <- FIFA_NUM_imputed[c(1:66)]

# write.csv(FIFA_NUM_imputed, './CompleteDataset_Muni.csv', row.names = FALSE)

# Remove the dependent and identifier variables
my_data <- subset(FIFA_NUM_imputed, select = -c(X, Overall))

# Finding correlation between different numerical inputs
x <- my_data
y <- my_data
cor(x, y)

# Check available variables
colnames(my_data)

# Since PCA works on numeric variables, let's see if we have any variable other than numeric
str(my_data)

# Base R function prcomp() is used to perform PCA. By default, it centers the variable to have mean equals to zero
# With parameter "scale. = T", we normalize the variables to have standard deviation equals to 1
prin_comp <- prcomp(my_data, scale. = T)
# Without parameter "scale. = T"
prin_comp <- prcomp(my_data)
names(prin_comp)

# Outputs the mean of variables
prin_comp$center

# Outputs the standard deviation of variables
prin_comp$scale

# Provides the principal component loading
prin_comp$rotation

# Plotting the resultant Principal Components
# The parameter scale = 0 ensures that arrows are scaled to represent the loadings
biplot(prin_comp, scale = 0)

# The prcomp() function also provides the facility to compute standard deviation of each principal component
# sdev refers to the standard deviation of principal components
std_dev <- prin_comp$sdev

# Compute Variance
pr_var <- std_dev^2

# Check variance of first 10 components
pr_var[1:10]

# To compute the proportion of variance explained by each component, 
# we simply divide the variance by sum of total variance
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

# In order to decide how many components should we select for modeling stage, we use scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# The plot above shows that ~ 6 components explains around 98.4% variance in the dataset
# Using PCA we have reduced 65 predictors to 30 without compromising on explained variance
# Let's do a confirmation check, by plotting a cumulative variance plot
# This will give us a clear picture of number of components
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
