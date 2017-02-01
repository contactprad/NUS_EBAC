import pandas as pd
import statsmodels.api as sm
import pylab as pl
import numpy as np
import matplotlib.pyplot as plt
import sys


#STAGE1: Data Ingestion: Add the path to current working directory in preferences -> console

filePath = os.path.join(r"data\workplace-injuries-by-industry-and-incident-types.csv")
df  = pd.read_csv(filePath)

#Add another column which  checks that if the injury involved more than 2 people
df['target_variable_gt_2_affected'] = (df['no._of_injuries'] > 2).astype(int)

#Stage2: Data Cleanup Delete the column for Year from the dataframe
#Check if the dataframe has any Null values
try:
    df.drop('year', axis=1, inplace=True)
except:
    print "There is no such key as Year"

#Factorize the variable to prepare for Logit
df.describe()
df.dtypes
dummy_degree_of_injury = pd.get_dummies(df['degree_of_injury'], prefix='degree_of_injury')
print dummy_degree_of_injury.head()

cols_to_keep = ['target_variable_gt_2_affected']
data = df[cols_to_keep].join(dummy_degree_of_injury)
print data.head()
data['intercept'] = 1.0

train_cols = data.columns[1:]
print train_cols
logit = sm.Logit(data['target_variable_gt_2_affected'], data[train_cols])
  # fit the model
result = logit.fit()
print result.summary()
print result.conf_int()
print np.exp(result.params)


