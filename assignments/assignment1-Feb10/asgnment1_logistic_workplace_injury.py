'''This is just a sample Python script to analyze the data and run regression using Python
This is just a learning script and original R script is for assignment submission'''

import pandas as pd
import statsmodels.api as sm
import pylab as pl
import numpy as np
import matplotlib.pyplot as plt
import sys, os


#STAGE1: Data Ingestion: Add the path to current working directory in preferences -> console

filePath = os.path.join(
                        r"data\workplace-injuries-by-industry-and-incident-types.csv")
df  = pd.read_csv(filePath)

#Add another column which  checks that if the injury involved more than 2 people
df['target_variable_gt_2_affected'] = (df['no._of_injuries'] > 2).astype(int)

#Stage2: Data Cleanup Delete the column for Year from the dataframe
#Check if the dataframe has any Null values
try:
    df.drop('year', axis=1, inplace=True)
    df.drop('no._of_injuries', axis=1, inplace=True)
except:
    print "There is no such key as Year"

#Factorize the variable to prepare for Logit
df.describe()
df.dtypes
dummy_columns_list = []
for i in df.columns.values:
    j=0
    print i
    if i == 'target_variable_gt_2_affected':
        continue
    dummy_columns_list.append(pd.get_dummies(df[i], prefix=i))
    print dummy_columns_list[j].head()
    j=j+1
    

cols_to_keep = ['target_variable_gt_2_affected']
data = df[cols_to_keep].join(dummy_columns_list[0])
for i in range(1, len(dummy_columns_list)):
    data = data.join(dummy_columns_list[i])
     
#separate out the train and test set
print len(data)
rand_df = pd.DataFrame(np.random.randn(len(data), 2))
msk = np.random.random(len(rand_df)) < 0.8
train_df = data[msk]

test_df = data[~msk]

train_df['intercept'] = 1.0
train_df.describe()
train_df.to_csv(path_or_buf=r"d:\test.csv")
train_cols = train_df.columns[1:]
print train_cols

glm_binom = sm.GLM(train_df['target_variable_gt_2_affected'], train_df[train_cols], family=sm.families.Binomial())
res = glm_binom.fit()
print(res.summary())
print res.conf_int
print np.exp(res.params)

