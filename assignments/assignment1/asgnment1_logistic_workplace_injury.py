import pandas as pd
import statsmodels.api as sm
import pylab as pl
import numpy as np
import matplotlib.pyplot as plt
import sys


#Add the path to current working directory in preferences -> console

filePath = os.path.join(r"data\workplace-injuries-by-industry-and-incident-types.csv")
df1  = pd.read_csv(filePath)

#Add another column which  checks the 
df1['target_variable_gt_2_affected'] = (df['no._of_injuries'] > 2).astype(int)

#Delete the column for Year from the dataframe
try:
    df1.drop('year', axis=1, inplace=True)
except:
    print "There is no such key as Year"

#summarize the data/
df1.describe()
for col in ['degree_of_injury','industry', 'sub_industry', 'incident_type', 'incident_agent', 'incident_agent_sub_type']:
                df1[col] = df[col].astype('category')
df1.dtypes
dummy_industry = pd.get_dummies(df1['industry'], prefix='industry')
print dummy_industry.head()

cols_to_keep = ['degree_of_injury', 'target_variable_gt_2_affected']
data = df1[cols_to_keep]
print data.head()
data.describe()

data['intercept'] = 1.0
train_cols = data.columns[1:]

logit = sm.Logit(data['target_variable_gt_2_affected'], data['degree_of_injury'])
result = logit.fit()
print result.summary()
print result.conf_int()
print np.exp(result.params)