
import pandas as pd

df =  pd.read_csv(r'D:\github\NUS_EBAC\BA-Huge-File\train\train.txt', sep='\t', nrows=500)
df.describe()
df.min()
len(df.columns)

