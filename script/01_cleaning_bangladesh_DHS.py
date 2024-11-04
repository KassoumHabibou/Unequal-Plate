#Importing library
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt

# Importing the datasets
df = pd.read_stata(os.getcwd() + str('\\input\\individual_data\\dat\\idhs_00020.dta')) 

# See variables in the dataset
df.head()

# Checking info of the dataset
df.info()

# Checking unique values in each column
df.nunique()

# duplication
df[df.duplicated()]

# Missing values
pd.DataFrame(data={'% of Missing Values':round(df.isna().sum()/df.isna().count()*100,2),'# of Missing Values':df.isna().sum()}).sort_values(by='% of Missing Values',ascending=False)


#outcome variable 
outcomes_cols = ['HWHAZWHO', 'HWWAZWHO', 'HWWHZWHO','HWBMIZWHO']

#Summarize data
df.describe().T

# View
df[outcomes_cols].boxplot(figsize=(20,10))
plt.show()