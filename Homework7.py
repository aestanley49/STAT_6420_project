#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  9 11:56:54 2021

@author: annabellestanley
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
import seaborn as sns

#import os
#print os.getcwd()  # Prints the current working directory


import os
path="/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project/HW7"
os.chdir(path)


####### Problem 1

data6 = pd.read_csv("data7.6.txt", delim_whitespace=True)

                       # preview of first 5 of the explanatory variables
data6.head()
data6.shape
                       # pandas drop a column with drop function
X = data6.drop(["RATE"], axis=1)
y = data6["RATE"]
                       # Using Pearson Correlation
plt.figure(figsize=(12,10))
cor = X.corr()
sns.heatmap(cor, annot=True, cmap=plt.cm.Reds)
plt.show()

