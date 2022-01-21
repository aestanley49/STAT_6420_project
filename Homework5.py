#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  4 13:31:48 2021

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
path="/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project/HW5"
os.chdir(path)


####### Problem 1

data1 = pd.read_csv("data5.1.txt", delim_whitespace=True)


### # 1a 

data1.head()

### #1b

model = smf.ols(formula= 'x~y' , data=data1).fit()

print(model.summary())


### #1c 

sns.regplot('x', 'y', data=data1, ci=99, scatter_kws={'color':'r', 's':20})
plt.show()

### ### ### ****** Need to save this plot 

### #1c 
  # Draw residual versus fitted plot, and mark the two largest (in absolute values) residuals


f = model.fittedvalues # fitted values
r = model.resid # residuals


ar = np.abs(r) # absolute residuals
ar_sort = ar.sort_values(ascending = False)
top_2_ar = ar_sort[0:3] # top 2 absolute residuals

sns.residplot(f,r)
plt.title("Residuals vs. Fitted")
plt.ylabel("Residuals")
plt.xlabel("Fitted values")

# Here we identify and mark the three largest (in absulute values) residuals
for i in top_2_ar.index:
    plt.annotate(i, xy = (f[i], r[i]))
plt.show()

# ******* this isn't working.. 


  # Now draing QQ plot with reference line
  
sm.qqplot(model.resid)
plt.title("Normal probability plot of residuals")
plt.show()


# Import required packages
from statsmodels.graphics.gofplots import ProbPlot
# Normal QQ Plot
QQ = ProbPlot(r)
QQ.qqplot(line = 's', color='C2', lw=1) # adding the reference line
plt.title("Normal probability plot of residuals")
plt.show()
