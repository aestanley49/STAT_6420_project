# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf

#import os
#print os.getcwd()  # Prints the current working directory


import os
path="/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project/HW3"
os.chdir(path)

####### Problem 1

df1 = pd.read_csv("data3.1.txt", delim_whitespace=True)


model = smf.ols(formula= 'x~y' , data=df1).fit()

import sklearn
from sklearn import linear_model


X = df1.iloc[:, 0].values.reshape(-1, 1)  # values converts it into a numpy array
Y = df1.iloc[:, 1].values.reshape(-1, 1)  # -1 means that calculate the dimension of rows, but have 1 column
linear_model = LinearRegression()  # create object for the class
linear_regressor.fit(X, Y)  # perform linear regression
Y_pred = linear_regressor.predict(X)  # make predictions
plt.scatter(X, Y)
plt.plot(X, Y_pred, color='red')
plt.show()



####### Problem 4 

df = pd.read_csv("data3.4.txt", delim_whitespace=True)

df.head()


# fit a model20

model1 = smf.ols(formula= 'sr~pop15 + pop75 + dpi + ddpi' , data=df).fit()
 
#print model summary

model1.summary()





#os.chdir("/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project")  # Provide the new path here


#f = open("/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project/HW3/data3.4.txt")

#with open("/Users/annabellestanley/Documents/R_projects/Classes/STAT_6420/STAT_6420_project/HW3/data3.4.txt") as f:
#    mylist = [line.rstrip('\n') for line in f]

#print(f.head.read())

1 + 1 


x = list(range(5))
x

import numpy as np 
x = np.array([1,3,5])
x
x2 = x**2
x2
 
 
y = [1,3,5]
y2 = y**2 # incorrect
y2 = [yi**2 for yi in y] # correct
y2
ylog = [math.log(yi) for yi in y] ## issue with math
ylog
yexp = [math.exp(yi) for yi in y]
yexp
y + y2 # concatenate two lists
[yi + y2i for yi,y2i in zip(y,y2)] # element-wise addition
