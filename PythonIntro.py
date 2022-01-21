#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 26 22:10:37 2021

@author: annabellestanley
"""

### ### ### 1 Basics

### Vectors can be created via list (square brackets [ ]), tuple (parentheses ( )) or numpy
x = [0,1,2,3,4] #which is the same as.. 
x = list(range(5))
x1 = [i for i in range(5)]

x = (0,1,2,3,4) ## stored as a tuple

import numpy as np #load 'numpy' package
x = np.arange([i for i in range(5)]) # same thing but something is wrong with the code here..

x = np.arange(5) #same thing
x = np.arange(0,10,0.1) #0.0,0.1,0.2,...,9.9
x = np.arange(10) #0,1,2,...,9
x[0] = 17 # replaces the x written in the previous line with 17 
print(x)

x[3:5] = 0
print(x)
 
w = x[-1] # last element in x
w



#### only numpy arrays support vectorized operations. For list objects, you need to loop each element via list comprehension.
x = np.array([1,3,5]) 
x
x2= x**2
x2
xlog = np.log(x)
xlog
xexp = np.exp(x)
xexp
x+x2

import math
y = [1,3,5]
y2 = y**2 ## incorrect
y2 = [yi**2 for yi in y] ## correct 
y2
ylog = [math.log(yi) for yi in y]
ylog
yexp = [math.exp(yi) for yi in y]
yexp
y+y2 # concatenate two lists
[yi + y2i for yi,y2i in zip(y,y2)] # element-wise addition

y = np.array([5,4,3,2,1,5,4,3,2,1])
y == 2 # a boolean vector
z = (y < 3); print(z)  # seperate two commands by a semi-colon
y[y<3] ## selects elements from y that fit the criteria (filter)
sorted(y) #orders from smallest to largest 




### ### ### 2 Matrices and Dictionaries

## To create a matrix, use the numpy package as follows:

import numpy as np  
aa = np.array([1, 2, 3, 4, 5, 0.5, 2, 6, 0, 1, 1, 0])
aa
mm = aa.reshape((4,3))
mm
mm = aa.reshape((4,3), order='F')
mm
y1 = mm[:,0] # y1 is the first column of mm
y1
x1 = mm[2,:] # x1 is the third row of mm
x1
z1 = mm[2,2]
z1

new1 = np.arange(9).reshape((3,3)) # create a vector and convert it into a 3 by 3 matrix
new1
hello1 = z1 + new1 ## adds value of z1 which is 1 to every value in matrix 
hello1
submm = mm[0:2,1:3] ### Not sure why, but creates a 2 by 2 matrix???
submm

mm[2,2] = 3
mm[:,[0,2]]
mm[-2,:]

x1 = np.arange(3)
x2 = np.array([7,6,6])
x3 = np.array([12,19,21])




