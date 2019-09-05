# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 13:53:18 2018

@author: kumarp
"""

def checkStringUnique(input):
    start = 0;
    for i in input:
        counter = 0
        for j in input:
            print(i, j)
            if(i == j):
                counter = counter + 1;
            if(counter >1):
                return False
    return True
            

print(checkStringUnique('abcdefgha'))