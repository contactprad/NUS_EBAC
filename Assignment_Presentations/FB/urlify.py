# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 14:27:03 2018

@author: kumarp
"""

def urlify(input):
    return input.rstrip().replace(" ", "%20")

print(urlify("test abc  "))