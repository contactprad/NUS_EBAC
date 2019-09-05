# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 14:23:24 2018

@author: kumarp
"""

def checkUniqueString(input):
    char_dict = {}
    for ch in input:
        print(ord(ch))
        if ord(ch) in char_dict:
            return False
        char_dict[ord(ch)] = True
    return True    
        
print(checkUniqueString('abcdefa'))