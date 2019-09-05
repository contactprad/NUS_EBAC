# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 15:23:31 2018

@author: kumarp
"""

import collections

def ifPermutationPalindrome(input):
    odd = False
    letters = collections.Counter(input)
    listOfFrequency =  list(letters.items())
    for item in listOfFrequency:
        if(item[1]==1 or item[1]%2):
            if(odd):
                return False
            odd = True;
    return True
    
print(ifPermutationPalindrome("aabbccd"))
        