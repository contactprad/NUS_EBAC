# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 17:23:28 2018

@author: kumarp
"""

import unittest 

def oneEditAway(input1, input2):
    if abs(len(input1) - len(input2)) > 1:
           return False
    elif abs(len(input1)-len(input2) == 0):
        return checkReplace(input1, input2);
    elif abs(len(input1)-len(input2) == 1):
        return checkInsertDelete(input1, input2)
        
def checkReplace(input1, input2):
    count = 0;
    for i in range(len(input1)):
        if(input1[i]!= input2[i]):
            count = count + 1
            print(input1[i], input2[i])
            print(count)
            if(count > 1):
                return False
    return True
        
def checkDelete(input1, input2):
    i = 0; j=0
    while(i<len(input1) and j<len(input2)):
        if (input1[i] != input2[j]):
            if(i != j):
                return False
            i = i+1
        else:
            i = i+1
            j = j+1
            
    return True
    
class test_oneEditAway(unittest.TestCase):
    def setUp(self):
           pass
    def test_differentsizesmorethan1(self):
        self.assertFalse(oneEditAway("pale", "pa"))
    
    def test_oneEditAway(self):
        self.assertTrue(oneEditAway("pale", "bale"))
    
    def test_twoEditAway(self):
        self.assertFalse(oneEditAway("pale", "bble"))
    
    def test_oneInsert(self):
        self.assertTrue(oneEditAway("pale", "ple"))
    
    def tearDown(self):
           pass
        
if __name__ == "__main__":
           unittest.main();