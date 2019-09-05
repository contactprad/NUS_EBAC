# -*- coding: utf-8 -*-
"""
Created on Sat Jul 28 10:56:40 2018

@author: kumarp
"""

def permutation(main_str, sub_str):
    index = 0
    length = len(sub_str)
    counter = 0
    for i in main_str:
        if(length<len(main_str)):
            curr_str = main_str[index:length]
            print(curr_str)
            if curr_str == sub_str:
                counter = counter + 1
            index = index + 1;
            length = length + 1;
    print(counter)
    
        
permutation('testertestertestertestrestetester', 'test')
