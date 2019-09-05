# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 19:03:21 2018

@author: kumarp
"""

# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:32:28 2018

@author: kumarp
"""

import collections

def zero_matrix(inputMatrix):
    row_with_0 = []
    zero_row_matrix = [0]*len(inputMatrix[0])
    for row in range(len(inputMatrix)):
        print("row is", inputMatrix[row])
        if(0 in inputMatrix[row]):
            inputMatrix[row] = zero_row_matrix
    print("after:", inputMatrix)
    return inputMatrix
    
    
import unittest
class test_matrix_rotation(unittest.TestCase):
    def setUp(self):
           pass
            
    #@unittest.skip("skipping")
    def test_2by2(self):
        inputMatrix = [[1,2], [0,4]]
        expectedMatrix = [[1,2],[0,0]]
        outputMatrix = zero_matrix(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
        
    #@unittest.skip("skipping")
    def test_3by3(self):
        inputMatrix = [[1,2,3], [4,0,6],[7,8,9]]
        expectedMatrix = [[1,2,3], [0,0,0],[7,8,9]]
        outputMatrix = zero_matrix(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");

    #@unittest.skip("skipping")
    def test_4by5(self):
        inputMatrix = [[1,2,3,4], [5,6,7,8],[0,9,10,11]]
        expectedMatrix = [[1,2,3,4], [5,6,7,8],[0,0,0,0]]
        outputMatrix = zero_matrix(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
        
    def tearDown(self):
           pass
        
if __name__ == "__main__":
           unittest.main();