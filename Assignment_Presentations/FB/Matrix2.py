# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 14:52:09 2018

@author: kumarp
"""

# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:32:28 2018

@author: kumarp
"""

import collections

def rotate_90_degree(inputMatrix):
    size = len(inputMatrix)
    print("size is", int(size/2))
    layer = 0
    while(layer < 1):
        first = 0
        last = size - 1
        print("first", first,"last:", last)
        for i in range(last):
            offset  = i - first
            
            top = inputMatrix[first][i]
            
            inputMatrix[first][i] = inputMatrix[last-offset][first]
            
            inputMatrix[last-offset][first] = inputMatrix[last][last-offset]
            
            inputMatrix[last][last-offset] = inputMatrix[i][last]
            
            inputMatrix[i][last] = top
        layer = layer + 1;    
        print("iteration", layer, inputMatrix)
    return inputMatrix
    
    
import unittest
class test_matrix_rotation(unittest.TestCase):
    def setUp(self):
           pass
        
    #@unittest.skip("skipping")
    def test_1by1(self):
        inputMatrix = [1]
        expectedMatrix = [1]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    #@unittest.skip("skipping")
    def test_2by2(self):
        inputMatrix = [[1,2], [3,4]]
        expectedMatrix = [[3,1],[4,2]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
        
    #@unittest.skip("skipping")
    def test_3by3(self):
        inputMatrix = [[1,2,3], [4,5,6],[7,8,9]]
        expectedMatrix = [[7,4,1],[8,5,2],[9,6,3]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    #@unittest.skip("skipping")
    def test_4by4(self):
        inputMatrix = [[1,2,3,4], [5,6,7,8],[9,10,11,12],[13,14,15,16]]
        expectedMatrix = [[13,9,5,1],[14,6,7,2],[15,10,11,3],[16,12,8,4]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
        #@unittest.skip("skipping")
    def test_5by5(self):
        inputMatrix = [[1,2,3,4,5], [6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20], [21,22,23,24,25]]
        expectedMatrix = [[21,16,11,6,1], [22,7,8,9,2],[23,12,13,14,3],[24,17,18,19,4], [25,20,15,10,5]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    def tearDown(self):
           pass
        
if __name__ == "__main__":
           unittest.main();