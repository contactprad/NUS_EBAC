# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:32:28 2018

@author: kumarp
"""

import collections

def rotate_90_degree(inputMatrix):
    outputMatrix = []
    if(inputMatrix is [] or len(inputMatrix) == 1):
        return inputMatrix
    
    for layer in inputMatrix:
        outputMatrix.append([0]*len(layer))
    outputMatrix = topRow(inputMatrix, outputMatrix)
    outputMatrix = bottomRow(inputMatrix, outputMatrix)
    if(len(inputMatrix) > 2):
        outputMatrix = middleRows(inputMatrix, outputMatrix)
    return outputMatrix

def topRow(inputMatrix, outputMatrix):
    for i in range(len(inputMatrix[0])):
        lastIndex = len(outputMatrix) - 1
        outputMatrix[i][lastIndex] = inputMatrix[0][i]
    return outputMatrix


def bottomRow(inputMatrix, outputMatrix):
    size = len(inputMatrix)
    for i in range(size):
        print("input matrix", inputMatrix[size-1][i])
        outputMatrix[i][0] = inputMatrix[size-1][i]
    return outputMatrix

def middleRows(inputMatrix, outputMatrix):
    first = 1
    last  = len(inputMatrix)
    for i in range(first, last-1):
        for j in range(last):
            outputMatrix[j][i] = inputMatrix[i][j] 
    return outputMatrix

import unittest
class test_matrix_rotation(unittest.TestCase):
    def setUp(self):
           pass
    def test_1by1(self):
        inputMatrix = [1]
        expectedMatrix = [1]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    def test_2by2(self):
        inputMatrix = [[1,2], [3,4]]
        expectedMatrix = [[3,1],[4,2]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
        
    def test_3by3(self):
        inputMatrix = [[1,2,3], [4,5,6],[7,8,9]]
        expectedMatrix = [[7,4,1],[8,5,2],[9,6,3]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    def test_4by4(self):
        inputMatrix = [[1,2,3,4], [5,6,7,8],[9,10,11,12],[13,14,15,16]]
        expectedMatrix = [[13,9,5,1],[14,6,7,2],[15,10,11,3],[8,12,16,4]]
        outputMatrix = rotate_90_degree(inputMatrix)
        self.assertListEqual(outputMatrix, expectedMatrix, "The exepected Matrix are not same");
    
    def tearDown(self):
           pass
        
if __name__ == "__main__":
           unittest.main();