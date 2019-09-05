import collections

def compress_string(input):
    final_str = ''
    collections.Counter(input).items()
    for item in collections.Counter(input).items():
        final_str = final_str + item[0] + str(item[1])
    if(len(input) <= len(final_str)):
        return input
    return final_str



import unittest
class test_compress_string(unittest.TestCase):
    def setUp(self):
           pass
    def test_differentsizesmorethan1(self):
        self.assertEqual(compress_string("pale"), "pale")
    
    def test_compression_acceptance(self):
        self.assertEqual(compress_string("pppp"), "p4")
    
    def test_longString(self):
        self.assertEqual(compress_string("pppppppp23333333333fffffffff"), "p821310f9")
    
    def test_longStringFalse(self):
        self.assertNotEqual(compress_string("pppp"),"p3")
    
    def tearDown(self):
           pass
        
if __name__ == "__main__":
           unittest.main();