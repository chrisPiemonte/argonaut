import unittest
from .from_reddit import *


class TestFromReddit(unittest.TestCase):

    def setUp(self):
        self.prova = 'prova'

    def tearDown(self):
        self.prova = ''

    def test_upper(self):
        self.assertEqual('foo'.upper(), 'FOO')

    def test_isupper(self):
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_split(self):
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)

    def suite():
        suite = unittest.TestSuite()
        suite.addTest(TestFromReddit('test_upper'))
        suite.addTest(TestFromReddit('test_isupper'))
        return suite

if __name__ == '__main__':
    # unittest.main()
    # SUITE
    # runner = unittest.TextTestRunner()
    # runner.run(suite())
    print('ciao')
    pass
