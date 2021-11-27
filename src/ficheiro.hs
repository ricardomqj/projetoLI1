module Teste where 

import Test.HUnit
import Distribution.Types.TestSuiteInterface (TestSuiteInterface(TestSuiteExeV10))

mysum::Int->Int->Int
mysum m n = m + n 

test1Plus2tq3=TestCase (assertEqual "1+2=3" 
                        3
                        (mysum 1 2))
