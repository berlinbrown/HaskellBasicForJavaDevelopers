{- arch-tag: main file
Copyright (C) 2010-2011 Berlin Brown

Also see :
 https://github.com/berlinbrown
 http://code.google.com/u/berlin.brown/
 
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : 
   Copyright  : Copyright (C) 2010-2011 Berlin Brown
   License    : GNU GPL, version 2 or above

   Maintainer : Berlin Brown 
   Stability  : provisional
   Portability: portable

This module provides simple euler examples

-}

--
-- Euler Problems in experimental haskell
-- Problem 19+, How many Sundays fell on the first of the month during 
-- the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-- Doing it wrong version

module EulerProblems where

import Control.Monad
import Data.List

monthsInYr = 52
timeFrame = 2000 - 1901

printData :: Int -> IO (Int)
printData x = do
        putStrLn("Data x=" ++ (show x))
        return (x)
        
printData2 :: (Int, Int, Int) -> IO ()
printData2 x = do
        putStrLn("At end, time=" ++ (show x))
        return ()

addDaysIO :: Int -> IO (Int)
addDaysIO 365 = do
        printData 365
        return 365
addDaysIO x = do
        printData x
        addDaysIO(x + 1)

weekday :: Int -> Int
weekday wd
        | wd >= 7    = 1
        | otherwise = wd + 1

checksunday :: (Int, Int, Int, Int) -> IO ()
checksunday (1, mnth, yr, 7) = do 
        putStrLn ">>>>>>> Sunday on First of Month[f36] !! "
checksunday (_, _, _, _) = return ()        

-- Imperative, doing it wrong version
addDaysMonthIO :: (Int, Int, Int, Int) -> IO ()
addDaysMonthIO (31, 12, 2000, _)  = putStrLn "Done with program on 12/31/2000"    
addDaysMonthIO (31, 12, yr, wd)   = addDaysMonthIO(1, 1,  yr+1, weekday(wd))      
addDaysMonthIO (30, 11, yr, wd)   = addDaysMonthIO(1, 12, yr, weekday(wd))
addDaysMonthIO (30, 9, yr, wd)    = addDaysMonthIO(1, 10, yr, weekday(wd))
addDaysMonthIO (30, 4, yr, wd)    = addDaysMonthIO(1, 5,  yr, weekday(wd))
addDaysMonthIO (30, 6, yr, wd)    = addDaysMonthIO(1, 7,  yr, weekday(wd))        
addDaysMonthIO (31, mnth, yr, wd) = addDaysMonthIO(1, mnth+1, yr, wd+1) 
addDaysMonthIO (day, mnth, yr, wd) 
        | (mod yr 100) == 0 && (mod yr 400) == 0 && mnth == 2 && day == 29 = do
                --putStrLn("A leap year[a] = " ++ (show yr)) 
                addDaysMonthIO(1, 3, yr, weekday(wd))        
        | (mod yr 100) == 0 && (mod yr 400) /= 0 && mnth == 2 && day == 28 = do                
                addDaysMonthIO(1, 3, yr, weekday(wd))
        | (mod yr 4) == 0 && mnth == 2 && day == 29 = do
                --putStrLn("A leap year[b] = " ++ (show yr)) 
                addDaysMonthIO(1, 3, yr, weekday(wd))
        | (mod yr 4) /= 0 && mnth == 2 && day == 28 = do                
                addDaysMonthIO(1, 3, yr, weekday(wd))
        | otherwise = do
                --printData2 (day, mnth, yr)
                --putStrLn("WeekDay : " ++ (show wd))
                checksunday(day, mnth, yr, wd)
                addDaysMonthIO(day + 1, mnth, yr, weekday(wd))

-- Problem 19
-- Solution from haskell resources                
problem_19 = length . filter (== sunday) . drop 12 . take 1212 $ since1900
since1900 = scanl nextMonth monday . concat $
              replicate 4 nonLeap ++ cycle (leap : replicate 3 nonLeap) 
nonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
leap = 31 : 29 : drop 2 nonLeap 
nextMonth x y = (x + y) `mod` 7
sunday = 0
monday = 1
    
-- Problem 21, explicit version
-- amicable numbers, sum
divisors :: Int -> [Int]
divisors n = filter (\x -> (mod n x) == 0) [1..(n-1)]
d :: Int -> Int 
d n = sum (divisors n)

amicable :: Int -> [Int] -> [Int]
amicable 10000 lst = nub(lst)
amicable n lst = 
        let a = d n
            b = d a            
        in if b == n && a /= b then a : b : (amicable (n+1) lst) else (amicable (n+1) lst)
problem_21 = sum (nub(amicable 2 []))

amicableIO :: Int -> IO ()
amicableIO 999 = return ()
amicableIO n = do        
        let a = d n
        let b = d a
        let z = b == n
        when z $ putStrLn("At n -> " ++ (show n) ++ " aRes=" ++ (show a) ++ " opposite, bRes=" ++ (show b) ++ " ami=" ++ (show z))               
        amicableIO (n + 1)    
                                          
-- Main Entry Point
-- Uncomment lines to run functions, debugging code 
main :: IO ()
main = do                
        putStrLn "Running"
        --putStrLn(show (monthsInYr * timeFrame))                               
        --addDaysMonthIO (1, 1, 1900, 1)  
        --putStrLn(show problem_19)    
        --amicableIO 2
        --putStrLn(show(nub(amicable 2 [])))
        --putStrLn(show problem_21)
        -- fib 6                
        putStrLn "Done"
        return ()
                
-- End of File        
