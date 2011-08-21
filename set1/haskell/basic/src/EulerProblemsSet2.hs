{- arch-tag: main file
Copyright (C) 2010-2011 Berlin Brown

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

module EulerProblems2 where

import Control.Monad
import Data.List
import System.Cmd
import System.Directory
import System.IO
import Data.Char

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)
-- End of split                                                 

myfoldl f z []     = z                    -- if the list is empty, the result is the initial value
myfoldl f z (x:xs) = myfoldl f (f z x) xs -- if not, we recurse immediately, making the new initial value the result
                                          -- of combining the old initial value with the first element.

-- (a -> b -> b) -> b -> [a] -> b
myfoldr f z []     = z
myfoldr f z (x:xs) = x `f` myfoldr f z xs

-- Use of map: (a -> b) -> [a] -> [b]
scoreString :: [Char] -> Int -> Int
scoreString s idx = idx * (60 + sum(map (\x  -> ord(x) - 64) s))

-- Example: sumStrings d = return (myfoldr (\x y -> 0) 0 d) **

sumStrings :: [String] -> Int -> Int -> IO (Int)
sumStrings [] idx total = 
        do  putStrLn (show total)
            return total
sumStrings (x:xs) idx total =
        do let score = (scoreString x idx)
           putStrLn(show score ++ " -> " ++ x ++ " -> " ++ (show idx))               
           sumStrings xs (idx + 1) (total + score)
                
eulerProblem :: [String] -> IO ()
eulerProblem d = 
        do let s = sort(d)
               y = show(s)
           tot <- sumStrings s 1 0           
           return ()
                           
{- | Process loop.
-}
processLoop :: Handle -> IO ()
processLoop inh = 
        do ineof <- hIsEOF inh
           if ineof
                then return ()
                else do str <- hGetLine inh
                        let d = split "," str
                        eulerProblem(d)
                        processLoop inh
-- End of process loop                        
                  
fac 0 = 1
fac n = n * fac (n - 1)

perms [] _ = []
perms xs n = x : perms (delete x xs) (mod n m)
  where m = fac $ length xs - 1
        y = div n m
        x = xs !! y 
problem_24 = perms "0123456789" 999999                       
                        
fib :: Int -> IO (Int)
fib 0 = return 0
fib 1 = return 1
fib 2 = return 1
fib n = do        
        a <- fib $ n - 1
        b <- fib $ n - 2
        return(a + b)
        
fib2 :: Int -> Int
fib2 0 = 0
fib2 1 = 1
fib2 2 = 1
fib2 n = (fib2 $ n - 1) + (fib2 $ n - 2)

-- fib n = phi ^ n - (-1) ^ n / phi ^ n // sqrt 5
phi = 1.61803398874989484820458683436563811772030917980576
fib4 :: Double -> Double                
fib4 n = ((phi ** n) - (-1 ** n)/(phi ** n)) / sqrt 5  

fibs6 = 0:1:(zipWith (+) fibs6 (tail fibs6))
t = 10^999 
problem_25 = length w where w = takeWhile (< t) fibs6
              
--
-- Main entry point
main :: IO ()
main = do
        z <- getCurrentDirectory
        putStrLn "Running Application"
        putStrLn("cwd=" ++ show(z))        
        --inh <- openFile "names.txt" ReadMode
        --processLoop inh
        --putStrLn(show problem_24)
        --let x = fib2 34
        let y = ceiling $ fib4 100
            z = show y
            l = length z
        --putStrLn $ "Fib=" ++ (show l)
        let n = (show problem_25)
        putStrLn $ "Fib=" ++ (show n)         
        putStrLn "Done."
        return ()        
-- End of Function