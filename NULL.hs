--Interpreter for NULL, a programming language I invented when I was 12.

module Main where

import Expmod
import System.IO
import Control.Concurrent

data Opcode = Next | Prev | Output | Input | Sub | Add | AddY | RotR | RotL | Disc | Enq | Drop | Swap | Halt deriving (Show, Eq, Enum)

data Environment = Env {x :: Integer, y :: Integer, q0 :: [Char], q1 :: [Char], q2 :: [Char]}

head' :: [Char] -> Char
head' [] = '\0'
head' (c:_) = c

tail' :: [Char] -> [Char]
tail' [] = []
tail' (_:s) = s

replace :: Char -> [Char] -> [Char]
replace c [] = [c]
replace c (d:ds) = c:ds

smallestFactor :: Integer -> Integer
smallestFactor 1 = 1
smallestFactor n = (fst.head.factor) n

execOpcode :: Opcode -> Environment -> (IO Environment)
execOpcode Next e = return $ Env {x = x e, y = y e, q0 = q1 e, q1 = q2 e, q2 = q0 e}
execOpcode Prev e = return $ Env {x = x e, y = y e, q0 = q2 e, q1 = q0 e, q2 = q1 e}
execOpcode Output e = (putChar $ head' $ q0 e) >> return e
execOpcode Input e = (\c -> Env {x = x e, y = y e, q0 = replace c $ q0 e, q1 = q1 e, q2 = q2 e}) <$> getChar
execOpcode Sub e = return $ Env {x = x e, y = min 0 $ (y e) - (fromIntegral $ fromEnum $ head' $ q0 e), q0 = q0 e, q1 = q1 e, q2 = q2 e}
execOpcode Add e = return $ Env {x = x e, y = (y e) + (fromIntegral $ fromEnum $ head' $ q0 e), q0 = q0 e, q1 = q1 e, q2 = q2 e}
execOpcode AddY e = return $ Env {x = x e, y = y e, q0 = replace (toEnum $ ((fromEnum $ head' $ q0 e)+(fromIntegral $ y e)) `mod` 128) $ q0 e, q1 = q1 e, q2 = q2 e}
execOpcode RotR e = return $ Env {x = x e, y = y e, q0 = tail' $ q0 e, q1 = (q1 e)++[head' $ q0 e], q2 = q2 e}
execOpcode RotL e = return $ Env {x = x e, y = y e, q0 = tail' $ q0 e, q1 = q1 e, q2 = (q2 e)++[head' $ q0 e]}
execOpcode Disc e = return $ Env {x = x e, y = y e, q0 = tail' $ q0 e, q1 = q1 e, q2 = q2 e}
execOpcode Enq e = return $ Env {x = x e, y = y e, q0 = (q0 e)++[toEnum $ fromIntegral $ (y e) `mod` 128], q1 = q1 e, q2 = q2 e}
execOpcode Drop e = if (head' $ q0 e) == '\0' then return e else let p = smallestFactor $ x e in return $ Env {x = (x e) `div` p, y = (y e)*p, q0 = q0 e, q1 = q1 e, q2 = q2 e}
execOpcode Swap e = return $ Env {x = y e, y = x e, q0 = q0 e, q1 = q1 e, q2 = q2 e}
execOpcode Halt e = return e

primeOp :: Integer -> Opcode
primeOp p = toEnum $ ((π p)-1) `mod` 14

runNULL :: Environment -> IO ()
runNULL e = do let p = smallestFactor $ x e
               let op = primeOp p
               let e' = Env {x = (x e) `div` p, y = (y e)*p, q0 = q0 e, q1 = q1 e, q2 = q2 e}
--               putStr "y = "
--               print $ y e'
               if (op == Halt)||(x e == 1) then return () else (execOpcode op e') >>= runNULL

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin NoBuffering
          putStr "\n> "
          c <- read <$> getLine
          let e = Env {x = c, y = 1, q0 = [], q1 = [], q2 = []}
          runNULL e
          main
