import System.IO
import Control.Concurrent
import Expmod

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin NoBuffering
          putStr "Base: "
          a <- read <$> getLine
          putStr "Number of digits: "
          m <- read <$> getLine
          let a' = show a
          putStr $ a'++"^"++a'++"^"++a'++"^"++a'++"^"++a'++"... = ..."
          let t = show $ a `towermod` (10^m)
          let l = max 0 $ m-(length t)
          putStrLn $ (take l $ cycle "0") ++ t
          putChar '\n'
          main
