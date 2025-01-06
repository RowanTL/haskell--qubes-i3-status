module Main where

import System.Process
import System.IO
-- import GHC.IO.Handle
import Data.Time

-- Could probably program this better
jsonFst :: (String, IO String, Maybe String) -> String
jsonFst (val, _, _) = val

jsonSnd :: (String, IO String, Maybe String) -> IO String
jsonSnd (_, val, _) = val

jsonTrd :: (String, IO String, Maybe String) -> String
jsonTrd (_, _, Just val) = ", \"color\": \"" <> val <> "\"" 
-- jsonTrd (_, _, Just val) = val
jsonTrd (_, _, Nothing) = ""

-- Gonna have to save this for last call
-- Have a list of 3 tuple of Strings: (name, fullText, Maybe color)
-- I love imperative programming
jsonOutput :: [(String, IO String, Maybe String)] -> IO String
jsonOutput xs = do
    combinedString <- jsonOutput' xs
    return (",[" <> concat combinedString)
    where
      jsonOutput' :: [(String, IO String, Maybe String)] -> IO [String]
      jsonOutput' [] = return []  -- This should never happen
      jsonOutput' [y] = do 
        secondSlot <- jsonSnd y
        return ["{\"name\": \"" <> jsonFst y <> "\", \"full_text\": \"" <> secondSlot <> "\"" <> jsonTrd y <> "}]"]
      jsonOutput' (y:ys) = do
        secondSlot <- jsonSnd y
        fmap ((:) ("{\"name\": \"" <> jsonFst y <> "\", \"full_text\": \"" <> secondSlot <> "\"" <> jsonTrd y <> "}, ")) (jsonOutput' ys)

-- https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
statusTime :: (String, IO String, Maybe String)
statusTime = 
  (
    "time"
  , fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (getCurrentTime >>= utcToLocalZonedTime)
  , Nothing
  )

-- This is written to fit my specific setup
batteryPath = "/sys/class/power_supply/BAT0"

statusBattery :: (String, IO String, Maybe String)
statusBattery =
  (
    "bat"
  , batteryPercentage
  , Nothing
  )
  where
    accumCalc :: String -> String -> Int
    accumCalc voltageNow chargeStatus = ((read voltageNow :: Int) `div` 1000) * ((read chargeStatus :: Int) `div` 1000)
    batteryPercentage :: IO String
    batteryPercentage = do
      hChargeNow <- openFile (batteryPath <> "/charge_now") ReadMode
      hVoltageNow <- openFile (batteryPath <> "/voltage_now") ReadMode
      hChargeFull <- openFile (batteryPath <> "/charge_full") ReadMode
      chargeNow <- hGetLine hChargeNow
      voltageNow <- hGetLine hVoltageNow
      chargeFull <- hGetLine hChargeFull 
      return (show (100 * (accumCalc voltageNow chargeNow + accumCalc voltageNow chargeFull) `div` accumCalc voltageNow chargeFull))



main :: IO ()
main = do
  print "{\"version\": 1}"
  -- hflush stdout

  -- print a [ for starting the endless array
  print "["
  
  -- send another [] this time to simplify the loop??
  print "[]"
