module Main where

import System.Process
import System.IO
import Data.Time
import Data.List.Split
import Data.Map
import Data.Bits
import Text.Regex.TDFA

-- Gonna have to save this for last call
-- Have a list of 3 tuple of Strings: (name, fullText, Maybe color)
-- I love imperative programming
jsonOutput :: String -> String -> Maybe String -> IO String
jsonOutput name fullText color =
        return ("{\"name\": \"" <> name <> "\", \"full_text\": \"" <> fullText <> "\"" <> haveColor color <> "}")
        where
          haveColor :: Maybe String -> String
          haveColor Nothing = ""
          haveColor (Just val) = ", \"color\": \"" <> val <> "\""

-- https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
statusTime :: IO String
statusTime = do
  currentTime <- fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (getCurrentTime >>= utcToLocalZonedTime)
  jsonOutput "time" currentTime Nothing

-- This is written to fit my specific setup
batteryPath :: String
batteryPath = "/sys/class/power_supply/BAT0"

statusBattery :: IO String
statusBattery = do
  hChargeNow <- openFile (batteryPath <> "/charge_now") ReadMode
  hVoltageNow <- openFile (batteryPath <> "/voltage_now") ReadMode
  hChargeFull <- openFile (batteryPath <> "/charge_full") ReadMode
  chargeNow <- hGetLine hChargeNow
  voltageNow <- hGetLine hVoltageNow
  chargeFull <- hGetLine hChargeFull 
  hClose hChargeNow
  hClose hVoltageNow
  hClose hChargeFull
  let fullCharge = accumCalc voltageNow chargeFull
  let percentage = 100 * (accumCalc voltageNow chargeNow + fullCharge) `div` fullCharge -- Double calculate full charge here :(
  jsonOutput "bat" ("Bat: " <> show percentage <> "%") (batteryColor percentage)
  where
    accumCalc :: String -> String -> Int
    accumCalc voltageNow chargeStatus = ((read voltageNow :: Int) `div` 1000) * ((read chargeStatus :: Int) `div` 1000)
    batteryColor :: Int -> Maybe String
    batteryColor percentage
      | percentage > 50 = Nothing -- White
      | percentage > 25 = Just "#ff0000" -- Yellow
      | otherwise = Just "ffff00" -- Red

statusLoad :: IO String
statusLoad = do
  hLoadAvg <- openFile "/proc/loadavg" ReadMode
  loadAvg <- hGetLine hLoadAvg
  hClose hLoadAvg
  jsonOutput "load" ("Load: " <> head (words loadAvg)) Nothing

statusQubes :: IO String
statusQubes = do
  (_, Just hout, _, _) <- createProcess (proc "qvm-ls" ["--running"]){ std_out = CreatePipe }
  myLines <- hGetContents' hout
  hClose hout
  jsonOutput "qubes" (show (length (splitOn "\n" myLines) -3)) Nothing -- -3 for Name, dom0, and empty string in splitOn

combineElements :: [a] -> [(a, a)]
combineElements (x0:x1:xs) =
  (x0, x1) : combineElements xs
combineElements [_] = []
combineElements [] = []

-- My computer uses vm-pool
statusDisk :: IO String
statusDisk = do
  (_, Just hout, _, _) <- createProcess (proc "qvm-pool" ["-i", "vm-pool"]){ std_out = CreatePipe }
  myLines <- hGetContents' hout
  let poolMap = Data.Map.fromList . combineElements $ words myLines  
  let diskSize = poolMap ! "size"
  let usage = poolMap ! "usage"
  jsonOutput "disk" ("Disk free: " <> diskOutSize ((read diskSize :: Int) - (read usage :: Int))) Nothing
  where
    diskOutSize :: Int -> String
    diskOutSize free
      | free >= 1 `shift` 40 = show (free `shift` (-40)) <> "T"
      | free >= 1 `shift` 30 = show (free `shift` (-30)) <> "G"
      | free >= 1 `shift` 20 = show (free `shift` (-20)) <> "M"
      | free >= 1 `shift` 10 = show (free `shift` (-10)) <> "K"
      | otherwise = show free <> "Bytes"

-- statusVolume :: IO String
statusVolume = do
  (_, Just hout, _, _) <- createProcess (proc "amixer" ["sget", "Master"]){ std_out = CreatePipe } 
  volumeLines <- hGetContents' hout
  let volumeMatches = getAllTextMatches (volumeLines =~ "Playback \\d+ \\[\\d+%\\] \\[(on|off)\\]") :: [String]
  return volumeLines
  -- undefined


main :: IO ()
main = do
  print "{\"version\": 1}"
  -- hflush stdout
  -- print a [ for starting the endless array
  print "["
  -- send another [] this time to simplify the loop??
  print "[]"
