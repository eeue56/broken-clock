module Quarterly where

type QuarterlyTime = BeforeSunrise | AfterSunrise | Night | Evening | AfterMidday | Err

toString : QuarterlyTime -> String
toString q = case q of
  BeforeSunrise -> "Before sunrise"
  AfterSunrise -> "After sunrise"
  Night -> "Night"
  Evening -> "Evening"
  AfterMidday -> "After midday"
  Err -> "Err.."
  
toColor : QuarterlyTime -> String
toColor q = case q of
  BeforeSunrise -> "lightBlue"
  AfterSunrise -> "lightYellow"
  Night -> "rgb(17, 32, 100)"
  Evening ->  "rgb(174, 11, 11)"
  AfterMidday -> "lightOrange"
  Err -> "green"

toQuarterly hour = if
  | hour < 6 -> BeforeSunrise
  | hour < 13 -> AfterSunrise
  | hour > 21 -> Night
  | hour > 16 -> Evening
  | hour > 12 -> AfterMidday
  | otherwise -> Err
