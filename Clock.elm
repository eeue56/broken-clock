module Clock where
import Time
import Random exposing (Seed, int, generate) 

import Alarms exposing (..)


type ClockType = Hourly | Quarterly

type alias RoughTime = {
    hour: Int,
    minute: Int,
    second: Int
}

type alias Clock = {
  time : Time.Time,
  clockType: ClockType, 
  alarms: List Alarm,
  parts: RoughTime
}

newRandomRoughTime : Seed -> (RoughTime, Seed)
newRandomRoughTime seed = 
  let
    (hour, seed') = generate (int 0 23) seed
    (minute, seed'') = generate (int 0 59) seed'
    (second, seed''') = generate (int 0 59) seed''
    seed'''' = seed'''
  in
    ({ hour=hour, minute=minute, second=second }, seed'''')

toClockType x = if
  | x == "Hourly" -> Hourly
  | otherwise -> Quarterly

addRoughTime : Time.Time -> RoughTime -> Time.Time
addRoughTime time parts = 
  let
    newSeconds = ((toFloat <| parts.second) * Time.second)
    newMinutes = ((toFloat <| parts.minute) * Time.minute)
    newHours = ((toFloat <| parts.hour) * Time.hour)
  in
    time + newSeconds + newMinutes + newHours

newAlarmTime : Clock -> Time.Time
newAlarmTime model = addRoughTime model.time model.parts