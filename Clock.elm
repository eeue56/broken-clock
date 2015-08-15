module Clock where
import Time
import Random exposing (Seed, int, generate) 

import Alarms exposing (..)


type ClockType = Hourly | Quarterly

type alias RoughTime = {
    hour : Int,
    minute : Int,
    second : Int
}

type alias Clock = {
  time : Time.Time,
  clockType : ClockType, 
  alarms : List Alarm,
  parts : RoughTime,
  seed : Seed
}

newRandomRoughTime : Seed -> RoughTime
newRandomRoughTime seed = 
  let
    (hour, _) = generate (int 0 23) seed
    (minute, _) = generate (int 0 59) seed
    (second, _) = generate (int 0 59) seed
    
  in
    { hour=hour, minute=minute, second=second }

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