module Clock where
import Alarms exposing (..)

import Time

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

toClockType x = if
  | x == "Hourly" -> Hourly
  | otherwise -> Quarterly

newAlarmTime : Clock -> Time.Time
newAlarmTime model = 
  let
    parts = model.parts
    newSeconds = ((toFloat <| parts.second) * second)
    newMinutes = ((toFloat <| parts.minute) * minute)
    newHours = ((toFloat <| parts.hour) * hour)
  in
    model.time + newHours + newMinutes + newMinutes