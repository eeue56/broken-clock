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
