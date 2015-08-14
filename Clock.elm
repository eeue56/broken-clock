module Clock where
import Alarms exposing (..)

import Time

type ClockType = Hourly | Quarterly

type alias Clock = {
  time : Time.Time,
  clockType: ClockType, 
  alarms: List Alarm
}

toClockType x = if
  | x == "Hourly" -> Hourly
  | otherwise -> Quarterly
