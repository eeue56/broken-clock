module Clock where

import Time

type ClockType = Hourly | Quarterly

type alias Clock = {
  time : Time.Time,
  clockType: ClockType
}

toClockType x = if
  | x == "Hourly" -> Hourly
  | otherwise -> Quarterly
