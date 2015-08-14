module Updates where

import Time
import Clock exposing (ClockType)
import Alarms exposing (Alarm)

type Update = 
    TimeUpdate (Time.Time) | 
    TypeUpdate (ClockType) | 
    NewAlarm (Alarm)       |
    Nothing
