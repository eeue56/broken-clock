module Updates where

import Time
import Clock exposing (ClockType)
import Alarms exposing (Alarm, AlarmPart)

type Update = 
    TimeUpdate (Time.Time) | 
    TypeUpdate (ClockType) | 
    NewAlarm (Alarm)       |
    AlarmTime (AlarmPart) |
    Nothing
