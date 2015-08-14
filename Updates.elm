module Updates where

import Time
import Clock exposing (ClockType)

type Update = TimeUpdate (Time.Time) | TypeUpdate (ClockType) | Nothing
