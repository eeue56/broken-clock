module Alarms where

import Time

type alias Alarm = {
    time: Time.Time,
    enabled: Bool
}