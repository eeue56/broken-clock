module Alarms where

import Time

type alias Alarm = {
    time: Time.Time,
    enabled: Bool, 
    goingOff: Bool
}

type AlarmPart = Hour Int | Minute Int | Second Int

alarmsGoingOff : List Alarm -> List Alarm
alarmsGoingOff = List.filter (\x -> x.goingOff)

setOffAlarm : Time.Time -> Alarm -> Alarm
setOffAlarm time alarm = 
    if time > alarm.time && alarm.enabled 
        then {alarm | goingOff <- True} 
        else alarm