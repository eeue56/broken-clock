module BrokenClock where
import Time exposing (every, second)
import Signal exposing ((<~))
import Random exposing (Seed, int, generate, initialSeed) 

import Quarterly exposing (..)
import Views exposing (..)
import Clock exposing (..)
import Updates exposing (..)
import Alarms exposing (Alarm, setOffAlarm, AlarmPart(..))

actions : Signal.Mailbox Update
actions = Signal.mailbox Updates.Nothing

model : Clock
model = { time = Time.second, 
  clockType = Quarterly,
  alarms = [],
  parts = {hour=0, minute=0, second=0},
  seed = initialSeed 5 }


clockSignal = Signal.map (TimeUpdate) <| every second

updateAlarms : Clock -> Clock
updateAlarms clock = { clock | alarms <- List.map (setOffAlarm clock.time) clock.alarms }

updateSeed : Clock -> Clock
updateSeed clock = 
  let
    (_, newSeed) = generate (int 0 1) clock.seed
  in
    { clock | seed <- newSeed}

updateAlarmParts : AlarmPart -> Clock -> Clock 
updateAlarmParts part clock = 
  let 
    oldParts = clock.parts
  in
  case part of
    Hour x -> { clock | parts <- { oldParts | hour <- x } }
    Minute x -> { clock | parts <- { oldParts | minute <- x } }
    Second x -> { clock | parts <- { oldParts | second <- x } }

update: Update -> Clock -> Clock
update update clock = case update of
  TimeUpdate newTime -> updateAlarms { clock | time <- newTime }
  TypeUpdate newType -> { clock | clockType <- newType }
  NewAlarm alarm -> { clock | alarms <- (alarm :: clock.alarms) }
  AlarmTime x -> updateAlarmParts x clock
  Updates.Nothing -> clock

startModel : Signal Clock
startModel = Signal.foldp 
  update 
  model 
  <|
    Signal.merge 
      actions.signal
      clockSignal

main = Signal.map (view actions.address) startModel