module GenericViews where

import Html exposing (..)

numberSelectView : List Attribute -> Int -> Int -> Html.Html
numberSelectView props min max = select props <| List.map (\x -> option [] [Basics.toString x |> text]) [min..max]
