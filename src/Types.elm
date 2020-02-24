module Types exposing (..)

import Lamdera exposing (ClientId)
import Dict exposing (Dict)
import Set exposing (Set)
import Time
import PixelEngine exposing (Area)


type alias BackendModel =
    { counter : Int
    , clients : Set ClientId
    , lastSeen : Dict ClientId Time.Posix
    }


type alias FrontendModel =
    { counter : Int
    , clientId : String
    , connectedClients : Set ClientId
    , gameArea : Area FrontendMsg
    }


type FrontendMsg
    = Increment
    | Decrement
    | FTick Time.Posix
    | FNoop


type ToBackend
    = ClientJoin
    | CounterIncremented
    | CounterDecremented
    | Ping


type BackendMsg
    = BTick Time.Posix
    | SetLastClientSeen ClientId Time.Posix


type ToFrontend
    = CounterNewValue Int
    | UserListNewValue (Set String)
