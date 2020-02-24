module Backend exposing (app, init)

import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Dict
import Set exposing (Set)
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Time.every 5000 BTick
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { counter = 0, clients = Set.empty, lastSeen = Dict.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        BTick now ->
            let
                connected: Dict.Dict ClientId Time.Posix
                connected = 
                    Dict.filter (\k v -> (Time.posixToMillis now)-(Time.posixToMillis v) <= 10000) model.lastSeen
                newClients = Set.intersect model.clients <| Set.fromList <| Dict.keys connected
                sendUserList = Dict.size connected /= Set.size model.clients
            in
            ( {model | clients = newClients, lastSeen = connected}
            , Cmd.batch
                [ if sendUserList then 
                    broadcast newClients (UserListNewValue newClients)
                  else
                    Cmd.none
                ] )
        SetLastClientSeen clientId posix ->
            ( { model | lastSeen = Dict.insert clientId posix model.lastSeen }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientJoin ->
            let
                newClients = Set.insert clientId model.clients
            in
                ( { model | clients = newClients }
                , Cmd.batch
                    [ sendToFrontend clientId (CounterNewValue model.counter)
                    , broadcast newClients (UserListNewValue newClients)
                    , Task.perform (SetLastClientSeen clientId) Time.now 
                    ]
                )

        CounterIncremented ->
            let
                newCounterValue =
                    model.counter + 1
            in
            ( { model | counter = newCounterValue }, broadcast model.clients (CounterNewValue newCounterValue) )

        CounterDecremented ->
            let
                newCounterValue =
                    model.counter - 1
            in
            ( { model | counter = newCounterValue }, broadcast model.clients (CounterNewValue newCounterValue) )
        
        Ping ->
            ( model, Task.perform (SetLastClientSeen clientId) Time.now )


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch
