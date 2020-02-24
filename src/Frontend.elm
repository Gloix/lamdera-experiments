module Frontend exposing (Model, app)

-- ELM UI STUFF

import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, height, padding, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Lamdera exposing (sendToBackend)
import Set
import Time
import Types exposing (..)

import PixelEngine
import PixelEngine.Tile
import Color


type alias Model =
    FrontendModel


myColors =
    { primary = rgb255 255 126 107
    , primaryLight = rgb255 255 166 158
    , primaryDark = rgb255 140 94 88
    , accent = rgb255 169 240 209
    , white = rgb255 255 255 255
    }


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "v1"
                , body = [ view model ]
                }
        , subscriptions = \m -> Time.every 5000 FTick
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


init : ( Model, Cmd FrontendMsg )
init =
    ( 
        { counter = 0
        , clientId = ""
        , connectedClients = Set.empty
        , gameArea = PixelEngine.tiledArea
            { rows = 10
            , tileset = PixelEngine.Tile.tileset
                { source = "http://localhost:8000/dungeon_sheet.png"
                , spriteWidth = 16
                , spriteHeight = 16
                }
            , background = PixelEngine.colorBackground (Color.Color.rgb255 0 0 0)
            }
            []
        }, 
        sendToBackend ClientJoin
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, sendToBackend CounterDecremented )

        FTick now ->
            ( model, sendToBackend Ping )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue ->
            ( { model | counter = newValue }, Cmd.none )

        UserListNewValue userList ->
            ( { model | connectedClients = userList }, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    Element.layout [ Background.color myColors.primaryLight ]
        (Element.row [ spacing 7, width fill, padding 50 ]
            [ mainAppContent model
            , usersList model
            ]
        )


mainAppContent : Model -> Element FrontendMsg
mainAppContent model =
    --Element.el [ Background.color (rgb255 140 94 88), width fill ]
    Element.column [ Background.color myColors.white, width fill, padding 10, alignTop, Border.rounded 10, spacing 5 ]
        [ Element.text "Main content"
        , row [ width fill, spacing 7 ]
            [ circleButton Big "+" [ Background.color myColors.accent, Font.color myColors.white ] <| Just Increment
            , text (String.fromInt model.counter)
            , circleButton Big "-" [ Background.color myColors.accent, Font.color myColors.white ] <| Just Decrement
            ]
        ]


type CircleButtonSize
    = Big
    | Small


circleButton : CircleButtonSize -> String -> List (Element.Attribute FrontendMsg) -> Maybe FrontendMsg -> Element FrontendMsg
circleButton size txt attrs msg =
    let
        shapeAttrs =
            case size of
                Big ->
                    [ width <| px 24, height <| px 24, Border.rounded 12 ]

                Small ->
                    [ width <| px 10, height <| px 10, Border.rounded 5 ]

        circleButtonAttrs =
            shapeAttrs
                ++ [ Font.center
                   ]
    in
    Input.button
        (circleButtonAttrs ++ attrs)
        { onPress = msg, label = text txt }


usersList : Model -> Element FrontendMsg
usersList model =
    Element.column [ Background.color myColors.primaryDark, width (px 260), padding 10, alignTop, Border.rounded 10, Font.color myColors.white ]
        (Element.text "Users list"
            :: List.map text (Set.toList model.connectedClients)
        )



-- Html.div [ style "padding" "30px" ]
--     [ Html.button [ onClick Increment ] [ text "+" ]
--     , Html.text (String.fromInt model.counter)
--     , Html.button [ onClick Decrement ] [ text "-" ]
--     , Html.div [] [ Html.text "Click me then refresh me!" ]
--     ]
