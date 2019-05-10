port module Main exposing (..)

import Html exposing (Html, div, button, text, li, section, ul, header, h1, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import String exposing (fromInt, toInt)
import Json.Decode as Json

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

emptyModel =
    { entries = []
    , currentInput = ""
    , currentUid = 0
    }

init : () -> ( Model, Cmd Msg )
init _ = 
  ( emptyModel, Cmd.none )
    
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  
type Msg 
    = Add
    | UpdateInput String
    | Check Int
    | RemoveEntry Int

type alias Model =
    { entries : List Entry
    , currentInput : String
    , currentUid : Int
    }
    
type alias Entry =
    { description : String
    , uid : Int
    , isCompleted : Bool
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add -> 
            let 
                newEntries = List.append model.entries [ { description = model.currentInput, uid = model.currentUid, isCompleted = False } ]
                newUId = model.currentUid + 1
            in
            
            ( { model | entries = newEntries, currentInput = "", currentUid = newUId }, Cmd.none )
            
        UpdateInput inputValue ->
            
            ( { model | currentInput = inputValue}, Cmd.none )
            
        Check uid -> 
            let
                checkEntry entry =
                    if entry.uid == uid then
                        { entry | isCompleted = not entry.isCompleted }
                    else
                        entry
                newEntries = List.map checkEntry model.entries
            in
                ( { model | entries = newEntries }, Cmd.none )
                
        RemoveEntry uid ->
            let 
                newEntries =
                    List.filter (\e -> not (e.uid == uid) ) model.entries
            in
                ( { model | entries = newEntries }, Cmd.none )
                
            
view : Model -> Html Msg            
view model = 
    div [ class "todoMVC-wrapper" ]
    [ section [ class "todoapp"]
        [ showHeader model
        , showEntries model
        ]
    ]
    
showHeader : Model -> Html Msg 
showHeader model =
    header [ class "header" ] 
    [ h1 [] [ text "Todos"]
    , input 
        [ class "new-todo"
        , placeholder "What needs to do done?"
        , onEnter Add
        , onInput UpdateInput
        , value model.currentInput
        ] 
        []
    ]

showEntries : Model -> Html Msg
showEntries model =
    section [ class "main" ] 
        [ ul [ class "todo-list"] ( List.map showEntry model.entries ) ]

showEntry : Entry -> Html Msg
showEntry entry =
    li [] 
        [ div [ class "view" ]
            [ input [class "toggle", type_ "checkbox", onClick (Check entry.uid) ] []
            , label [] [ text entry.description]
            , button [ class "destroy", onClick ( RemoveEntry entry.uid) ] [text "delete"]
            ]
        ]
    
onEnter msg =
     let
         isEnter key= 
             if key == 13 then
                 Json.succeed msg
             else
                 Json.fail "Not enter"
     in
         on "keydown" (Json.andThen isEnter keyCode)
                 