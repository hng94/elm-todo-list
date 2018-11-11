port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (..)
import Array exposing (..)
import Json.Encode as E
import Json.Decode as Decode

---- MODEL ----
port setStorage : Model -> Cmd msg
-- port toElm : (E.Value -> msg) -> Sub msg

type alias Entry =
    { 
        id : Int, 
        description : String,
        done: Bool
    }

newEntry : Int -> String -> Entry
newEntry id desc =
    {
        id = id,
        description = desc,
        done = False
    }

type alias Model =
    {
        todoList: List Entry,
        inputField: String,
        id: Int
    }
emptyModel : Model
emptyModel = 
    {
        todoList = [],
        inputField = "",
        id = 0
    }

init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    (Maybe.withDefault emptyModel maybeModel, Cmd.none )



---- UPDATE ----

type Msg
    = NoOp
    | Delete Int
    | AddTodo
    | UpdateInputField String
    | Check Int

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model = 
    let 
        (updatedModel, cmds) = update msg model
    in
        case msg of
            UpdateInputField str -> (updatedModel, cmds)
            _ -> (updatedModel, Cmd.batch [setStorage updatedModel, cmds])
        
            

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        UpdateInputField str ->
            (
                { model | inputField = str }
                , Cmd.none
            )
        AddTodo ->
            log model.inputField
            (
                {
                    model
                    | id = model.id + 1
                    , inputField = ""
                    , todoList = 
                        if String.isEmpty model.inputField then
                            model.todoList
                        else
                            model.todoList ++ [newEntry model.id model.inputField]
                }
                , Cmd.none
            )
        Delete id ->
            (
                { model | todoList = List.filter (\t -> t.id /= id) model.todoList }
                , Cmd.none
            )
        Check id ->
            let
                checkEntry t = 
                    if t.id == id then 
                        { t | done =  not t.done}  
                    else 
                        t   
            in
            (
                { model | todoList = List.map checkEntry model.todoList}
                , Cmd.none
            )


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ img [ class "logo",src "/todos.png" ] []
        , span [ class "header"] [ text "Hoang's todo list" ]
        , div [ class "input-group"]
              [ input [ value model.inputField, class "form-control", placeholder "Your new task", onInput UpdateInputField ] []
               ,div [ class "input-group-append", onClick AddTodo ]
                    [ button [ class "btn btn-primary" ] [ text "Add"]]
              ]
        , br [] []
        , ul [ class "list-group" ]
             <| List.map renderTodo model.todoList  
        ]

renderTodo : Entry -> Html Msg
renderTodo todo = 
    li  [ classList [("list-group-item", True), ("clickable", True), ("finished-todo", todo.done)], onClick(Check todo.id)]
        [ text todo.description
        , span [ class "text-danger float-right clickable", onClick (Delete todo.id) ]
            [ text "Delete"]
        ]

---- PROGRAM ----


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }
